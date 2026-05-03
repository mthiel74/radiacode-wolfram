/* radiacode_link.c
 *
 * Wolfram LibraryLink shim for the RadiaCode USB bulk protocol.
 *
 * The C side stays deliberately small: enumerate devices, open / close
 * a handle, drain stale data, and round-trip a single request frame on
 * the bulk OUT endpoint while returning whatever comes back on the bulk
 * IN endpoint as a flat byte array.  Higher-level protocol decoding
 * (request framing, sequence numbers, spectrum / realtime decode, ...)
 * lives on the Wolfram side in DeviceNative.wl, where it is much easier
 * to inspect, edit, and unit-test.
 *
 * Note on the transport: RadiaCode is a vendor-class USB device with
 * bulk endpoints 0x01 (OUT) and 0x81 (IN) — NOT a HID device — so the
 * task brief's mention of hidapi is moot.  We use libusb-1.0, which is
 * exactly what the upstream Python radiacode lib uses under the hood.
 *
 * Build: see clib/build.wls.
 */

#include "WolframLibrary.h"
#include "WolframNumericArrayLibrary.h"

#include <libusb-1.0/libusb.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define RC_VID 0x0483
#define RC_PID 0xF123

#define RC_EP_OUT 0x01
#define RC_EP_IN  0x81

#define RC_MAX_HANDLES 8
#define RC_PACKET 256
#define RC_TIMEOUT_MS 3000
#define RC_DRAIN_TIMEOUT_MS 100

static libusb_context *g_ctx = NULL;

typedef struct {
    libusb_device_handle *dev;
    int interface_claimed;
    char serial[128];
} rc_slot;

static rc_slot g_slots[RC_MAX_HANDLES];

/* libraryLink lifecycle ---------------------------------------------- */

DLLEXPORT mint WolframLibrary_getVersion(void) {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    (void)libData;
    if (g_ctx == NULL) {
        if (libusb_init(&g_ctx) != 0) return LIBRARY_FUNCTION_ERROR;
    }
    memset(g_slots, 0, sizeof(g_slots));
    return LIBRARY_NO_ERROR;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
    (void)libData;
    for (int i = 0; i < RC_MAX_HANDLES; i++) {
        if (g_slots[i].dev) {
            if (g_slots[i].interface_claimed)
                libusb_release_interface(g_slots[i].dev, 0);
            libusb_close(g_slots[i].dev);
            g_slots[i].dev = NULL;
            g_slots[i].interface_claimed = 0;
        }
    }
    if (g_ctx) {
        libusb_exit(g_ctx);
        g_ctx = NULL;
    }
}

/* helpers ------------------------------------------------------------ */

static int set_string(MArgument out, const char *s, WolframLibraryData libData) {
    char *buf = (char *)libData->UTF8String_disown;
    (void)buf;
    /* The convention: caller allocates with malloc; Wolfram frees via
     * UTF8String_disown.  We just malloc + strcpy. */
    size_t n = strlen(s);
    char *p = (char *)malloc(n + 1);
    if (!p) return LIBRARY_FUNCTION_ERROR;
    memcpy(p, s, n + 1);
    MArgument_setUTF8String(out, p);
    return LIBRARY_NO_ERROR;
}

static int slot_alloc(void) {
    for (int i = 0; i < RC_MAX_HANDLES; i++)
        if (g_slots[i].dev == NULL) return i;
    return -1;
}

static int slot_valid(mint h) {
    return (h >= 0 && h < RC_MAX_HANDLES && g_slots[(int)h].dev != NULL);
}

/* drain any pending input from a previous orphaned read */
static void drain(libusb_device_handle *dev) {
    uint8_t buf[RC_PACKET];
    int actual = 0;
    int trials = 0;
    while (trials < 8) {
        int rc = libusb_bulk_transfer(dev, RC_EP_IN, buf, sizeof(buf),
                                       &actual, RC_DRAIN_TIMEOUT_MS);
        if (rc == LIBUSB_ERROR_TIMEOUT) break;
        if (rc != 0) break;
        if (actual == 0) break;
        trials++;
    }
}

/* exported functions ------------------------------------------------- */

/* RC_Enumerate: ()  -> List of UTF8 strings (serial numbers)
 *
 * LibraryLink doesn't expose List-of-String returns through the simple
 * MArgument calling convention, so we return a single newline-separated
 * UTF8 string and split on the WL side.  Empty string == no devices.
 */
DLLEXPORT int RC_Enumerate(WolframLibraryData libData, mint argc,
                            MArgument *args, MArgument res) {
    (void)libData; (void)argc; (void)args;

    if (!g_ctx) {
        if (libusb_init(&g_ctx) != 0) return LIBRARY_FUNCTION_ERROR;
    }

    libusb_device **list = NULL;
    ssize_t n = libusb_get_device_list(g_ctx, &list);
    if (n < 0) return LIBRARY_FUNCTION_ERROR;

    /* build newline-separated string of serials */
    size_t cap = 256, len = 0;
    char *buf = (char *)malloc(cap);
    if (!buf) { libusb_free_device_list(list, 1); return LIBRARY_FUNCTION_ERROR; }
    buf[0] = '\0';

    for (ssize_t i = 0; i < n; i++) {
        struct libusb_device_descriptor d;
        if (libusb_get_device_descriptor(list[i], &d) != 0) continue;
        if (d.idVendor != RC_VID || d.idProduct != RC_PID) continue;

        libusb_device_handle *h = NULL;
        if (libusb_open(list[i], &h) != 0) continue;

        unsigned char sbuf[128] = {0};
        int slen = libusb_get_string_descriptor_ascii(h, d.iSerialNumber,
                                                       sbuf, sizeof(sbuf) - 1);
        libusb_close(h);
        if (slen <= 0) continue;
        sbuf[slen] = '\0';

        size_t need = len + slen + 2;
        if (need > cap) {
            while (need > cap) cap *= 2;
            char *nb = (char *)realloc(buf, cap);
            if (!nb) { free(buf); libusb_free_device_list(list, 1);
                       return LIBRARY_FUNCTION_ERROR; }
            buf = nb;
        }
        if (len > 0) buf[len++] = '\n';
        memcpy(buf + len, sbuf, slen);
        len += slen;
        buf[len] = '\0';
    }

    libusb_free_device_list(list, 1);
    MArgument_setUTF8String(res, buf);
    return LIBRARY_NO_ERROR;
}

/* RC_Open: (UTF8 serial)  -> Integer handle (>=0) or -1 on failure */
DLLEXPORT int RC_Open(WolframLibraryData libData, mint argc,
                       MArgument *args, MArgument res) {
    (void)libData;
    if (argc < 1) { MArgument_setInteger(res, -1); return LIBRARY_NO_ERROR; }

    if (!g_ctx) {
        if (libusb_init(&g_ctx) != 0) {
            MArgument_setInteger(res, -1); return LIBRARY_NO_ERROR;
        }
    }
    char *serial = MArgument_getUTF8String(args[0]);
    int slot = slot_alloc();
    if (slot < 0) { MArgument_setInteger(res, -1); return LIBRARY_NO_ERROR; }

    libusb_device **list = NULL;
    ssize_t n = libusb_get_device_list(g_ctx, &list);
    if (n < 0) { MArgument_setInteger(res, -1); return LIBRARY_NO_ERROR; }

    libusb_device_handle *opened = NULL;
    char found_serial[128] = {0};

    for (ssize_t i = 0; i < n && !opened; i++) {
        struct libusb_device_descriptor d;
        if (libusb_get_device_descriptor(list[i], &d) != 0) continue;
        if (d.idVendor != RC_VID || d.idProduct != RC_PID) continue;

        libusb_device_handle *h = NULL;
        if (libusb_open(list[i], &h) != 0) continue;
        unsigned char sbuf[128] = {0};
        int slen = libusb_get_string_descriptor_ascii(h, d.iSerialNumber,
                                                       sbuf, sizeof(sbuf) - 1);
        if (slen <= 0) { libusb_close(h); continue; }
        sbuf[slen] = '\0';

        int match = (serial == NULL) || (serial[0] == '\0') ||
                    (strcmp((const char *)sbuf, serial) == 0);
        if (match) {
            opened = h;
            memcpy(found_serial, sbuf, slen + 1);
        } else {
            libusb_close(h);
        }
    }
    libusb_free_device_list(list, 1);
    libData->UTF8String_disown(serial);

    if (!opened) { MArgument_setInteger(res, -1); return LIBRARY_NO_ERROR; }

    libusb_set_auto_detach_kernel_driver(opened, 1);
    int rc = libusb_claim_interface(opened, 0);
    if (rc != 0) {
        libusb_close(opened);
        MArgument_setInteger(res, -1);
        return LIBRARY_NO_ERROR;
    }

    g_slots[slot].dev = opened;
    g_slots[slot].interface_claimed = 1;
    strncpy(g_slots[slot].serial, found_serial, sizeof(g_slots[slot].serial) - 1);

    drain(opened);

    MArgument_setInteger(res, slot);
    return LIBRARY_NO_ERROR;
}

/* RC_Close: (Integer handle)  -> Integer 0 ok / -1 fail */
DLLEXPORT int RC_Close(WolframLibraryData libData, mint argc,
                        MArgument *args, MArgument res) {
    (void)libData;
    if (argc < 1) { MArgument_setInteger(res, -1); return LIBRARY_NO_ERROR; }
    mint h = MArgument_getInteger(args[0]);
    if (!slot_valid(h)) { MArgument_setInteger(res, -1); return LIBRARY_NO_ERROR; }
    int idx = (int)h;
    if (g_slots[idx].interface_claimed)
        libusb_release_interface(g_slots[idx].dev, 0);
    libusb_close(g_slots[idx].dev);
    g_slots[idx].dev = NULL;
    g_slots[idx].interface_claimed = 0;
    g_slots[idx].serial[0] = '\0';
    MArgument_setInteger(res, 0);
    return LIBRARY_NO_ERROR;
}

/* RC_Serial: (handle) -> UTF8 string */
DLLEXPORT int RC_Serial(WolframLibraryData libData, mint argc,
                         MArgument *args, MArgument res) {
    (void)libData;
    if (argc < 1) return set_string(res, "", libData);
    mint h = MArgument_getInteger(args[0]);
    if (!slot_valid(h)) return set_string(res, "", libData);
    return set_string(res, g_slots[(int)h].serial, libData);
}

/* RC_Execute: (handle, request : UInt8 NumericArray)
 *   -> response UInt8 NumericArray (empty on transport error)
 *
 * The 'request' bytes are exactly what gets written on EP OUT, including
 * the 4-byte little-endian length prefix that the protocol expects
 * (assembled on the WL side).  Response framing: first 4 bytes are the
 * declared payload length, followed by that many bytes of payload.  The
 * device may split the payload across multiple bulk reads (256 byte
 * packets on this firmware).  We strip the 4-byte length and return the
 * payload.
 */
DLLEXPORT int RC_Execute(WolframLibraryData libData, mint argc,
                          MArgument *args, MArgument res) {
    if (argc < 2) return LIBRARY_FUNCTION_ERROR;

    mint h = MArgument_getInteger(args[0]);
    if (!slot_valid(h)) return LIBRARY_FUNCTION_ERROR;
    libusb_device_handle *dev = g_slots[(int)h].dev;

    MNumericArray req = MArgument_getMNumericArray(args[1]);
    WolframNumericArrayLibrary_Functions naFns = libData->numericarrayLibraryFunctions;

    if (naFns->MNumericArray_getType(req) != MNumericArray_Type_UBit8)
        return LIBRARY_FUNCTION_ERROR;
    if (naFns->MNumericArray_getRank(req) != 1)
        return LIBRARY_FUNCTION_ERROR;

    mint reqlen = naFns->MNumericArray_getFlattenedLength(req);
    uint8_t *reqbuf = (uint8_t *)naFns->MNumericArray_getData(req);

    int actual = 0;
    int rc = libusb_bulk_transfer(dev, RC_EP_OUT, reqbuf, (int)reqlen,
                                  &actual, RC_TIMEOUT_MS);
    if (rc != 0 || actual != (int)reqlen) {
        /* return empty array */
        mint dims0[1] = {0};
        MNumericArray empty = NULL;
        naFns->MNumericArray_new(MNumericArray_Type_UBit8, 1, dims0, &empty);
        MArgument_setMNumericArray(res, empty);
        return LIBRARY_NO_ERROR;
    }

    /* read first packet */
    uint8_t pkt[RC_PACKET];
    int trials = 0, max_trials = 3;
    int got = 0;
    while (trials < max_trials) {
        rc = libusb_bulk_transfer(dev, RC_EP_IN, pkt, sizeof(pkt),
                                  &got, RC_TIMEOUT_MS);
        if (rc == 0 && got > 0) break;
        if (rc != 0 && rc != LIBUSB_ERROR_TIMEOUT) {
            mint dims0[1] = {0};
            MNumericArray empty = NULL;
            naFns->MNumericArray_new(MNumericArray_Type_UBit8, 1, dims0, &empty);
            MArgument_setMNumericArray(res, empty);
            return LIBRARY_NO_ERROR;
        }
        trials++;
    }
    if (got < 4) {
        mint dims0[1] = {0};
        MNumericArray empty = NULL;
        naFns->MNumericArray_new(MNumericArray_Type_UBit8, 1, dims0, &empty);
        MArgument_setMNumericArray(res, empty);
        return LIBRARY_NO_ERROR;
    }

    uint32_t resp_len = (uint32_t)pkt[0]
                       | ((uint32_t)pkt[1] << 8)
                       | ((uint32_t)pkt[2] << 16)
                       | ((uint32_t)pkt[3] << 24);

    /* Allocate output buffer = resp_len */
    mint dims[1] = { (mint)resp_len };
    MNumericArray out = NULL;
    if (naFns->MNumericArray_new(MNumericArray_Type_UBit8, 1, dims, &out) != 0)
        return LIBRARY_FUNCTION_ERROR;
    uint8_t *outbuf = (uint8_t *)naFns->MNumericArray_getData(out);

    int copied = 0;
    int avail = got - 4;
    if (avail > 0) {
        int take = (avail < (int)resp_len) ? avail : (int)resp_len;
        memcpy(outbuf, pkt + 4, take);
        copied = take;
    }
    while (copied < (int)resp_len) {
        int want = (int)resp_len - copied;
        if (want > (int)sizeof(pkt)) want = (int)sizeof(pkt);
        rc = libusb_bulk_transfer(dev, RC_EP_IN, pkt, want, &got, RC_TIMEOUT_MS);
        if (rc != 0 || got <= 0) break;
        memcpy(outbuf + copied, pkt, got);
        copied += got;
    }
    if (copied != (int)resp_len) {
        /* short read; truncate by re-allocating */
        naFns->MNumericArray_free(out);
        mint dims2[1] = { copied };
        if (naFns->MNumericArray_new(MNumericArray_Type_UBit8, 1, dims2, &out) != 0)
            return LIBRARY_FUNCTION_ERROR;
        outbuf = (uint8_t *)naFns->MNumericArray_getData(out);
        /* note: we can't recover the dropped bytes, this is an error path */
        (void)outbuf;
    }

    MArgument_setMNumericArray(res, out);
    return LIBRARY_NO_ERROR;
}
