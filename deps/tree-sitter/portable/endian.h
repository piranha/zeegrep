#ifndef PORTABLE_ENDIAN_H
#define PORTABLE_ENDIAN_H

#if defined(__APPLE__)
#include <libkern/OSByteOrder.h>
#define le16toh(x) OSSwapLittleToHostInt16(x)
#define be16toh(x) OSSwapBigToHostInt16(x)
#elif defined(__linux__) || defined(__CYGWIN__)
#include <endian.h>
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)
#include <sys/endian.h>
#elif defined(_WIN32)
#include <stdlib.h>
#if BYTE_ORDER == LITTLE_ENDIAN
#define le16toh(x) (x)
#define be16toh(x) _byteswap_ushort(x)
#else
#define le16toh(x) _byteswap_ushort(x)
#define be16toh(x) (x)
#endif
#else
// Fallback: assume little-endian
#define le16toh(x) (x)
#define be16toh(x) ((uint16_t)((((x) & 0xFF) << 8) | (((x) >> 8) & 0xFF)))
#endif

#endif // PORTABLE_ENDIAN_H
