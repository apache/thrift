/************************ sha-private.h ************************/
/***************** See RFC 6234 for details. *******************/
#ifndef THRIFT_SHA_PRIVATE_H
#define THRIFT_SHA_PRIVATE_H

#define SHA_Ch(x,y,z)        (((x) & (y)) ^ ((~(x)) & (z)))
#define SHA_Maj(x,y,z)       (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define SHA_Parity(x, y, z)  ((x) ^ (y) ^ (z))

#endif
