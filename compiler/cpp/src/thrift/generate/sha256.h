/*********************************************************************
* Filename:   sha256.h / sha256.c (combined header-only)
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Defines the API for the corresponding SHA-256 implementation.
*             SHA-256 is one of the three algorithms in the SHA2
*             specification. The others, SHA-384 and SHA-512, are not
*             offered in this implementation.
*             Algorithm specification can be found here:
*              * http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf
*             This implementation uses little endian byte order.
*
* Source:     https://github.com/B-Con/crypto-algorithms
*             Public domain — no copyright claimed by the author.
*
* Modifications for Apache Thrift:
*   - Combined .h and .c into a single header-only file.
*   - Added C++ wrapper (thrift_generator::sha256) returning std::vector<uint8_t>.
*   - No algorithmic changes.
*********************************************************************/

#pragma once

#include <cstring>
#include <string>
#include <vector>
#include <cstdint>
#include <cstddef>

/****************************** MACROS ******************************/
#define THRIFT_SHA256_ROTLEFT(a,b)  (((a) << (b)) | ((a) >> (32-(b))))
#define THRIFT_SHA256_ROTRIGHT(a,b) (((a) >> (b)) | ((a) << (32-(b))))

#define THRIFT_SHA256_CH(x,y,z)  (((x) & (y)) ^ (~(x) & (z)))
#define THRIFT_SHA256_MAJ(x,y,z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define THRIFT_SHA256_EP0(x) (THRIFT_SHA256_ROTRIGHT(x,2)  ^ THRIFT_SHA256_ROTRIGHT(x,13) ^ THRIFT_SHA256_ROTRIGHT(x,22))
#define THRIFT_SHA256_EP1(x) (THRIFT_SHA256_ROTRIGHT(x,6)  ^ THRIFT_SHA256_ROTRIGHT(x,11) ^ THRIFT_SHA256_ROTRIGHT(x,25))
#define THRIFT_SHA256_SIG0(x) (THRIFT_SHA256_ROTRIGHT(x,7) ^ THRIFT_SHA256_ROTRIGHT(x,18) ^ ((x) >> 3))
#define THRIFT_SHA256_SIG1(x) (THRIFT_SHA256_ROTRIGHT(x,17) ^ THRIFT_SHA256_ROTRIGHT(x,19) ^ ((x) >> 10))

/**************************** DATA TYPES ****************************/
#define THRIFT_SHA256_BLOCK_SIZE 32

typedef struct {
  unsigned char data[64];
  unsigned int  datalen;
  unsigned long long bitlen;
  unsigned int  state[8];
} THRIFT_SHA256_CTX;

/**************************** VARIABLES *****************************/
// clang-format off
static const unsigned int thrift_sha256_k[64] = {
  0x428a2f98u,0x71374491u,0xb5c0fbcfu,0xe9b5dba5u,0x3956c25bu,0x59f111f1u,0x923f82a4u,0xab1c5ed5u,
  0xd807aa98u,0x12835b01u,0x243185beu,0x550c7dc3u,0x72be5d74u,0x80deb1feu,0x9bdc06a7u,0xc19bf174u,
  0xe49b69c1u,0xefbe4786u,0x0fc19dc6u,0x240ca1ccu,0x2de92c6fu,0x4a7484aau,0x5cb0a9dcu,0x76f988dau,
  0x983e5152u,0xa831c66du,0xb00327c8u,0xbf597fc7u,0xc6e00bf3u,0xd5a79147u,0x06ca6351u,0x14292967u,
  0x27b70a85u,0x2e1b2138u,0x4d2c6dfcu,0x53380d13u,0x650a7354u,0x766a0abbu,0x81c2c92eu,0x92722c85u,
  0xa2bfe8a1u,0xa81a664bu,0xc24b8b70u,0xc76c51a3u,0xd192e819u,0xd6990624u,0xf40e3585u,0x106aa070u,
  0x19a4c116u,0x1e376c08u,0x2748774cu,0x34b0bcb5u,0x391c0cb3u,0x4ed8aa4au,0x5b9cca4fu,0x682e6ff3u,
  0x748f82eeu,0x78a5636fu,0x84c87814u,0x8cc70208u,0x90befffau,0xa4506cebu,0xbef9a3f7u,0xc67178f2u
};
// clang-format on

/*********************** FUNCTION DEFINITIONS ***********************/
inline void thrift_sha256_transform(THRIFT_SHA256_CTX *ctx, const unsigned char data[])
{
  unsigned int a, b, c, d, e, f, g, h, i, j, t1, t2, m[64];

  for (i = 0, j = 0; i < 16; ++i, j += 4)
    m[i] = ((unsigned int)data[j] << 24) | ((unsigned int)data[j+1] << 16)
          | ((unsigned int)data[j+2] << 8) | ((unsigned int)data[j+3]);
  for (; i < 64; ++i)
    m[i] = THRIFT_SHA256_SIG1(m[i-2]) + m[i-7] + THRIFT_SHA256_SIG0(m[i-15]) + m[i-16];

  a = ctx->state[0]; b = ctx->state[1]; c = ctx->state[2]; d = ctx->state[3];
  e = ctx->state[4]; f = ctx->state[5]; g = ctx->state[6]; h = ctx->state[7];

  for (i = 0; i < 64; ++i) {
    t1 = h + THRIFT_SHA256_EP1(e) + THRIFT_SHA256_CH(e,f,g) + thrift_sha256_k[i] + m[i];
    t2 = THRIFT_SHA256_EP0(a) + THRIFT_SHA256_MAJ(a,b,c);
    h = g; g = f; f = e; e = d + t1;
    d = c; c = b; b = a; a = t1 + t2;
  }

  ctx->state[0] += a; ctx->state[1] += b; ctx->state[2] += c; ctx->state[3] += d;
  ctx->state[4] += e; ctx->state[5] += f; ctx->state[6] += g; ctx->state[7] += h;
}

inline void thrift_sha256_init(THRIFT_SHA256_CTX *ctx)
{
  ctx->datalen  = 0;
  ctx->bitlen   = 0;
  ctx->state[0] = 0x6a09e667u;
  ctx->state[1] = 0xbb67ae85u;
  ctx->state[2] = 0x3c6ef372u;
  ctx->state[3] = 0xa54ff53au;
  ctx->state[4] = 0x510e527fu;
  ctx->state[5] = 0x9b05688cu;
  ctx->state[6] = 0x1f83d9abu;
  ctx->state[7] = 0x5be0cd19u;
}

inline void thrift_sha256_update(THRIFT_SHA256_CTX *ctx, const unsigned char data[], size_t len)
{
  for (size_t i = 0; i < len; ++i) {
    ctx->data[ctx->datalen] = data[i];
    ctx->datalen++;
    if (ctx->datalen == 64) {
      thrift_sha256_transform(ctx, ctx->data);
      ctx->bitlen += 512;
      ctx->datalen = 0;
    }
  }
}

inline void thrift_sha256_final(THRIFT_SHA256_CTX *ctx, unsigned char hash[])
{
  unsigned int i = ctx->datalen;

  if (ctx->datalen < 56) {
    ctx->data[i++] = 0x80u;
    while (i < 56) ctx->data[i++] = 0x00u;
  } else {
    ctx->data[i++] = 0x80u;
    while (i < 64) ctx->data[i++] = 0x00u;
    thrift_sha256_transform(ctx, ctx->data);
    memset(ctx->data, 0, 56);
  }

  ctx->bitlen += ctx->datalen * 8;
  ctx->data[63] = (unsigned char)(ctx->bitlen);
  ctx->data[62] = (unsigned char)(ctx->bitlen >> 8);
  ctx->data[61] = (unsigned char)(ctx->bitlen >> 16);
  ctx->data[60] = (unsigned char)(ctx->bitlen >> 24);
  ctx->data[59] = (unsigned char)(ctx->bitlen >> 32);
  ctx->data[58] = (unsigned char)(ctx->bitlen >> 40);
  ctx->data[57] = (unsigned char)(ctx->bitlen >> 48);
  ctx->data[56] = (unsigned char)(ctx->bitlen >> 56);
  thrift_sha256_transform(ctx, ctx->data);

  for (i = 0; i < 4; ++i) {
    hash[i]      = (ctx->state[0] >> (24 - i * 8)) & 0xffu;
    hash[i +  4] = (ctx->state[1] >> (24 - i * 8)) & 0xffu;
    hash[i +  8] = (ctx->state[2] >> (24 - i * 8)) & 0xffu;
    hash[i + 12] = (ctx->state[3] >> (24 - i * 8)) & 0xffu;
    hash[i + 16] = (ctx->state[4] >> (24 - i * 8)) & 0xffu;
    hash[i + 20] = (ctx->state[5] >> (24 - i * 8)) & 0xffu;
    hash[i + 24] = (ctx->state[6] >> (24 - i * 8)) & 0xffu;
    hash[i + 28] = (ctx->state[7] >> (24 - i * 8)) & 0xffu;
  }
}

/******************** C++ convenience wrapper ********************/
namespace thrift_generator {

/** Compute SHA-256 of arbitrary bytes. Returns a 32-byte digest. */
inline std::vector<uint8_t> sha256(const uint8_t* data, size_t len) {
  THRIFT_SHA256_CTX ctx;
  thrift_sha256_init(&ctx);
  thrift_sha256_update(&ctx, reinterpret_cast<const unsigned char*>(data), len);
  std::vector<uint8_t> digest(THRIFT_SHA256_BLOCK_SIZE);
  thrift_sha256_final(&ctx, digest.data());
  return digest;
}

/** Convenience overload: compute SHA-256 of a std::string. */
inline std::vector<uint8_t> sha256(const std::string& s) {
  return sha256(reinterpret_cast<const uint8_t*>(s.data()), s.size());
}

} // namespace thrift_generator
