/************************* sha256.c ***************************/
/***************** See RFC 6234 for details. *******************/
/*
   Copyright (c) 2011 IETF Trust and the persons identified as
   authors of the code.  All rights reserved.

   Redistribution and use in source and binary forms, with or
   without modification, are permitted provided that the following
   conditions are met:

   - Redistributions of source code must retain the above
     copyright notice, this list of conditions and
     the following disclaimer.

   - Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     disclaimer in the documentation and/or other materials provided
     with the distribution.

   - Neither the name of Internet Society, IETF or IETF Trust, nor
     the names of specific contributors, may be used to endorse or
     promote products derived from this software without specific
     prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "sha.h"
#include "sha-private.h"

#define SHA256_ROTL(bits, word) \
    (((word) << (bits)) | ((word) >> (32 - (bits))))

#define SHA256_SHR(bits, word) \
    ((word) >> (bits))

static uint32_t addTemp256;
#define SHA256AddLength(context, length) \
    (addTemp256 = (context)->Length_Low, \
     (context)->Corrupted = \
         (((context)->Length_Low += (length)) < addTemp256) && \
         (++(context)->Length_High == 0) ? shaInputTooLong \
                                         : (context)->Corrupted)

static void SHA256ProcessMessageBlock(SHA256Context*);
static void SHA256Finalize(SHA256Context*, uint8_t);
static void SHA256PadMessage(SHA256Context*, uint8_t);

int SHA256Reset(SHA256Context* context) {
  if (!context) return shaNull;

  context->Length_High = context->Length_Low = 0;
  context->Message_Block_Index = 0;

  context->Intermediate_Hash[0] = 0x6a09e667;
  context->Intermediate_Hash[1] = 0xbb67ae85;
  context->Intermediate_Hash[2] = 0x3c6ef372;
  context->Intermediate_Hash[3] = 0xa54ff53a;
  context->Intermediate_Hash[4] = 0x510e527f;
  context->Intermediate_Hash[5] = 0x9b05688c;
  context->Intermediate_Hash[6] = 0x1f83d9ab;
  context->Intermediate_Hash[7] = 0x5be0cd19;

  context->Computed = 0;
  context->Corrupted = shaSuccess;

  return shaSuccess;
}

int SHA256Input(SHA256Context* context, const uint8_t* message_array, unsigned length) {
  if (!context) return shaNull;
  if (!length) return shaSuccess;
  if (!message_array) return shaNull;
  if (context->Computed) return context->Corrupted = shaStateError;
  if (context->Corrupted) return context->Corrupted;

  while (length--) {
    context->Message_Block[context->Message_Block_Index++] = *message_array;

    if ((SHA256AddLength(context, 8) == shaSuccess) &&
        (context->Message_Block_Index == SHA256_Message_Block_Size))
      SHA256ProcessMessageBlock(context);

    message_array++;
  }

  return context->Corrupted;
}

int SHA256FinalBits(SHA256Context* context, uint8_t message_bits, unsigned int length) {
  static uint8_t masks[8] = {
    0x00, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE
  };
  static uint8_t markbit[8] = {
    0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01
  };

  if (!context) return shaNull;
  if (!length) return shaSuccess;
  if (context->Corrupted) return context->Corrupted;
  if (context->Computed) return context->Corrupted = shaStateError;
  if (length >= 8) return context->Corrupted = shaBadParam;

  SHA256AddLength(context, length);
  SHA256Finalize(context, (uint8_t)((message_bits & masks[length]) | markbit[length]));

  return context->Corrupted;
}

int SHA256Result(SHA256Context* context, uint8_t Message_Digest[SHA256HashSize]) {
  int i;

  if (!context) return shaNull;
  if (!Message_Digest) return shaNull;
  if (context->Corrupted) return context->Corrupted;

  if (!context->Computed)
    SHA256Finalize(context, 0x80);

  for (i = 0; i < SHA256HashSize; ++i)
    Message_Digest[i] = (uint8_t)(context->Intermediate_Hash[i >> 2]
                                   >> (8 * (3 - (i & 0x03))));

  return shaSuccess;
}

static void SHA256ProcessMessageBlock(SHA256Context* context) {
  const uint32_t K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  };

  int t;
  uint32_t temp1, temp2;
  uint32_t W[64];
  uint32_t A, B, C, D, E, F, G, H;

  for (t = 0; t < 16; t++) {
    W[t]  = ((uint32_t)context->Message_Block[t * 4]) << 24;
    W[t] |= ((uint32_t)context->Message_Block[t * 4 + 1]) << 16;
    W[t] |= ((uint32_t)context->Message_Block[t * 4 + 2]) << 8;
    W[t] |= ((uint32_t)context->Message_Block[t * 4 + 3]);
  }

  for (t = 16; t < 64; t++)
    W[t] = SHA256_SHR(2, W[t-2]) + W[t-7] + SHA256_SHR(7, W[t-15]) + W[t-16];

  A = context->Intermediate_Hash[0];
  B = context->Intermediate_Hash[1];
  C = context->Intermediate_Hash[2];
  D = context->Intermediate_Hash[3];
  E = context->Intermediate_Hash[4];
  F = context->Intermediate_Hash[5];
  G = context->Intermediate_Hash[6];
  H = context->Intermediate_Hash[7];

  for (t = 0; t < 64; t++) {
    temp1 = H + SHA256_ROTL(5, E) + SHA_Ch(E, F, G) + K[t] + W[t];
    temp2 = SHA256_ROTL(30, A) + SHA_Maj(A, B, C);
    H = G; G = F; F = E; E = D + temp1;
    D = C; C = B; B = A; A = temp1 + temp2;
  }

  context->Intermediate_Hash[0] += A;
  context->Intermediate_Hash[1] += B;
  context->Intermediate_Hash[2] += C;
  context->Intermediate_Hash[3] += D;
  context->Intermediate_Hash[4] += E;
  context->Intermediate_Hash[5] += F;
  context->Intermediate_Hash[6] += G;
  context->Intermediate_Hash[7] += H;
  context->Message_Block_Index = 0;
}

static void SHA256Finalize(SHA256Context* context, uint8_t Pad_Byte) {
  int i;
  SHA256PadMessage(context, Pad_Byte);
  for (i = 0; i < SHA256_Message_Block_Size; ++i)
    context->Message_Block[i] = 0;
  context->Length_High = 0;
  context->Length_Low = 0;
  context->Computed = 1;
}

static void SHA256PadMessage(SHA256Context* context, uint8_t Pad_Byte) {
  if (context->Message_Block_Index >= (SHA256_Message_Block_Size - 8)) {
    context->Message_Block[context->Message_Block_Index++] = Pad_Byte;
    while (context->Message_Block_Index < SHA256_Message_Block_Size)
      context->Message_Block[context->Message_Block_Index++] = 0;
    SHA256ProcessMessageBlock(context);
  } else
    context->Message_Block[context->Message_Block_Index++] = Pad_Byte;

  while (context->Message_Block_Index < (SHA256_Message_Block_Size - 8))
    context->Message_Block[context->Message_Block_Index++] = 0;

  context->Message_Block[56] = (uint8_t)(context->Length_High >> 24);
  context->Message_Block[57] = (uint8_t)(context->Length_High >> 16);
  context->Message_Block[58] = (uint8_t)(context->Length_High >> 8);
  context->Message_Block[59] = (uint8_t)(context->Length_High);
  context->Message_Block[60] = (uint8_t)(context->Length_Low >> 24);
  context->Message_Block[61] = (uint8_t)(context->Length_Low >> 16);
  context->Message_Block[62] = (uint8_t)(context->Length_Low >> 8);
  context->Message_Block[63] = (uint8_t)(context->Length_Low);

  SHA256ProcessMessageBlock(context);
}