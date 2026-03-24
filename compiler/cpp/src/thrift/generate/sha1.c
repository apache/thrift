/************************* sha1.c ***************************/
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

#define SHA1_ROTL(bits, word) \
    (((word) << (bits)) | ((word) >> (32 - (bits))))

static uint32_t addTemp;
#define SHA1AddLength(context, length) \
    (addTemp = (context)->Length_Low, \
     (context)->Corrupted = \
        (((context)->Length_Low += (length)) < addTemp) && \
        (++(context)->Length_High == 0) ? shaInputTooLong \
                                        : (context)->Corrupted)

static void SHA1ProcessMessageBlock(SHA1Context*);
static void SHA1Finalize(SHA1Context*, uint8_t);
static void SHA1PadMessage(SHA1Context*, uint8_t);

int SHA1Reset(SHA1Context* context) {
  if (!context) return shaNull;

  context->Length_High = context->Length_Low = 0;
  context->Message_Block_Index = 0;

  context->Intermediate_Hash[0] = 0x67452301;
  context->Intermediate_Hash[1] = 0xEFCDAB89;
  context->Intermediate_Hash[2] = 0x98BADCFE;
  context->Intermediate_Hash[3] = 0x10325476;
  context->Intermediate_Hash[4] = 0xC3D2E1F0;

  context->Computed = 0;
  context->Corrupted = shaSuccess;

  return shaSuccess;
}

int SHA1Input(SHA1Context* context, const uint8_t* message_array, unsigned length) {
  if (!context) return shaNull;
  if (!length) return shaSuccess;
  if (!message_array) return shaNull;
  if (context->Computed) return context->Corrupted = shaStateError;
  if (context->Corrupted) return context->Corrupted;

  while (length--) {
    context->Message_Block[context->Message_Block_Index++] = *message_array;

    if ((SHA1AddLength(context, 8) == shaSuccess) &&
        (context->Message_Block_Index == SHA1_Message_Block_Size))
      SHA1ProcessMessageBlock(context);

    message_array++;
  }

  return context->Corrupted;
}

int SHA1FinalBits(SHA1Context* context, uint8_t message_bits, unsigned int length) {
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

  SHA1AddLength(context, length);
  SHA1Finalize(context, (uint8_t)((message_bits & masks[length]) | markbit[length]));

  return context->Corrupted;
}

int SHA1Result(SHA1Context* context, uint8_t Message_Digest[SHA1HashSize]) {
  int i;

  if (!context) return shaNull;
  if (!Message_Digest) return shaNull;
  if (context->Corrupted) return context->Corrupted;

  if (!context->Computed)
    SHA1Finalize(context, 0x80);

  for (i = 0; i < SHA1HashSize; ++i)
    Message_Digest[i] = (uint8_t)(context->Intermediate_Hash[i >> 2]
                                   >> (8 * (3 - (i & 0x03))));

  return shaSuccess;
}

static void SHA1ProcessMessageBlock(SHA1Context* context) {
  const uint32_t K[4] = {
    0x5A827999, 0x6ED9EBA1, 0x8F1BBCDC, 0xCA62C1D6
  };

  int t;
  uint32_t temp;
  uint32_t W[80];
  uint32_t A, B, C, D, E;

  for (t = 0; t < 16; t++) {
    W[t]  = ((uint32_t)context->Message_Block[t * 4]) << 24;
    W[t] |= ((uint32_t)context->Message_Block[t * 4 + 1]) << 16;
    W[t] |= ((uint32_t)context->Message_Block[t * 4 + 2]) << 8;
    W[t] |= ((uint32_t)context->Message_Block[t * 4 + 3]);
  }

  for (t = 16; t < 80; t++)
    W[t] = SHA1_ROTL(1, W[t-3] ^ W[t-8] ^ W[t-14] ^ W[t-16]);

  A = context->Intermediate_Hash[0];
  B = context->Intermediate_Hash[1];
  C = context->Intermediate_Hash[2];
  D = context->Intermediate_Hash[3];
  E = context->Intermediate_Hash[4];

  for (t = 0; t < 20; t++) {
    temp = SHA1_ROTL(5,A) + SHA_Ch(B, C, D) + E + W[t] + K[0];
    E = D; D = C; C = SHA1_ROTL(30,B); B = A; A = temp;
  }

  for (t = 20; t < 40; t++) {
    temp = SHA1_ROTL(5,A) + SHA_Parity(B, C, D) + E + W[t] + K[1];
    E = D; D = C; C = SHA1_ROTL(30,B); B = A; A = temp;
  }

  for (t = 40; t < 60; t++) {
    temp = SHA1_ROTL(5,A) + SHA_Maj(B, C, D) + E + W[t] + K[2];
    E = D; D = C; C = SHA1_ROTL(30,B); B = A; A = temp;
  }

  for (t = 60; t < 80; t++) {
    temp = SHA1_ROTL(5,A) + SHA_Parity(B, C, D) + E + W[t] + K[3];
    E = D; D = C; C = SHA1_ROTL(30,B); B = A; A = temp;
  }

  context->Intermediate_Hash[0] += A;
  context->Intermediate_Hash[1] += B;
  context->Intermediate_Hash[2] += C;
  context->Intermediate_Hash[3] += D;
  context->Intermediate_Hash[4] += E;
  context->Message_Block_Index = 0;
}

static void SHA1Finalize(SHA1Context* context, uint8_t Pad_Byte) {
  int i;
  SHA1PadMessage(context, Pad_Byte);
  for (i = 0; i < SHA1_Message_Block_Size; ++i)
    context->Message_Block[i] = 0;
  context->Length_High = 0;
  context->Length_Low = 0;
  context->Computed = 1;
}

static void SHA1PadMessage(SHA1Context* context, uint8_t Pad_Byte) {
  if (context->Message_Block_Index >= (SHA1_Message_Block_Size - 8)) {
    context->Message_Block[context->Message_Block_Index++] = Pad_Byte;
    while (context->Message_Block_Index < SHA1_Message_Block_Size)
      context->Message_Block[context->Message_Block_Index++] = 0;
    SHA1ProcessMessageBlock(context);
  } else
    context->Message_Block[context->Message_Block_Index++] = Pad_Byte;

  while (context->Message_Block_Index < (SHA1_Message_Block_Size - 8))
    context->Message_Block[context->Message_Block_Index++] = 0;

  context->Message_Block[56] = (uint8_t)(context->Length_High >> 24);
  context->Message_Block[57] = (uint8_t)(context->Length_High >> 16);
  context->Message_Block[58] = (uint8_t)(context->Length_High >> 8);
  context->Message_Block[59] = (uint8_t)(context->Length_High);
  context->Message_Block[60] = (uint8_t)(context->Length_Low >> 24);
  context->Message_Block[61] = (uint8_t)(context->Length_Low >> 16);
  context->Message_Block[62] = (uint8_t)(context->Length_Low >> 8);
  context->Message_Block[63] = (uint8_t)(context->Length_Low);

  SHA1ProcessMessageBlock(context);
}
