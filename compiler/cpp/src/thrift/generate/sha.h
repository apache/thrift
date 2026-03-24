/**************************** sha.h ****************************/
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
#ifndef THRIFT_SHA_H
#define THRIFT_SHA_H

#include <stdint.h>

#ifndef _SHA_enum_
#define _SHA_enum_
enum {
    shaSuccess = 0,
    shaNull,
    shaInputTooLong,
    shaStateError,
    shaBadParam
};
#endif

enum {
    SHA1_Message_Block_Size = 64,
    SHA1HashSize = 20,
    SHA1HashSizeBits = 160
};

typedef struct SHA1Context {
    uint32_t Intermediate_Hash[SHA1HashSize/4];
    uint32_t Length_High;
    uint32_t Length_Low;
    int_least16_t Message_Block_Index;
    uint8_t Message_Block[SHA1_Message_Block_Size];
    int Computed;
    int Corrupted;
} SHA1Context;

extern int SHA1Reset(SHA1Context*);
extern int SHA1Input(SHA1Context*, const uint8_t*, unsigned int);
extern int SHA1FinalBits(SHA1Context*, uint8_t, unsigned int);
extern int SHA1Result(SHA1Context*, uint8_t[SHA1HashSize]);

#endif
