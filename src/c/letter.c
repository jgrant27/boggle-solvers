// Copyright (c) 2009, Justin Grant <justin at imagine27 dot com>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of the <organization> nor the
//       names of its contributors may be used to endorse or promote products
//       derived from this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY <copyright holder> ''AS IS'' AND ANY
// EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//
//
//

#include "letter.h"
#include "stdio.h"


letter* new_letter() {

  letter* l = (letter*)malloc(sizeof(letter));

  l->neighbors = malloc(sizeof(letter) * MAX_NEIGHBORS);
  l->used = 0;
  l->ncount = 0;
  l->uncount = 0;
  l->value = '\0';
 
  return l;
}


void set_neighbors(letter* ltr, letter** nbrs) {

  ltr->ncount = MAX_NEIGHBORS;
  int i = 0;
  
  for (; i < MAX_NEIGHBORS; i++) {
    if (nbrs[i] == NULL)
      ltr->ncount--;
  }

  ltr->uncount = ltr->ncount;

  ltr->neighbors = (struct letter**)nbrs;

}


