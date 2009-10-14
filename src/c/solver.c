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

#include "solver.h"


void solve_board(trie* t, board* brd) {

  int rind; int cind;

  for (rind = 0; rind < brd->rows; rind++) {
    for (cind = 0; cind < brd->cols; cind++) {
      char word[MAX_WORD_LENGTH] = {};
      find_words(t, word, 0, (letter*)brd->letters[rind][cind]);
    }
  }
  
}


void find_words(trie* t, char* word, int fi, letter* ltr) {

  ltr->used = 1;

  // Append the next letter to the word
  char cc = word[fi] = ltr->value;
  word[++fi] = 0;

  // handle Q's
  if (cc == 'Q') {
    word[fi] = 'U'; 
    word[++fi] = 0;
    //t = t->children['Q' - 'A'];
    t = get_child(t, 'Q');
    cc = 'U';
  }

  if (fi >= MIN_WORD_LENGTH && include_rm(t, word))
    printf("%s\n", word);

  t = (trie*)t->children[cc - 'A'];
  //t = get_child(t, cc);

  if (t != NULL && fi <= MAX_WORD_LENGTH) {
    int i;
    letter** neighbors = (letter**)ltr->neighbors;
    for (i = 0; i < ltr->ncount; i++) {
      letter* n = (letter*)neighbors[i];
      if(n != NULL && !n->used) {
        find_words(t, word, fi, n);
      }
    }
  }

  // release the letter
  ltr->used = 0;

}


