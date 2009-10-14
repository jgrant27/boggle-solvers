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
//
// A Trie/Prefix Tree 
//
// Author : Justin Grant                                                                            
// Date : 03/26/2009
//

#ifndef TRIE_H
#define TRIE_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "global.h"
#include "utils.h"


#define NULL_KEY "*NULL*"

typedef struct {
  struct trie* children[30];
  struct trie* parent;
  char* text;
  int level;
  int depth;
  int count;
  int is_word;
  int word_count;
} trie;


extern trie* new_trie();
extern char curr_letter(trie* t, char* word);
extern void insert(trie* t, char* word);
extern int include(trie* t, char* word);
extern int begin(trie* t, char* word);
extern trie* get_child(trie* t, char l);
extern int load(trie* t, char* path);
extern trie* load_dict(char* load_path, int show_output);
extern void inc_word_count(trie* t);
extern void dec_word_count(trie* t);

#endif
