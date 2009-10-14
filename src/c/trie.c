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

#include "trie.h"


trie* new_trie() {
  trie* t = (trie*)malloc(sizeof(trie));
  t = memset(t, 0, sizeof(trie));


  return t;
}

void insert(trie* t, char* word) {

  char l = word[t->level];

  if (l == 0) {
    t->is_word = 1;
    t->text = word;    
    inc_word_count(t);
  } else {
    int i = l - 'A';
    trie* tt = (trie*)t->children[i];
    if (tt == (trie*)NULL) {
      tt = new_trie();
      tt->level = t->level + 1;
      tt->parent = (struct trie*)t;
    }
    t->children[i] = (struct trie*)tt;
    insert(tt, word);
  }
}

int include(trie* t, char* word) {

  char l = word[t->level];

  if (l == 0) {
    char* val = (char*)t->text;
    if(val == NULL) return 0;
    return strcmp(val, word) == 0;
  } else {
    trie* tt = (trie*)t->children[l - 'A'];
    return tt != (trie*)NULL && include(tt, word);
  }
}

int include_rm(trie* t, char* word) {

  char l = word[t->level];

  if (l == 0) {
    char* val = (char*)t->text;
    if(val == NULL) return 0;
    if (!t->is_word) return 0;
    t->is_word = 0;
    dec_word_count(t);
    // This should free the memory associated with children
    //if (((trie*)t->parent)->word_count == 0) {
    //  free((trie*) ((trie*)t->parent)->children[word[t->level - 1] - 'A']);
    //}
    return 1;
  } else {
    trie* tt = (trie*)t->children[l - 'A'];
    return tt != (trie*)NULL && include_rm(tt, word);
  }
}

int begin(trie* t, char* word) {

  char l = word[t->level];

  if (l == (char)NULL || l == (char)0) {
    return ((trie*)t->children != NULL) ? 1 : 0;
  } else {
    trie* tt = (trie*)t->children[l - 'A'];
    return tt != (trie*)NULL && begin(tt, word);
  }
  
}


void inc_word_count(trie* t) {
  if (t->parent != NULL) {
    ((trie*)t->parent)->word_count++;
    inc_word_count(((trie*)t->parent));
  }
}

void dec_word_count(trie* t) {
  if (t->parent != NULL) {
    ((trie*)t->parent)->word_count--;
    dec_word_count(((trie*)t->parent));
  }
}

trie* get_child(trie* t, char l) {
  int i = l - 'A';
  trie* tt = (trie*)t->children[i];

  if (tt == (trie*)NULL) {
    tt = new_trie();
    tt->level = t->level + 1;
    t->children[i] = (struct trie*)tt;
    tt->parent = (struct trie*)t;
  }

  return tt;
}

int load(trie* t, char* path) {
  
  FILE* file = fopen ( path, "r" );

  if ( file != (FILE*)NULL ) {
    char line [LINE_MAX]; /* or other suitable maximum line size */
    int word_depth = 0;
    int count = 0;
    char* word;

    while ( fgets ( line, sizeof line, file ) != NULL ) /* read a line */ {
      word = (char*) strip(line);
      uppercase(word);

      // don't even insert invalid words in the trie
      if (word == NULL || strlen(word) < MIN_WORD_LENGTH) continue;
      //fputs(line, stdout);

      insert(t, word);
      if (strlen(word) > word_depth) word_depth = strlen(word);

      count++;
    }
    fclose(file);
    t->depth = word_depth;
    t->count = count;

    return 0;

  } else {

    perror (path); /* why didn't the file open? */
    return 1;

  }

}

trie* load_dict(char* load_path, int show_output) {

  if (load_path == (char*)NULL || strlen(load_path) < 1) {
    printf("\nNo dictionary available.\nSpecify a word dictionary e.g. boggle --dict $PATH_TO_DICT\n");
    exit(0);
  }
  
  // create a new instance
  trie* t = new_trie();
  if (show_output) {
    printf("\nloading dictionary from %s ",load_path); 
  }
  fflush(stdout);
  load(t, load_path);
  if (show_output) {
    printf("done.\n");
  } 
  fflush(stdout);
  if (show_output) {
    printf("%i words loaded. \n", t->word_count); 
  }
  fflush(stdout);
  //  if (show_output) {
  //  printf("nodes / word: %i  \n\n", t->node_count / t->word_count); 
  //}
  //fflush(stdout);
  if (show_output) {
    printf("trie depth: %i, level: %i \n\n", t->depth, t->level); 
  }
  fflush(stdout);
  
  return t;
  
}

