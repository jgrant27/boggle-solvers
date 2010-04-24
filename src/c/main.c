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



#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "board.h"
#include "trie.h"
#include "solver.h"
#include "tests.h"


int main( int argc, const char **argv ) {

  srand((unsigned)time(NULL)/2);

  board* brd;
  char* load_path = NULL;
  short run_tests = 0;
  int sleep_delay = 0;

  int i = 1;
  for (; i < argc; i++) {
    if(argv[i]!=NULL){
      if (strcmp(strip((char*)argv[i]), "--test") == 0) {
        run_tests = 1;
      } else if (strcmp(strip((char*)argv[i]), "--dict") == 0) {
        load_path=(char*)argv[i+1];
      } else if (strcmp(strip((char*)argv[i]), "--sleep") == 0) {
        sleep_delay = strtol(strip((char*)argv[i+1]), NULL, 10);
      }
    }
  }

  // Load the board from stdin
  brd = get_board_from_stdin(run_tests);

  //run some simple tests
  if (run_tests) { test_board(brd); }

  // Load the dictionary
  trie* t = load_dict(load_path, run_tests);

  // run some quick tests on the Trie if --test has been used
  if (run_tests) { run_dict_tests(t); }

  clock_t start;
  clock_t end;
  start = clock();

  // solve the board !
  if (run_tests) { printf("\nsolving board ...\n"); }

  solve_board(t, brd);

  if (run_tests) {
    printf("\nboard solve time :       %.9fs\n", (float)(clock() - start) / CLOCKS_PER_SEC);
  }

  // if --sleep secs has been used
  if (sleep_delay > 0) {
    if (run_tests) {
      printf("\nsleeping for %i seconds before exit...\n\n", sleep_delay); fflush(stdout);
      sleep(sleep_delay);
    }
  }

  // clean up
  free(brd);
  free(t);

  exit(0);

}


