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

#include "board.h"




board* new_board(char** conf, int r, int c) {
  board* brd = (board*)malloc(sizeof(board));

  brd->rows = r;
  brd->cols = c;
  brd->config = conf;

  setup_board(brd);

  return brd;
}

void setup_board(board* brd) {

  brd->letters = (struct letter***)malloc(sizeof(letter**) * brd->rows);
  memset(brd->letters, 0, sizeof(brd->letters));
  int rcnt = 0 ; int ccnt = 0; 
  for (;rcnt < brd->rows; rcnt++) {
    brd->letters[rcnt] = (struct letter**)malloc(sizeof(letter*) * brd->cols);
    for (;ccnt < brd->cols; ccnt++) {
      letter* ltr = new_letter();
      ltr->value= brd->config[rcnt][ccnt];
      ltr->used = 0;
      brd->letters[rcnt][ccnt] = (struct letter*)ltr;
    }
    ccnt = 0;
  }

  // set up neighbors for each letter
  int roffset; int coffset; int ncnt = 0;
  letter** neighbors;
  letter* ltr;
  for (rcnt = 0; rcnt < brd->rows; rcnt++) {
    for (ccnt = 0; ccnt < brd->cols; ccnt++) {
      ltr = (letter*)brd->letters[rcnt][ccnt];
      for (ncnt = 0, roffset = -1; roffset <= 1; roffset++) {
        int rind = rcnt + roffset;
        if (!(rind >= 0 && rind < brd->rows)) continue;
        for (coffset = -1; coffset <= 1; coffset++) {
          if (roffset == 0 && coffset == 0) continue;
          int cind = ccnt + coffset;
          if (!(cind >= 0 && cind < brd->cols)) continue;
          // add the neighbor
          ltr->neighbors[ltr->ncount++] = brd->letters[rind][cind];
        }
      }
    }
  }
  
}


char* config_string(board* brd) {
  char* str = (char*)malloc((sizeof(char) * brd->rows * brd->cols)
			    + (brd->rows * sizeof(char))); // for new line chars
  memset(str, 0, sizeof(str));

  int i = 0;

  for (i = 0; i < brd->rows; i++) {
    strcat(str, brd->config[i]);
    if (i != brd->rows - 1) strcat(str,"\n");
  }

  return (char*)str;
}


board* get_board_from_stdin(int show_output) {

  char** config = (char**)malloc(sizeof(char*));
  char* line =(char*) malloc(sizeof(char) * LINE_MAX);
  int lncnt = 0;

  while(fgets(line, LINE_MAX + 1, stdin) != NULL) {
    
    char* oline = strip(line);
    char* sline = oline;
    char* cline = sline;
    char cc;

    // if row is empty then skip
    if (strlen(oline) < 2 || !isalpha(*sline)) continue;

    config = (char**)realloc(config, (lncnt+1) * sizeof(char*) * LINE_MAX);

    // ignore spaces or commas separating letters
    while (*sline) {
      if (!isalpha(*sline)) { 
        ++sline; 
        continue;
      } else { 
        cc = toupper(*sline++);
        *cline++ = cc; 
        // handle Q's
        if(cc == 'Q' && *sline == 'U') sline++;
      }
    }

    *cline = 0;
    config[lncnt] = oline;
    lncnt++;

  }  
   
  board* brd = new_board(config, lncnt, strlen(config[0]));

  //char* config[] = {"UCDNA", "WIISE", "EGLAI", "MLAIO", "XXXXX"};
  if (show_output) {
    printf("\nboard loaded : \n  rows: %i cols: %i\n", brd->rows, brd->cols);
    printf("config: \n");
    printf("'%s'", config_string(brd));
    //printf("%s\n...\n", substring([board config_string], 0, [board cols] * 3));
  }

  return brd;

}


