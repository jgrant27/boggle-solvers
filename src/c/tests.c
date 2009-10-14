#include "tests.h"



void test_board(board* brd) {
  
  // create a new instance
  trie* t = new_trie();
  
  // board tests
  int x = get_random(brd->rows);
  int y = get_random(brd->cols);
  letter* ltr = (letter*)brd->letters[x][y];

  printf("\n\nBoard Tests : \ncharacter on board at [%ld][%ld] : %c\n", x, y, ltr->value);
  
  // neighbors
  printf("neighbors of character on board at [%ld][%ld] : ", x, y);
  int i;
  for (i = 0; i < ltr->ncount; i++) { 
    printf("%c", ((letter*)ltr->neighbors[i])->value); 
  }

  // unused neighbors
  letter* ul1 = (letter*)ltr->neighbors[1];
  letter* ul2 = (letter*)ltr->neighbors[2];
  ul1->used = 1; ul2->used = 1;
  printf("\nunused neighbors of character on board at [%ld][%ld] (set [1,2] as used]) : ", x, y);
  for (i = 0; i < ltr->ncount; i++) {
    letter* n = (letter*)ltr->neighbors[i];
    if(n != NULL && !n->used) {
      printf("%c", ((letter*)ltr->neighbors[i])->value);
    }
  }

  // unused neighbors reset
  ul1->used = 0; ul2->used = 0;
  printf("\nunused neighbors of character on board at [%ld][%ld] (all unused) : ", x, y);
  for (i = 0; i < ltr->ncount; i++) {
    letter* n = (letter*)ltr->neighbors[i];
    if(n != NULL && !n->used) {
      printf("%c", ((letter*)ltr->neighbors[i])->value);
    }
  }
  printf("\n");

  // Trie tests
  printf("\nTrie Tests : \ntest trie insert of text %s\n", "LAMINATE");
  insert(t, "LAMINATE");

  char cl = "GREEN"[t->level];
  printf(" current letter of word %s : %c (expect G)\n",
         "GREEN", cl);
  assert(cl == 'G');

  t->level = 5;
  cl = "YELLOW"[t->level];
  printf(" current letter of word %s : %c (expect W)\n",
         "YELLOW", cl);
  assert(cl == 'W');
  t->level = 0; // careful !

  short res = include(t, "WANDER");
  printf(" include of text %s : %s (expect false)\n",
         "WANDER", res ? "true" : "false");

  assert(!res);

  res = include(t, "LAMINATE");
  printf(" include of text %s : %s (expect true)\n",
         "LAMINATE", res ? "true" : "false");
  assert(res);

  res = begin(t, "WAN");
  printf(" begin of text %s : %s (expect false)\n",
         "WAN", res ? "true" : "false");
  assert(!res);

  res = begin(t, "LAM");
  printf(" begin of text %s : %s (expect true)\n",
         "LAM", res ? "true" : "false");
  assert(res);

  int level =
    ((trie*)
     ((trie*)
      ((trie*)
       ((trie*)t->
        children['L' - 'A'])->
       children['A' - 'A'])->
      children['M' - 'A'])->
     children['I' - 'A'])->level;
    
  printf(" subtree for letter %s : %i (expect level 4)\n", "LAMI'N'", level) ;
  assert(level == 4);

  char* padded_text = " PADDED?   \n\r";
  padded_text = strip(padded_text);
  printf(" length of '%s' is %i (expect 7)\n", padded_text, strlen(padded_text));
  assert(strlen(padded_text) == 7);

}


void run_dict_tests(trie* t) {

  //A simple lookup test
  printf("test tree load :\n");

  int res;

  res = begin(t, "ZOOPSY");
  printf(" begin of text %s: %s (expect true)\n",
         "ZOOPSY", res ? "true" : "false"); fflush(stdout);
  assert(res);

  res = begin(t, "WILLNOT");
  printf(" begin of text %s: %s (expect false)\n",
         "WILLNOT", res ? "true" : "false"); fflush(stdout);
  assert(res == 0);

  res = include(t, "ZOOPSYCHOLOGIES");
  printf(" include of text %s: %s (expect true)\n",
         "ZOOPSYCHOLOGIES", res ? "true" : "false"); fflush(stdout);
  assert(res);

  res = include(t, "WILLNOTBEFOUND");
  printf(" include of text %s: %s (expect false)\n",
         "WILLNOTBEFOUND", res ? "true" : "false"); fflush(stdout);
  assert(res == 0);

}
