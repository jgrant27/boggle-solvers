#import "tests.h"


void test_board(Board* board) {
  
  // create a new instance
  Trie* trie = [[Trie alloc] init];

  // board tests
  int x = get_random([board rows]); int y = get_random([board cols]);
  Letter* letter = [board letters][x][y];
  printf("\n\nBoard Tests : \ncharacter on board at [%ld][%ld] : %c\n", x, y, [letter value]);

  // neighbors
  printf("\nneighbors count of character on board at [%ld][%ld] (no more than 8) : %ld\n",
	 x, y, [letter neighbors_count]);
  printf("neighbors of character on board at [%ld][%ld] : ", x, y);
  Letter** neighbors = [letter neighbors]; int i;
  for (i = 0; i < [letter neighbors_count]; i++) { printf("%c", [neighbors[i] value]); }

  // unused neighbors
  [neighbors[0] set_used: YES]; [neighbors[2] set_used: YES];
  Letter** unused_neighbors = [letter unused_neighbors];
  printf("\nunused neighbors count of character on board at [%ld][%ld] (count - 2) : %ld",
	 x, y,  [letter unused_neighbors_count]);
  printf("\nunused neighbors of character on board at [%ld][%ld] : ", x, y);
  for (i = 0; i < [letter unused_neighbors_count]; i++) { 
    printf("'%c' ", [unused_neighbors[i] value]); 
  }
  printf("\n");
  
  // unused neighbors reset
  [neighbors[0] set_used: NO]; [neighbors[2] set_used: NO];
  unused_neighbors = [letter unused_neighbors];
  printf("unused neighbors count of character on board at [%ld][%ld] (count) : %ld",
	 x, y,  [letter unused_neighbors_count]);
  printf("\nunused neighbors of character on board at [%ld][%ld] : ", x, y);
  for (i = 0; i < [letter unused_neighbors_count]; i++) { 
    printf("'%c' ", [unused_neighbors[i] value]); 
  }
  printf("\n");


  // Trie tests
  printf("\nTrie Tests : \ntest trie insert of text %s\n", "LAMINATE");
  [trie insert: "LAMINATE"];

  char* cl = [trie curr_letter: "GREEN"];
  printf(" current letter of word %s : %c (expect G)\n",
	 "GREEN", cl[0]);
  assert(cl[0] == 'G');

  [trie set_level: 5];
  cl = [trie curr_letter: "YELLOW"];
  printf(" current letter of word %s : %c (expect W)\n",
	 "YELLOW", cl[0]);
  assert(cl[0] == 'W');
  [trie set_level: 0]; // careful !

  BOOL res = [trie include: "WANDER"];
  printf(" include of text %s : %s (expect false)\n",
	 "WANDER", res ? "true" : "false");
  assert(res == NO);

  res = [trie include: "LAMINATE"];
  printf(" include of text %s : %s (expect true)\n",
	 "LAMINATE", res ? "true" : "false");
  assert(res);

  res = [trie begin: "WAN"];
  printf(" begin of text %s : %s (expect false)\n",
	 "WAN", res ? "true" : "false");
  assert(res == NO);

  res = [trie begin: "LAM"];
  printf(" begin of text %s : %s (expect true)\n",
	 "LAM", res ? "true" : "false");
  assert(res);

  int level = [[[[[trie sub_trie: "L"] sub_trie: "A"]
  		  sub_trie: "M"] sub_trie: "I"]  level];
  printf(" subtree for letter %s : %i (expect level 4)\n",
  	 "LAMI'N'", level) ;
  assert(level == 4);

  char* padded_text = " PADDED?   \n\r";
  padded_text = (char*) strip(padded_text);
  printf(" length of '%s' is %i (expect 7)\n", padded_text, strlen(padded_text));
  assert(strlen(padded_text) == 7);

  //  sleep(30);

  // free memory
  [trie free];

  //sleep(30);

}


void run_dict_tests(Trie* trie) {

  //A simple lookup test
  printf("test tree load :\n");

  BOOL res;

  //printf("loaded trie depth : %i\n", [trie depth]);
  
  res = [trie begin: "ZOOPSY"];
  printf(" begin of text %s: %s (expect true)\n",
	 "ZOOPSY", res ? "true" : "false"); fflush(stdout);
  assert(res);

  res = [trie begin: "WILLNOT"];
  printf(" begin of text %s: %s (expect false)\n",
	 "WILLNOT", res ? "true" : "false"); fflush(stdout);
  assert(res == NO);

  res = [trie include: "ZOOPSYCHOLOGIES"];
  printf(" include of text %s: %s (expect true)\n",
	 "ZOOPSYCHOLOGIES", res ? "true" : "false"); fflush(stdout);
  assert(res);

  res = [trie include: "WILLNOTBEFOUND"];
  printf(" include of text %s: %s (expect false)\n",
	 "WILLNOTBEFOUND", res ? "true" : "false"); fflush(stdout);
  assert(res == NO);

}
