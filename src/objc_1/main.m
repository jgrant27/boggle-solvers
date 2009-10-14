


#import <stdio.h>
#import <stdlib.h>
#import <time.h>


#import "Trie.h"
#import "Board.h"
#import "Letter.h"
#import "solver.h"
#import "tests.h"


int main( int argc, const char **argv ) {

  srand((unsigned)time(NULL)/2);

  Board* board;
  char* load_path = NULL;
  BOOL run_tests = NO;
  long sleep_delay = 0;

  long i = 1;
  for (; i < argc; i++) {
    if(argv[i]!=NULL){
      if (strncmp(argv[i], "--test", 6) == 0) {
	run_tests = YES;
      } else if (strncmp(argv[i], "--dict", 6) == 0) {
	load_path=(char*)argv[i+1];
      } else if (strncmp(argv[i], "--sleep", 7) == 0) {
	sleep_delay = strtol(argv[i+1], NULL, 10); 
      }
    }
  }

  // Load the board from stdin
  get_board_from_stdin(&board);

  //run some simple tests
  if (run_tests) { test_board(board); }

  // Load the dictionary
  Trie* trie = load_dict(load_path);

  // run some quick tests on the Trie if --test has been used
  if (run_tests) { run_dict_tests(trie); }
 
  clock_t start;
  clock_t end;
  start = clock();

  // solve the board !
  if (run_tests) { printf("\nsolving board ...\n"); }

  solve_board(trie, board);

  // clean up
  [trie free];

  //if (run_tests) {
    printf("\nboard solve time :       %.9fs\n", (float)(clock() - start) / CLOCKS_PER_SEC);
  //}

  // if --sleep secs has been used
  if (sleep_delay > 0) {
    printf("\nsleeping for %i seconds before exit...\n\n", sleep_delay); fflush(stdout);
    sleep(sleep_delay);
  } 

  exit(0);

}


