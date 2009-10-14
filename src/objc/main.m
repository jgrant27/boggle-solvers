
#ifndef MAIN_H
#define MAIN_H

#import <Foundation/NSAutoreleasePool.h>
#import <stdlib.h>
#import <stdio.h>
#import "tests.h"


int main( int argc, const char **argv ) {

  NSAutoreleasePool *autoreleasepool = [[NSAutoreleasePool alloc] init];

  char* load_path = NULL;
  BOOL run_tests = NO;
  long sleep_delay = 0;

  int i = 1;
  for (; i < sizeof(argv); i++) {
    if(argv[i]!=NULL){
      if (strncmp(argv[i], "--test", 6) == 0) {
        run_tests = YES;
      } else if (strncmp(argv[i], "--dict", 6) == 0) {
        load_path = (char*)argv[i+1];
      } else if (strncmp(argv[i], "--sleep", 7) == 0) {
        sleep_delay = strtol(argv[i+1], NULL, 10);
      }
    }
  }
  
  if (load_path == NULL || strlen(load_path) < 1) {
    printf("No dictionary available.\nSpecify a word dictionary e.g. boggle --dict $PATH_TO_DICT\n");
    exit(0);
  }

  //run some simple tests
  if (run_tests) { test(); }

  // create a new instance
  Trie* trie = [[Trie alloc] init];

  //load it up
  trie=[[Trie alloc] init];
  printf("\nloading dictionary from %s ",load_path); fflush(stdout);
  [trie load:load_path];
  printf("done.\n"); fflush(stdout);
  printf("%i words loaded. \n", [trie count]); fflush(stdout);
  printf("trie depth: %i, level: %i \n\n", [trie depth], [trie level]); fflush(stdout);

  if (run_tests) {
    //A simple lookup test
    printf("test trie include of text %s: %s (expect true)\n",
           "ZOOPSYCHOLOGIES", [trie include: "ZOOPSYCHOLOGIES"] ? "true" : "false"); fflush(stdout);
    printf("test trie include of text %s: %s (expect false)\n",
           "WILLNOTBEFOUND", [trie include: "WILLNOTBEFOUND"] ? "true" : "false"); fflush(stdout);
  }

  [trie release];

  [autoreleasepool release];

  if (sleep_delay > 0) {
    printf("\nsleeping for %i seconds before exit...\n\n", sleep_delay); fflush(stdout);
    sleep(sleep_delay);
  }

  exit(0);

}


#endif
