

#import <string.h>
#import <stdlib.h>
#include <ctype.h>

#import "Board.h"
#import "Trie.h"
#import "utils.h"



#define LINE_MAX 4096


int get_random(long ceil) {
  return rand() % ceil;
}

char* substring(const char* str, size_t begin, size_t len)
{
  if (str == 0 || strlen(str) == 0 || strlen(str) < begin || strlen(str) < (begin+len))
    return NULL;

  return (char*)strndup(str + begin, len);
}

char* strip(char* str) {
  char* nstr = malloc((size_t)strlen(str)+1);
  long i = 0; long j = 0;
  while(j < strlen(str)) {
    if (str[j] != ' ' && str[j] != '\t' && str[j] != '\n' && str[j] != '\r') {
      nstr[i++] = str[j];
    } 
    j++;
  }
  nstr[i] = '\0';

  return nstr;
} 

int uppercase(char* str) {
  unsigned char c;
  while(*str) {
    c = *str; *str = (char)toupper(c); str++;
  }
  return 0;
}


void get_board_from_stdin(Board** bptr) {

  //char* boards_str;
  char** config = malloc(sizeof(char*));
  char* line = malloc(sizeof(char) * LINE_MAX);
  long lncnt = 0;

  while(fgets(line, LINE_MAX + 1, stdin) != NULL) {
    
    char* oline = strip(line);
    char* sline = oline;
    char* cline = sline;
    char cc;

    // if row is empty then skip
    if (strlen(oline) < 2 || !isalpha(*sline)) continue;

    config = realloc(config, (lncnt+1) * sizeof(char*) * LINE_MAX);

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

    *cline = '\0';
    config[lncnt] = oline;
    lncnt++;

  }  
   
  //char* config[] = {"UCDNA", "WIISE", "EGLAI", "MLAIO", "XXXXX"};
  Board* board = *bptr = [[Board alloc] init: config r: lncnt c: strlen(config[0])];
  printf("\nBoard loaded : \n  rows: %i cols: %i\n", [board rows], [board cols]);
  printf("config: \n");
  printf("'%s'", [board config_string]);
  //printf("%s\n...\n", substring([board config_string], 0, [board cols] * 3));

}


Trie* load_dict(char* load_path) {

  if (load_path == NULL || strlen(load_path) < 1) {
    printf("\nNo dictionary available.\nSpecify a word dictionary e.g. boggle --dict $PATH_TO_DICT\n");
    exit(0);
  }
  
  // create a new instance
  Trie* trie = [[Trie alloc] init];
  printf("\nloading dictionary from %s ",load_path); fflush(stdout);
  [trie load:load_path];
  printf("done.\n"); fflush(stdout);
  printf("%i words loaded. \n", [trie count]); fflush(stdout);
  printf("trie depth: %i, level: %i \n\n", [trie depth], [trie level]); fflush(stdout);
  
  return trie;
  
}

