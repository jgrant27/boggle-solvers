
#import "solver.h"


Trie* restrie;


void solve_board(Trie* trie, Board* board) {

  int rind; int cind;
  restrie = [[Trie alloc] init];

  for (rind = 0; rind < [board rows]; rind++) {
    for (cind = 0; cind < [board cols]; cind++) {
      char word[MAX_WORD_LENGTH] = {'\0'};
      find_words(trie, word, 0, [board letters][rind][cind]);
    }
  }
  
}


void find_words(Trie* trie, char* word, int fi, Letter* letter) {

  if (word == NULL || trie == NULL || letter == NULL) return;

  //printf("\n%s %c %i\n", word, [letter value], [letter unused_neighbors_count]);
  

  [letter set_used: YES];

  // Append the next letter to the word
  word[fi] = [letter value];
  word[fi+1] = '\0';

  Trie* tmp_trie = trie;

  // handle Q's
  if ([letter value] == 'Q') {
    //printf("%s\n", word);
    fi++;
    word[fi] = 'U'; 
    word[fi+1] = '\0'; 
    //tmp_trie = [trie sub_trie: "U"];
  } 

  //  for(; fi < 10; fi++) {
  //[trie begin: word];
  //[tmp_trie any_words];
    //[restrie insert: word];
    int i;
    //  for (i = 0; i < [letter unused_neighbors_count]; i++) {
    //  [letter unused_neighbors][i];
    //}
    //}

   fi++;

  if ([tmp_trie any_words] && [trie begin: word]) {
    if (![restrie include: word] && [trie include: word]) {
      [restrie insert: word];
      printf("%s\n", word);
    }
    if (fi < MAX_WORD_LENGTH) {
      int i;      
      for (i = 0; i < [letter unused_neighbors_count]; i++) {
	find_words(trie, word, fi, [letter unused_neighbors][i]);
      }
    }
  }

  // release the letter
  [letter set_used: NO];

}


