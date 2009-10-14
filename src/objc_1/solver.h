
#ifndef SOLVER_H
#define SOLVER_H

#import "Trie.h"
#import "Board.h"
#import "Letter.h"

#define MIN_WORD_LENGTH 3
#define MAX_WORD_LENGTH 16

extern void solve_board(Trie* trie, Board* board);
extern void find_words(Trie* trie, char* word, int fi, Letter* letter);

#endif
