
#ifndef TESTS_H
#define TESTS_H

#include <stdio.h>
#include <assert.h>

#include "trie.h"
#include "board.h"
#include "letter.h"
#include "utils.h"
#include "assert.h"

extern void test_board(board* brd);
extern void run_dict_tests(trie* t);

#endif
