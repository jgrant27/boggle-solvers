

#import "tests.h"


void test() {

  // create a new instance
  Trie* trie = [[Trie alloc] init];

  printf("test trie insert of text %s\n", "LAMINATE");
  [trie insert: "LAMINATE"];
  printf("test trie include of text %s : %s (expect false)\n", 
	 "WANDER", [trie include: "WANDER"] ? "true" : "false");
  printf("test trie include of text %s : %s (expect true)\n", 
	 "LAMINATE", [trie include: "LAMINATE"] ? "true" : "false");
  printf("test trie begin of text %s : %s (expect false)\n", 
	 "WAN", [trie include: "WAN"] ? "true" : "false");
  printf("test trie begin of text %s : %s (expect true)\n", 
	 "LAM", [trie include: "LAM"] ? "true" : "false");
  printf("test trie subtree for letter %s : %i (expect level 4)\n", 
	 "LAMI'N'", [[[[[trie sub_trie: "L"] sub_trie: "A"] 
			sub_trie: "M"] sub_trie: "I"]  level]) ;

  char* padded_text = " PADDED?   \n\r";
  padded_text = (char*)strip(padded_text);
  printf("length of '%s' is %i\n", padded_text, strlen(padded_text));

  // free memory
  [trie release];

}

