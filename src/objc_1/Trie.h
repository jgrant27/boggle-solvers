//
// A Trie/Prefix Tree 
//
// Author : Justin Grant                                                                            
// Date : 03/14/2009
//


#import <objc/Object.h>
#import <objc/hash.h>


#define NULL_KEY "*NULL*"


@interface Trie: Object {
  long level;
  long depth;
  long count;
  cache_ptr hash_ptr;
  char* curr_letter;
}

+(cache_ptr) new_hash;

-(id) init;

-(char*) curr_letter: (char*) word;
-(void) insert: (char*) word;
-(int) include: (char*) word;
-(int) begin: (char*) word;
-(Trie*) sub_trie: (char*) letter;
-(int) load: (char*) path;
-(int) any_words;

-(int) level;
-(int) depth;
-(int) set_level: (int) l;
-(int) set_depth: (int) d;
-(int) count;
-(cache_ptr) hash; 
-(void) set_hash: (cache_ptr) cp;
-(void*) get_hash_value: (void*) key;
-(int) set_count: (int) c;


@end
