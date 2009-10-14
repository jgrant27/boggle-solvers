//
// A Trie/Prefix Tree implementaion
//
// Author : Justin Grant                                                                            
// Date : 03/14/2009
//


#ifndef TRIE_H
#define TRIE_H


#import <Foundation/NSObject.h>
#import <Foundation/NSDictionary.h>
#import <Foundation/NSString.h>



@interface Trie: NSObject {
  int level;
  int depth;
  int count;
  NSMutableDictionary* hash_ptr;
}

-(id) init;
-(int) level;
-(int) depth;
-(int) count;
-(int) set_level: (int) l;
-(int) set_depth: (int) d;
-(int) set_count: (int) c;

-(NSMutableDictionary*) hash; 
//-(void) set_hash: (cache_ptr) cp;

-(char*) curr_letter: (char*) word;
-(void) insert: (char*) word;
-(BOOL) include: (char*) word;
-(BOOL) begin: (char*) word;
-(Trie*) sub_trie: (char*) letter;
-(BOOL) load: (char*) path;


@end


#import <stdlib.h>
#import <stdio.h>
#import <string.h>


#endif
