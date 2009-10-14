//
// A Trie/Prefix Tree implementaion
//
// Author : Justin Grant                                                                            
// Date : 03/14/2009
//

#import "Trie.h"



@implementation Trie

-(id) init {
  if (self = [super init]) {
    level  = 0;
    depth = 0;
    count = 0;
    hash_ptr = [NSMutableDictionary dictionary];
  }
  return self;
}

-(int) level {
  return level;
}

-(int) depth {
  return depth;
}

-(int) count {
  return count;
}

-(int) set_level: (int) l {
  level = l;
}

-(int) set_depth: (int) d {
  depth = d;
}

-(int) set_count: (int) c {
  count = c;
}

-(NSMutableDictionary*) hash {
  return hash_ptr;
}

//-(void) set_hash: (cache_ptr) cp {
//  hash_ptr = cp;
//}

//-(void*) get_hash_value: (void*) key {
//  hash_value_for_key(hash_ptr, key);
//}

-(Trie*)getTrieFromDictForKey: (char*) str {
   NSString* key3 = [NSString stringWithCString: str encoding: NSASCIIStringEncoding];
   return (Trie*)[hash_ptr objectForKey:key3];
}

-(void)insertStringIntoDict: (char*) str usingKey: (char*) key {
  NSString* key1 = [NSString stringWithCString: key encoding: NSASCIIStringEncoding];
  NSString* obj1 = [NSString stringWithCString: str encoding: NSASCIIStringEncoding];
  [hash_ptr setObject: obj1 forKey: key1];
}


-(void)insertTrieIntoDict: (Trie*) trie usingKey: (char*) str {
  NSString* key2 = [NSString stringWithCString: str encoding: NSASCIIStringEncoding];
  [hash_ptr setObject: trie forKey: key2];
}

-(char*) curr_letter: (char*) word {
  // returns the letter for the current level
  if (strlen(word) == 0 || strlen(word) == level) {
    return (char*)nil;
  } else {
    return (char*)sub_string(word, level, 1);
  }
}

-(void) insert: (char*) word {
  char* letter = [self curr_letter: word];
  if (letter == NULL) {
    [self insertStringIntoDict: word usingKey: "*NULL*"];
  } else {
    Trie* tmp_trie = [self getTrieFromDictForKey: letter];
    if (tmp_trie == NULL) {
      tmp_trie = [[Trie alloc] init];
      [tmp_trie set_level: [self level] + 1]; 
      [self insertTrieIntoDict: tmp_trie usingKey: letter];
    }
    [tmp_trie insert: word];
  }
}

-(BOOL) include: (char*) word {
  char* letter = [self curr_letter: word];
  if (letter == NULL) {
    [self insertStringIntoDict: word usingKey: "*NULL*"];
  } else {
    Trie* tmp_trie = [self getTrieFromDictForKey: letter];
    tmp_trie != NULL && [tmp_trie include: word];
  }
}

-(BOOL) begin: (char*) word {
  char* letter = [self curr_letter: word];
  if (letter == NULL) {
    0;
  } else {
    Trie* tmp_trie = [self getTrieFromDictForKey: letter];
    tmp_trie != NULL && [tmp_trie begin: word];
  }
}

-(Trie*) sub_trie: (char*) letter {
  Trie* tmp_trie = [self getTrieFromDictForKey: letter];
  if (tmp_trie == NULL) {
      tmp_trie = [Trie alloc]; [tmp_trie init];
      [tmp_trie set_level: [self level] + 1]; 
      [self insertTrieIntoDict: tmp_trie usingKey: letter];
  }
  tmp_trie;
}

-(BOOL) load: (char*) path {
  
  FILE *file = fopen ( path, "r" );

  if ( file != NULL ) {
    char line [4096]; /* or other suitable maximum line size */
    int word_depth = 0;
    long lcnt = 0;
    while ( fgets ( line, sizeof line, file ) != NULL ) /* read a line */ {
      uppercase(line);
      char* word = (char*) strip(line);
      [self insert: word];
      if (strlen(word) > word_depth) word_depth = strlen(word);
      //fputs(line, stdout);
      lcnt++;
    }
    fclose(file);
    [self set_depth: word_depth];
    [self set_count: lcnt];
    return 1;
  } else {
    perror (path); /* why didn't the file open? */
    return 0;
  }

}

@end

