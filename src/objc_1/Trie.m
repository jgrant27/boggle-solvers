#import "Trie.h"
#import "solver.h"

@implementation Trie

+(cache_ptr) new_hash {
  return hash_new(64, &hash_string, &compare_strings);
}

-(id) init {
  if (self = [super init]) {
    level  = 0;
    depth = 0;
    count = 0;
    hash_ptr = [Trie new_hash];
    curr_letter = malloc(sizeof(char) * 2);    
  }
    
  return self;
}

-(int) any_words {
  hash_ptr->used > 0;
}


-(char*) curr_letter: (char*) word {
  // returns the letter for the current level
  if (strlen(word) == 0 || strlen(word) == level) {
    return NULL;
  } else {
    char* wp = word;
    realloc(curr_letter, sizeof(char) * 2);
    long i = 0;
    while (i != level) { i++; wp++; }
    curr_letter[0] = *wp;
    curr_letter[1] = '\0';
    return curr_letter;
  }
}

-(void) insert: (char*) word {
  char* letter = [self curr_letter: word];
  if (letter == NULL || strlen(letter) == 0) {
    hash_add(&hash_ptr, NULL_KEY, word);
  } else {
    Trie* tmp_trie = (Trie*)[self get_hash_value: letter];
    if (tmp_trie == NULL) {
      tmp_trie = [Trie alloc]; [tmp_trie init];
      [tmp_trie set_level: [self level] + 1]; 
      hash_add(&hash_ptr, letter, tmp_trie);
    }
    [tmp_trie insert: word];
  }
}

-(int) include: (char*) word {
  char* letter = [self curr_letter: word];

  if (letter == NULL || strlen(letter) == 0) {
    char* val = [self get_hash_value: NULL_KEY];
    if (val == NULL) return 0;
    return strcmp(val, word) == 0;
  } else {
    Trie* tmp_trie = (Trie*)[self get_hash_value: letter];
    return tmp_trie != NULL && [tmp_trie include: word];
  }
}

-(int) begin: (char*) word {
  char* letter = [self curr_letter: word];

  if (letter == NULL || strlen(letter) == 0) {
    //return hash_is_key_in_hash(hash_ptr, NULL_KEY);
    return hash_ptr;
  } else {
    Trie* tmp_trie = (Trie*)[self get_hash_value: letter];
    return tmp_trie != NULL && [tmp_trie begin: word];
  }
  
}

-(Trie*) sub_trie: (char*) letter {
  Trie* tmp_trie = (Trie*)[self get_hash_value: letter];
  if (tmp_trie == NULL) {
      tmp_trie = [[Trie alloc] init];
      [tmp_trie set_level: [self level] + 1]; 
      hash_add(&hash_ptr, letter, tmp_trie);
  }
  return tmp_trie;
}

-(int) load: (char*) path {
  
  FILE *file = fopen ( path, "r" );

  if ( file != NULL ) {
    char line [1024 * 1024]; /* or other suitable maximum line size */
    long word_depth = 0;
    long count = 0;
    char* word;

    while ( fgets ( line, sizeof line, file ) != NULL ) /* read a line */ {
      word = (char*) toupper(strip(line));
      // don't even insert invalid words in the trie
      if (strlen(word) < MIN_WORD_LENGTH) continue;

      [self insert: word];
      if (strlen(word) > word_depth) word_depth = strlen(word);
      //fputs(line, stdout);
      count++;
    }
    fclose(file);
    [self set_depth: word_depth];
    [self set_count: count];

    return 0;

  } else {

    perror (path); /* why didn't the file open? */
    return 1;

  }

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

-(cache_ptr) hash {
  return hash_ptr;
}

-(void) set_hash: (cache_ptr) cp {
  hash_ptr = cp;
}

-(void*) get_hash_value: (void*) key {
  hash_value_for_key(hash_ptr, key);
}


@end

