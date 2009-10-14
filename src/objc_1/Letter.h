//
// A Letter representation for a Boggle board
//
// Author : Justin Grant                                                        
// Date : 03/14/2009
//


#import <objc/Object.h>
#import <malloc.h>

static const int MAX_NEIGHBORS = 8;


@interface Letter: Object {
  char value;
  BOOL used;
  Letter** neighbors;
  Letter* prev_letter;
  int neighbors_count;
  Letter** uneighbors;
  int uncount;
}

-(id) init;
-(char) value;
-(BOOL) used;
-(Letter**) neighbors;
-(int) neighbors_count;
-(Letter**) unused_neighbors;
-(int) unused_neighbors_count;
-(int) set_value: (char) c;
-(void) set_prev_letter: (Letter*) pl;
-(void) set_used: (BOOL) u;
-(void) set_neighbors: (Letter**) n;
-(void) set_neighbors_count: (int) cnt;


@end
