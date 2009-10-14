#import "Letter.h"


@implementation Letter


-(id) init {
  if (self = [super init]) {
    value = NULL;
    used = NO;
    neighbors = NULL;
    uneighbors = malloc(MAX_NEIGHBORS * (sizeof(Letter*)));
    uncount = 0;
  }
  return self;
}

-(char) value {
  return value;
}

-(BOOL) used {
  return used;
}

-(Letter**) neighbors {
  return neighbors;
}

-(int) neighbors_count {
  return neighbors_count;
}


-(int) unused_neighbors_count {
  return uncount;
}

-(Letter**) unused_neighbors {

  int i; int ui;

  for (i = 0, ui = 0; i < neighbors_count; i++) {
    if (neighbors[i] != NULL && ![neighbors[i] used])
      uneighbors[ui++] = neighbors[i];
  }

  uncount = ui;

  return uneighbors;
}

-(int) set_value: (char) c {
  value = c;
}

-(void) set_used: (BOOL) u {

    if (used == u) return;

  used = u;

  if (used == YES) {
    uncount++;
  } else {
    uncount--;
  }

}

-(void) set_neighbors: (Letter**) n {

  uncount = neighbors_count = MAX_NEIGHBORS;
  int i = 0;
  
  for (; i < MAX_NEIGHBORS; i++) {
    if (n[i] == NULL) {
      neighbors_count--;
    } else {
      if (![n[i] used]) uncount--;
    }
  }

  neighbors = n;

}

-(void) set_prev_letter: (Letter*) pl {
  prev_letter = pl;
}

-(void) set_neighbors_count: (int) cnt {
  neighbors_count = cnt;
}



@end

