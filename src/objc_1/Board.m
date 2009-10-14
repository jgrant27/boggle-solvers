#import "Board.h"


@implementation Board


-(id) init: (char**) conf r: (long) r c: (long) c {
  if (self = [super init]) {
    rows = r;
    cols = c;
    letters = NULL;
    config = conf;
    [self setup_board];
  }
  return self;
}

-(void) setup_board {
  
  // set up board with letters
  //  cols = strlen(*config); // set column count
  //rows = (sizeof(config) * cols) / sizeof(char*); // set row count

  letters = malloc(sizeof(Letter**) * rows);
  long rcnt = 0 ; long ccnt = 0; 
  for (;rcnt < rows; rcnt++) {
    letters[rcnt] = malloc(sizeof(Letter*) * cols);
    for (;ccnt < cols; ccnt++) {
      Letter* letter = [[Letter alloc] init];
      [letter set_value: config[rcnt][ccnt]];
      [letter set_used: NO];
      letters[rcnt][ccnt] = letter;
    }
    ccnt = 0;
  }
  //printf("\n%c\n", [letters[0][0] value]);    

  // set up neighbors for each letter
  int roffset; int coffset; int ncnt = 0;
  Letter** neighbors;
  Letter* l;
  for (rcnt = 0; rcnt < rows; rcnt++) {
    for (ccnt = 0; ccnt < cols; ccnt++) {
      l = letters[rcnt][ccnt];
      neighbors = malloc(sizeof(Letter*) * MAX_NEIGHBORS);
      for (ncnt = 0, roffset = -1; roffset <= 1; roffset++) {
	int rind = rcnt + roffset;
	if (!(rind >= 0 && rind < rows)) continue;
	for (coffset = -1; coffset <= 1; coffset++) {
	  if (roffset == 0 && coffset == 0) continue;
	  int cind = ccnt + coffset;
	  if (!(cind >= 0 && cind < cols)) continue;
	  // add the neighbor
	  neighbors[ncnt] = letters[rind][cind];
	  ncnt++;
	}
      }
      // set the neighbors for the letter
      [l set_neighbors: neighbors];
    }
  }

}


-(char**) config_string {
  char* str = malloc((size_t)(sizeof(char*) * rows * cols + (rows * sizeof(char*))));

  long i = 0;

  for (i = 0; i < rows; i++) {
    strcat(str, config[i]);
    if (i != rows - 1) strcat(str,"\n");
  }

  return (char**)str;
}

-(long) rows {
  return rows;
}

-(long) cols {
  return cols;
}

-(Letter***) letters {
  return letters;
}

-(char**) config {
  return config;
}

-(void) set_config: (char**) c {
  config = c;
}

-(void) set_letters: (Letter***) l {
  letters = l;
}


@end
