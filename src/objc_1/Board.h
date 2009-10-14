//
// A Boggle board representation
//
// Author : Justin Grant                                                        
// Date : 03/14/2009
//


#import <objc/Object.h>
#import "Letter.h"

@interface Board: Object {
  long rows;
  long cols;
  Letter*** letters;
  char** config;
}


-(id) init: (char**) conf r: (long) r c: (long) c;
-(char**) config;
-(char**) config_string;
-(long) rows;
-(long) cols;
-(void) set_config: (char**) c;
-(Letter***) letters;
-(void) set_letters: (Letter***) l;

@end

