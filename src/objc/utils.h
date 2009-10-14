
#ifndef UTILS_H
#define UTILS_H

#import <string.h>
#import <stdio.h>
#import <stdlib.h>
#import <sys/malloc.h>

extern char* ca_strndup(const char *t, size_t n);
extern char* sub_string(const char* str, size_t begin, size_t len);
extern char* strip(char* str);
extern int uppercase(char* str);

#endif
