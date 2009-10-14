

#ifndef UTILS_H
#define UTILS_H

extern char* strip(char* str);
extern char* substring(const char* str, size_t begin, size_t len);
extern int uppercase(char* str);
extern void get_board_from_stdin(Board** bptr);
extern Trie* load_dict(char* load_path);
extern int get_random();


#endif
