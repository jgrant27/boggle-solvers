
#import "utils.h"


char *ca_strndup(const char *t, size_t n) {
  size_t l = strlen(t);
  char *r;
  
  if (l > n)
    l = n;
  
  if (!(r = malloc(l+1)))
    return NULL;
  
  memcpy(r, t, l);
  r[l] = 0;
  return r;
}


char* sub_string(const char* str, size_t begin, size_t len)
{
  if (str == 0 || strlen(str) == 0 || strlen(str) < begin || strlen(str) < (begin+len))
    return NULL;

  return (char*)ca_strndup(str + begin, len);
}

char* strip(char* str) {
  char* nstr = malloc((size_t)strlen(str)+1);
  int i =0; int j = 0;
  while(j < strlen(str)) {
    if (str[j] != ' ' && str[j] != '\t' && str[j] != '\n' && str[j] != '\r') {
      nstr[i++] = str[j];
    } 
    j++;
  }
  nstr[i] = '\0';

  return nstr;
} 

int uppercase(char* str) {
  unsigned char c;
  while(*str) {
    c = *str; *str = (char)toupper(c); str++;
  }
  return 0;
}

