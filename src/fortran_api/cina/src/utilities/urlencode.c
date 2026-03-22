#include <stdio.h>
#include <ctype.h>

/* One web reference for URL encoding is
   http://www.blooberry.com/indexdot/html/topics/urlencoding.htm
*/

int main() {
  int char_in;

  while ((char_in = getchar()) != EOF) {
    /* echo characters that don't need to be escaped */
    if (isalnum(char_in) || char_in == '-' || char_in == '_' ||
	char_in == '.' || char_in == '*') putchar(char_in);

    /* otherwise escape character */
    else {
      printf("%%%02x",char_in);
    }
  }

  return 0;
}
