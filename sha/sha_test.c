/*
   Program to test sha_old.c (SHA) and sha.c (SHA-1) hash algorithms.
   sha.c has the capability to impliment SHA, as well as being endian-
   independent (or dependent, depending how you look at it.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#if defined(unix)
#include <unistd.h>
#endif

int main(argc, argv)
int argc;
char *argv[];
{
void print_hashes();
int i;
char *text;

text=(char *)malloc(1000000);
if (text == NULL) {
  printf("error mallocing");
  exit(1);
}

printf("Test set 1:\n");
strcpy(text, "abc");
printf("%s\n", text);
print_hashes(text, strlen(text));
printf("NIST Message digest: 0164b8a9 14cd2a5e 74c4f7ff 082c4d97 f1edf880\n\n");

printf("Test set 2:\n");
strcpy(text, "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
printf("%s\n", text);
print_hashes(text, strlen(text));
printf("NIST Message digest: d2516ee1 acfa5baf 33dfc1c4 71e43844 9ef134c8\n\n");

printf("Test set 3:\n");
printf("1,000,000 repetitions of 'a'\n");
for(i=0; i<1000000; i++)
  text[i]='a';
text[i]='\0';
print_hashes(text, strlen(text));
printf("NIST Message digest: 3232affa 48628a26 653b5aaa 44541fd9 0d690603\n");
}


void
print_hashes (char *agreed_key, int length)
{
    int digest[5];
    unsigned char old_digest[20];
    extern void sha_memory();

    sha_memory(agreed_key, length, digest);

    printf ("     New SHA digest: %08x %08x %08x %08x %08x\n",
	     digest[0], digest[1], digest[2], digest[3],
	     digest[4]);
    
    shs_init();
    while (length-- > 0)
      shs_process(*agreed_key++);
    shs_hash(old_digest);

    printf ("     Old SHA digest: %02x%02x%02x%02x %02x%02x%02x%02x %02x%02x%02x%02x %02x%02x%02x%02x %02x%02x%02x%02x\n",
	     old_digest[0], old_digest[1], old_digest[2], old_digest[3],
	     old_digest[4], old_digest[5], old_digest[6], old_digest[7],
	     old_digest[8], old_digest[9], old_digest[10], old_digest[11],
	     old_digest[12], old_digest[13], old_digest[14], old_digest[15],
	     old_digest[16], old_digest[17], old_digest[18], old_digest[19]);
}
