/*
convert binary file to a C initializer
arg 1 = type and name of array
uint8_t is recommended

build helper for nautilus
C) johannes P"ohlmann, 2009, License: GPL2 and artistic Licence.
*/
#include <stdio.h>
#include <stdint.h>

int main(int argc,char **argv){
	int col=0;
	uint8_t	c;
	printf ( "%s [] = {", argv[1]);
	if (( fread (&c, 1, 1, stdin)) > 0)
	{
            printf ("0x%x\n",c);
			while( ! feof(stdin)){
				if (( fread (&c, 1, 1, stdin)) > 0)
				{
					printf (",0x%x",c);
					if (col++ >15){
						puts("");	
						col=0;
					}
				}
			}
			puts("};\n");
	}
	return 0;
}
