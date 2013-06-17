/*
 * header file for win32a[io].c
 */

int win32ai_open(int sample_rate);
int win32ai_read(unsigned char* buf, unsigned count);
int win32ai_close(void);

int win32ao_open(int sample_rate, float jbufsecs);
int win32ao_write(unsigned char* buf, unsigned count);
int win32ao_close(void);
