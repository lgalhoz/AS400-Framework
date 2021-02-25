#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE         0
#define FALSE       -1

void main(void){
    char command[128];
    char mylib[11];

    system("DSPLIBL *PRINT");

    memset(mylib, 0, sizeof(mylib));
    strcpy(mylib, "QTEMP");
    sprintf(command, "DSPLIB LIB('%s') *PRINT", mylib);
    system(command);
    getchar();
}
