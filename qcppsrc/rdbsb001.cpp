#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <recio.h>

#define TRUE         0
#define FALSE       -1
#define MAX_LEN 3000
#define MAX_KEY_LEN 256

int main(void){
   _RFILE *fp;
   _RIOFB_T *iofb;
   char key[MAX_KEY_LEN];
   unsigned int key_len;
   char record[128];
   int i;
   unsigned long rrn =0;

   if ((fp = _Ropen("QTRFIL/LSHOHNS", "rr+ blkrcd=Y")) == NULL){
      printf("cannot open?");
      return 0;
   }

   memset(key, 0, sizeof(key));
   strcpy(key, "0001");
   key_len = 4;
   _Rlocate(fp, &key, key_len, __KEY_LT);  // Instrução SETLL


    /*[ READE ]*/
    for(i=0; i< 99999; i++){  // Instrução READE
       if(i == 0) {
          iofb = _Rreadk(fp, record,MAX_LEN, __KEY_NEXTUNQ, &key, key_len);
       } else {
          iofb = _Rreadk(fp, record,MAX_LEN, __KEY_NEXTEQ, &key, key_len);
       }
       if(iofb->num_bytes == EOF || iofb->rrn == rrn ||
              strncmp(iofb->key, key, key_len) != 0){
          break;
       }
       printf("record = %s?", record);
       rrn = iofb->rrn;
    }/*for-loop*/
    _Rclose(fp);
    getchar();
    return 0;
}
