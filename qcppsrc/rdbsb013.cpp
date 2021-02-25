 /**************************************************************/
 /* Program Name - RDBSB013                                    */
 /* This program is called when a delete operation occurs in   */
 /* the ATMTRANS file.                                         */
 /*                                                            */
 /* This program will delete the records from ATMS and ACCTS   */
 /* based on the ATM ID and ACCT ID that are passed in from    */
 /* the trigger buffer.                                        */
 /*                                                            */
 /* The application will delete ATMID 10003 from the ATMTRANS  */
 /* file.                                                      */
 /*          8                                                 */
 /**************************************************************/
 #include <stdio.h>
 #include <stdlib.h>
 #include <recio.h>
 #include "#JMPRDSRC/QCPPSRC/RDBSB014" /* message handler include       */
 #include "qsysinc/h/trgbuf"           /* trigger buffer include without*/
                                       /* old and new records           */
 Qdb_Trigger_Buffer_t *hstruct;    /* pointer to the trigger buffer */
 char *datapt;

 #define KEYLEN 5

 /**************************************************************/
 /* Need to define file structures here since there are non-   */
 /* character fields in each file. For each non-character      */
 /* field, C requires boundary alignment. Therefore, a _PACKED */
 /* struct should be used in order to access the data that     */
 /* is passed to the trigger program.                          */
 /*                                                            */
 /**************************************************************/

 /** record area for ATMTRANS                 **/
_Packed struct rec {
              char atmid[5];
              char acctid[5];
              char tcode[1];
              char amount[5];
           } oldbuf, newbuf;

 /** record area for ATMS                     **/
 _Packed struct rec1{
              char atmn[5];
              char locat[2];
              char atmamt[9];
           } atmfile;

 /** record area for ACCTS                    **/
 _Packed struct rec2{
              char acctn[5];
              char bal[9];
              char actacc[1];
           } accfile;


 /********************************************************************/
 /* Start of the Main Line Code.  ************************************/
 /********************************************************************/
 main(int argc, char **argv) {
 _RFILE  *out1;                /* file pointer for ATMS        */
 _RFILE  *out2;                /* file pointer for ACCTS       */
 _RIOFB_T *fb;                 /* file feedback pointer        */
 char record[16];             /* record buffer            */
 _FEEDBACK fc;                 /* feedback for message handler */
 _HDLR_ENTRY hdlr = main_handler;
                               /********************************/
                               /* active exception handler     */
                               /********************************/
 CEEHDLR(&hdlr, NULL, &fc);;
                               /********************************/
                               /* ensure exception handler OK  */
                               /********************************/
 if (fc.MsgNo != CEE0000) {
    printf("Failed to register exception handler.\n");
    exit(99);
  }

 /* set pointer to the input parameter     */
 hstruct = (Qdb_Trigger_Buffer_t *)argv[1];
 datapt  = (char *) hstruct;

 /* Copy old and new record from the input parameter  */

  if ((strncmp(hstruct ->trigger_event,"2",1)== 0)|| /* delete event */
      (strncmp(hstruct -> trigger_event,"3",1)== 0)) /* update event */
    { obufoff = hstruct ->old_record_offset;
      memcpy(&oldbuf,datapt+obufoff,; hstruct->old_record_len);
    }
  if ((strncmp(hstruct -> trigger_event,"1",1)== 0) ||  /* insert event */
      (strncmp(hstruct -> trigger_event,"3",1)== 0))    /* update event */
    { nbufoff = hstruct ->new_record_offset;
      memcpy(&newbuf,datapt+nbufoff,; hstruct->new_record_len);
    }

 /*****************************************************/
 /* Open ATM and ACCTS    files                       */
 /*                                                   */
 /* Check the application's commit lock level. If it  */
 /* runs under commitment control, then open both     */
 /* files with commitment control. Otherwise, open    */
 /* both files without commitment control.            */
 /*****************************************************/
 if(strcmp(hstruct->commit_lock_level,"0") == 0) {  /* no commit    */
    if ((out1=_Ropen("APPLIB/ATMS","rr+")) == NULL) {
        printf("Error opening ATM file");
        exit(1);
      }
    if ((out2=_Ropen("APPLIB/ACCTS","rr+")) == NULL) {
        printf("Error opening ACCTS file");
        exit(1);
      }
  } else {                 /* with commitment control   */
    if ((out1=_Ropen("APPLIB/ATMS","rr+,commit=Y")) == NULL) {
        printf("Error opening ATMS file");
        exit(1);
      }
    if ((out2=_Ropen("APPLIB/ACCTS","rr+,commit=Y")) == NULL) {
        printf("Error opening ACCTS file");
        exit(1);
      }
   }

/* Delete the record based on the input parameter  */
 fb =_Rlocate(out1,&oldbuf.atmid,KEYLEN,__DFT);
 if (fb->num_bytes != 1) {
    printf("record not found in ATMS\n");
    _Rclose(out1);
    exit(1);
  }
  _Rdelete(out1);         /* delete record from ATMS */
  _Rclose(out1);

 fb =_Rlocate(out2,&oldbuf.acctid,KEYLEN,__DFT);
 if (fb->num_bytes != 1) {
    printf("record not found in ACCOUNTS\n");
    _Rclose(out2);
    exit(1);
  }
  _Rdelete(out2);         /* delete record from ACCOUNTS  */
  _Rclose(out2);

  } /* end of main */
