/********************************************************************/
/* Program Name:          RTVFD                                     */
/*                                                                  */
/* Program Language:      ILE C                                     */
/*                                                                  */
/* Description:           Retrieve a file definition template to a  */
/*                        user space.                               */
/*                                                                  */
/* Header Files Included: <stdlib.h>                                */
/*                        <signal.h>                                */
/*                        <string.h>                                */
/*                        <stdio.h>                                 */
/*                        <quscrtus.h>                              */
/*                        <quscusat.h>                              */
/*                        <qusptrus.h>                              */
/*                        <qdbrtvfd.h>                              */
/*                        <qusec.h>                                 */
/*                        <qus.h>                                   */
/*                        <qliept.h>                                */
/*                                                                  */
/* APIs Used:             QUSCRTUS - Create User Space              */
/*                        QUSCUSAT - Change User Space Attributes   */
/*                        QUSPTRUS - Retrieve Pointer to User Space */
/*                        QDBRTVFD - Retrieve File Description      */
/********************************************************************/
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <stdio.h>
#include <quscrtus.h>
#include <quscusat.h>
#include <qusptrus.h>
#include <qdbrtvfd.h>
#include <qusec.h>
#include <qus.h>
#include <qliept.h>             /* Note that this must be the last */
                                /* include specified.              */

int error_flag = 0;             /* Set by error handler            */
/********************************************************************/
/* Function:    error_handler                                       */
/* Description: Handle exceptions.                                  */
/********************************************************************/
void error_handler(int errparm) {
 _INTRPT_Hndlr_Parms_T ExcDta = {0};
 _GetExcData(&ExcDta);
 error_flag = 1;
 signal(SIGALL,error_handler);
}

/********************************************************************/
/* Start of main procedure                                          */
/********************************************************************/
main(int argc, char **argv) {
  typedef struct attrib_struct {
    int attrib_count;
    Qus_Vlen_Rec_3_t  keyinfo;
    char key_value;
  } attrib_struct;

  Qus_EC_t error_code;               /* Error code parameter        */
  attrib_struct attrib_info;         /* Attribute to change         */
  char user_space[21];               /* User space and library      */
  char descr[50];                    /* Text description            */
  char initial_value = 0x00;         /* Initial value for user space*/
  char return_lib[10];               /* Return library              */
  char ret_file_lib[20];             /* Returned file and library   */
  char file_and_lib[21];             /* File and library            */
  char record_fmt[11];               /* Record format name          */
  char *space_ptr;                   /* Pointer to user space object*/

  /******************************************************************/
  /* Start of executable code.                                      */
  /******************************************************************/
  if (argc != 4) {
     printf("This program requires 3 parameters:\n");
     printf("   1) User space name and library\n");
     printf("   2) File name and library\n");
     printf("   3) Record format name\n");
     printf("Please retry with those parameters.\n");
     exit(1);
  }

  memcpy(user_space, *++argv, 20);
  memcpy(file_and_lib, *++argv, 20);
  memcpy(record_fmt, *++argv, 10);
  memset(descr,' ',50);
  memcpy(descr,"RTVFD User Space",16);

  signal(SIGALL,error_handler);      /* Enable the error handler    */
  error_code.Bytes_Provided=0;       /* Have APIs return exceptions */

  /******************************************************************/
  /* Create the user space.                                         */
  /******************************************************************/
  QUSCRTUS(user_space,               /* User space                  */
           "          ",             /* Extended attribute          */
           1024,                     /* Initial size                */
           &initial_value,           /* Initial value               */
           "*CHANGE   ",             /* Public authority            */
           descr,                    /* Text description            */
           "*YES      ",             /* Replace if it exists        */
           &error_code,              /* Error code                  */
           "*USER     ");            /* Domain = USER               */

  if (error_flag) {
     printf("(QUSCRTUS) Ocorreram Erros.\n");
     exit(1);
  }

  /******************************************************************/
  /* Initialize the attributes to change structure.                 */
  /******************************************************************/
  attrib_info.attrib_count = 1;      /* Number of attributes        */
  attrib_info.keyinfo.Key = 3;       /* Key of attribute to change  */
  attrib_info.keyinfo.Length_Vlen_Record = 1;
                                     /* Length of data              */
  attrib_info.key_value='1';         /* Autoextend space            */

  /******************************************************************/
  /* Change the user space to be automatically extendable.          */
  /******************************************************************/
  QUSCUSAT(return_lib,               /* Return library              */
           user_space,               /* User space name and library */
           &attrib_info,             /* Attributes to change        */
           &error_code);             /* Error code                  */

  if (error_flag) {
     printf("(QUSCUSAT) Ocorreram Erros.\n");
     exit(1);
  }

  /******************************************************************/
  /* Retrieve a pointer to the user space object.                   */
  /******************************************************************/
  QUSPTRUS(user_space,&space_ptr);
  if (error_flag) {
     printf("(QUSPTRUS) Ocorreram Erros.\n");
     exit(1);
  }

  /******************************************************************/
  /* Retrieve the file description information to the user space.   */
  /******************************************************************/
  QDBRTVFD(space_ptr,                /* Receiver variable           */
           16776704,                 /* Return up to 16MB minus 512 */
                                     /* bytes of data               */
           ret_file_lib,             /* Returned file and library   */
           "FILD0100",               /* File definition template    */
           file_and_lib,             /* File and library name       */
           record_fmt,               /* Record format name          */
           "0",                      /* No override processing      */
           "*LCL      ",             /* Local system                */
           "*INT      ",             /* Internal formats (1)        */
           &error_code);             /* Error code                  */

  if (error_flag) {
     printf("(QDBRTVFD) Ocorreram Erros.\n");
     exit(1);
  }
}
