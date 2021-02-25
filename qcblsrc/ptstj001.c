/**********************************************************************/
/**********************************************************************/
/*                                                                    */
/* Program:      List objects that adopt owner authority              */
/*                                                                    */
/* Language:     ILE C                                                */
/*                                                                    */
/* Description:  This program prints a report showing all objects     */
/*               that adopt owner authority.  The two parameters      */
/*               passed to the program are the profile to be          */
/*               checked and the type of objects to be listed.        */
/*               The parameter values are the same as those           */
/*               accepted by the QSYLOBJP API.                        */
/*                                                                    */
/* APIs Used:    QSYLOBJP - List Objects that Adopt Owner Authority   */
/*               QUSCRTUS - Create User Space                         */
/*               QUSPTRUS - Retrieve Pointer to User Space            */
/*               QUSROBJD - Retrieve Object Description               */
/*                                                                    */
/**********************************************************************/
/**********************************************************************/

#include <stdio.h>
#include <string.h>
#include <qsylobjp.h>    /* QSYLOBJP API Header                       */
#include <quscrtus.h>    /* QUSCRTUS API Header                       */
#include <qusptrus.h>    /* QUSPTRUS API Header                       */
#include <qusrobjd.h>    /* QUSROBJD API Header                       */
#include <qusgen.h>      /* Format Structures for User Space          */
#include <qusec.h>       /* Error Code Parameter Include for the APIs */
#include <qliept.h>      /* Entry Point Table Include                 */

/**********************************************************************/
/* Error Code Structure                                               */
/*                                                                    */
/* This shows how the user can define the variable length portion of  */
/* error code for the exception data.                                 */
/*                                                                    */
/**********************************************************************/
typedef struct {
     Qus_EC_t   ec_fields;
     char       Exception_Data[100];
        } error_code_t;

/**********************************************************************/
/* Global Variables                                                   */
/**********************************************************************/
char       api_name[10];
char       cont_hdl[20];
char       ext_attr[10];
char       list_status;
char       mbr_list[8];
char       obj_type[10];
char       rcvvar[8];
char       rjobd_fmt[8];
char       space_auth[10];
char       space_dmn[10];
char       space_init;
char       space_name[20];
char       space_rep[10];
char       space_text[50];
char       space_type[10];
char       usr_prf[10];
char       *usrspc_ptr, *usrspc_base;
int        rcvlen = 8;
int        size_entry;
int        space_size = 1;
error_code_t error_code;
FILE       *record;

/**********************************************************************/
/* Function:      done                                                */
/*                                                                    */
/* Description:   This function prints the end of listing print line  */
/*                and returns to the caller.                          */
/**********************************************************************/
void done()
{
  char command_string[32];

  fwrite("*** End of List",1, 15, record);
  fclose(record);
  exit();

} /* done */

/**********************************************************************/
/* Function:      apierr                                              */
/*                                                                    */
/* Description:   This function prints the API name, and exception    */
/*                identifier of an error that occurred.               */
/**********************************************************************/
void apierr()
{
  printf("API: %.10s\n", api_name);
  printf("Failed with exception: %.7s\n",
          error_code.ec_fields.Exception_Id);
  done();

} /* apierr */


/**********************************************************************/
/* Function:      getlst                                              */
/*                                                                    */
/* Description:   This function calls QSYLOBJP to build a list.       */
/*                                                                    */
/**********************************************************************/
void getlst()
{
  memcpy(mbr_list, "OBJP0200", 8);

  /********************************************************************/
  /* Call QSYLOBJP API to generate a list.  The continuation handle   */
  /* is set by the caller of this function.                           */
  /********************************************************************/
  QSYLOBJP(space_name,             /* User space and library          */
           mbr_list,               /* Member list                     */
           usr_prf,                /* User profile                    */
           obj_type,               /* Object type                     */
           cont_hdl,               /* Continuation handle   (3)          */
           &error_code);           /* Error code                      */

  /********************************************************************/
  /* Check for errors on QSYLOBJP.                                    */
  /********************************************************************/
  if(error_code.ec_fields.Bytes_Available > 0)
  {
    memcpy(api_name, "QSYLOBJP  ", 10);
    apierr();
  }

} /* getlst */

/**********************************************************************/
/* Function:      init                                                */
/*                                                                    */
/* Description:   This function does all the necessary initialization */
/*                for this program.                                   */
/**********************************************************************/
void init()
{
  memcpy(space_name, "ADOPTS    QTEMP     ", 20);
  space_init = 0x00;
  memcpy(mbr_list, "OBJP0200", 8);
  memcpy(rjobd_fmt, "OBJD0100", 8);
  memcpy(space_type, "*USRSPC   ", 10);
  memcpy(ext_attr, "QSYLOBJP  ", 10);
  memcpy(space_auth, "*ALL      ", 10);
  memcpy(space_rep, "*YES      ", 10);
  memcpy(space_dmn, "*USER     ", 10);

  /********************************************************************/
  /* Open QPRINT file so that data can be written to it.  If the file */
  /* cannot be opened, print a message and exit.                      */
  /********************************************************************/
  if((record = fopen("QPRINT", "wb, lrecl=132, type=record")) == NULL)
  {
    printf("File could not be opened\n");
    exit(1);
  }

  error_code.ec_fields.Bytes_Provided = sizeof(error_code_t);

  /********************************************************************/
  /* Call QUSROBJD to see if the user space was previously created in */
  /* QTEMP.  If it was, simply reuse it.                              */
  /********************************************************************/
  QUSROBJD(rcvvar,                  /* Receiver variable              */
           rcvlen,                  /* Receiver variable length       */
           rjobd_fmt,               /* Format                         */
           space_name,              /* User space name and library    */
           space_type,              /* User object type               */
           &error_code);            /* Error code                     */

  if(error_code.ec_fields.Bytes_Available > 0)
  {
    /******************************************************************/
    /* If a CPF9801 error was received, then the user space was not   */
    /* found.                                                         */
    /******************************************************************/
    if(memcmp(error_code.ec_fields.Exception_Id, "CPF9801", 7) == 0)
    {
      /****************************************************************/
      /* Create a user space for the list generated by QSYLOBJP.      */
      /****************************************************************/
      QUSCRTUS(space_name,          /* User space name and library    */
               ext_attr,            /* Extended attribute             */
               space_size,          /* Size of the user space         */
               &space_init,         /* Space initialization           */
               space_auth,          /* Public authority to user space */
               space_text,          /* User space text                */
               space_rep,           /* Replace existing user space?   */
               &error_code,         /* Error Code                     */
               space_dmn);          /* Domain of created user space   */

      /****************************************************************/
      /* Check for errors on QUSCRTUS.                                */
      /****************************************************************/
      if(error_code.ec_fields.Bytes_Available > 0)
      {
         memcpy(api_name, "QUSCRTUS  ", 10);
         apierr();
      }
    }
    /******************************************************************/
    /* An error occurred accessing the user space.                    */
    /******************************************************************/
    else
    {
      memcpy(api_name, "QUSRJOBD  ", 10);
      apierr();
    }
  }

    /******************************************************************/
    /* Set QSYLOBJP (via GETLST) to start a new list.                 */
    /******************************************************************/
    memset(cont_hdl, ' ', 20);
    getlst();

    /******************************************************************/
    /* Get a resolved pointer to the user space for performance.      */
    /******************************************************************/
    QUSPTRUS(space_name,            /* User space name and library    */
             &usrspc_ptr,           /* User space pointer             */
             &error_code);          /* Error Code                     */

    /******************************************************************/
    /* Check for errors on QUSPTRUS.                                  */
    /******************************************************************/
    if(error_code.ec_fields.Bytes_Available > 0)
    {
      memcpy(api_name, "QUSPTRUS  ", 10);
      apierr();
    }

    usrspc_base = usrspc_ptr;

} /* init */

/**********************************************************************/
/* Function:      proces2                                             */
/*                                                                    */
/* Description:   This function processes each entry returned by      */
/*                QSYLOBJP.                                           */
/*                                                                    */
/**********************************************************************/
void proces2()
{
  char obj_type[112];

  sprintf(obj_type, "Object: %.10s Library: %.10s Type: %.10s Text: %.50s\n",
         ((Qsy_OBJP0200_List_T *)usrspc_ptr)->Object.Name,
         ((Qsy_OBJP0200_List_T *)usrspc_ptr)->Object.Library,
         ((Qsy_OBJP0200_List_T *)usrspc_ptr)->Object_Type,
         ((Qsy_OBJP0200_List_T *)usrspc_ptr)->Object_Text);
  fwrite(obj_type, 1, 112, record);

  /********************************************************************/
  /* After each entry, increment to the next entry.                   */
  /********************************************************************/
  usrspc_ptr += size_entry;                                (7)

} /* proces2 */

/**********************************************************************/
/* Function:      proces1                                             */
/*                                                                    */
/* Description:   This function processes each entry returned by      */
/*                QSYLOBJP.                                           */
/*                                                                    */
/**********************************************************************/
void proces1()
{
  int i;
  int num_entries;
  int offset;

  num_entries = ((Qus_Generic_Header_0100_t *)\
                  usrspc_ptr)->Number_List_Entries;

  /********************************************************************/
  /* If valid information was returned.     (1)                          */
  /********************************************************************/
  if((((Qus_Generic_Header_0100_t *)usrspc_ptr)->Information_Status == 'C') ||
     (((Qus_Generic_Header_0100_t *)usrspc_ptr)->Information_Status == 'P'))
  {
    if(num_entries > 0)
    {
      /****************************************************************/
      /* Get the size of each entry to use later.  (4)                */
      /****************************************************************/
      size_entry = ((Qus_Generic_Header_0100_t *)usrspc_ptr)->Size_Each_Entry;

      /****************************************************************/
      /* Increment to the first list entry.                           */
      /****************************************************************/
      offset = ((Qus_Generic_Header_0100_t *)usrspc_ptr)->Offset_List_Data; (5)
      usrspc_ptr += offset;

      /****************************************************************/
      /* Process all of the entries.                                  */
      /****************************************************************/
      for(i=0; i<num_entries; i++)                         (6)
        proces2();

      /****************************************************************/
      /* Reset the user space pointer to the beginning.               */
      /****************************************************************/
      usrspc_ptr = usrspc_base;

      /****************************************************************/
      /* If all entries in this user space have been processed, check */
      /* if more entries exist than can fit in one user space.        */
      /****************************************************************/
      if(((Qus_Generic_Header_0100_t *)usrspc_ptr)->Information_Status == 'P')
      {
        /**************************************************************/
        /* Address the input parameter header.                        */
        /**************************************************************/
        offset = ((Qus_Generic_Header_0100_t *)\
                   usrspc_ptr)->Offset_Input_Parameter;
        usrspc_ptr += offset;

        /**************************************************************/
        /* If the continuation handle in the input parameter header   */
        /* is blank, then set the list status to complete.
        /**************************************************************/
        if(memcmp(((Qsy_OBJP_Input_T *)usrspc_ptr)->Continuation_Handle,
           "                    ", 20) == 0)
        {
          list_status = 'C';
        }
        else
        /**************************************************************/
        /* Else, call QSYLOBJP reusing the user space to get more     */
        /* list entries.                                              */
        /**************************************************************/
        {
          memcpy(cont_hdl, ((Qsy_OBJP_Input_T *)\
                 usrspc_ptr)->Continuation_Handle, 20);    (2)
          getlst();
          list_status = ((Qus_Generic_Header_0100_t *)\
                          usrspc_ptr)->Information_Status;
        }
      }
    }
    else
    /******************************************************************/
    /* If there exists an unexpected status, log an error (not shown) */
    /* and exit.                                                      */
    /******************************************************************/
    {
      done();
    }
  }
} /* proces1 */

/**********************************************************************/
/* Function:      proces                                              */
/*                                                                    */
/* Description:   Processes entries until they are complete.          */
/*                                                                    */
/**********************************************************************/
void proces()
{
  list_status = ((Qus_Generic_Header_0100_t *)usrspc_ptr)->Information_Status;

  do
  {
    proces1();
  } while (list_status != 'C');

} /* proces */

/**********************************************************************/
/* main                                                               */
/**********************************************************************/

main(int argc, char *argv[])
{
  /********************************************************************/
  /* Make sure we received the correct number of parameters. The argc */
  /* parameter will contain the number of parameters that was passed  */
  /* to this program. This number also includes the program itself,   */
  /* so we need to evaluate argc-1.                                   */
  /********************************************************************/

  if (((argc - 1) < 2) || ((argc - 1 > 2)))
  /********************************************************************/
  /* We did not receive all of the required parameters so exit the    */
  /* program.                                                         */
  /********************************************************************/
  {
    exit(1);
  }
  else
  /********************************************************************/
  /* Copy parameters into local variables.                            */
  /********************************************************************/
  {
    memcpy(usr_prf, argv[1], 10);
    memcpy(obj_type, argv[2], 10);
  }

  init();
  proces();
  done();

} /* main */
