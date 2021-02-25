 /******************************************************************/
 /*   INCLUDE NAME : MSGHANDLER                                    */
 /*                                                                */
 /*   DESCRIPTION  : Message handler to signal an exception message*/
 /*                  to the caller of this trigger program.        */
 /*                                                                */
 /*   Note: This message handler is a user defined routine.        */
 /*                                                                */
 /******************************************************************/
 #include <stdio.h>
 #include <stdlib.h>
 #include <recio.h>
 #include <leawi.h>

 #pragma linkage (QMHSNDPM, OS)
 void QMHSNDPM(char *,             /* Message identifier             */
               void *,             /* Qualified message file name    */
               void *,             /* Message data or text           */
               int,                /* Length of message data or text */
               char *,             /* Message type                   */
               char *,             /* Call message queue             */
               int,                /* Call stack counter             */
               void *,             /* Message key                    */
               void *,             /* Error code                     */
               ...);               /* Optionals:
                                        length of call message queue
                                         name
                                        Call stack entry qualification
                                        display external messages
                                         screen wait time            */
/*********************************************************************/
 /********  This is the start of the exception handler function.      */
 /*********************************************************************/
   void main_handler(_FEEDBACK *cond, _POINTER *token, _INT4 *rc,
                     _FEEDBACK *new) {
                             /****************************************/
                             /* Initialize variables for call to     */
                             /* QMHSNDPM.                            */
                             /* User defines any message ID and      */
                             /* message file for the following data  */
                             /****************************************/
   char      message_id[7] = "TRG9999";
   char      message_file[20] = "MSGF      LIB1      ";
   char      message_data[50] = "Trigger error               ";
   int       message_len = 30;
   char      message_type[10] = "*ESCAPE   ";
   char      message_q[10] = "_C_pep    ";
   int       pgm_stack_cnt = 1;
   char      message_key[4];
                              /****************************************/
                              /* Declare error code structure for     */
                              /* QMHSNDPM.                            */
                              /****************************************/
   struct error_code {
     int bytes_provided;
     int bytes_available;
     char message_id[7];
   } error_code;

   error_code.bytes_provided = 15;
                              /****************************************/
                              /* Set the error handler to resume and  */
                              /* mark the last escape message as      */
                              /* handled.                             */
                              /****************************************/
   *rc = CEE_HDLR_RESUME;
                              /****************************************/
                              /* Send my own *ESCAPE message.         */
                              /****************************************/
   QMHSNDPM(message_id,
            &message_file,
            &message_data,
            message_len,
            message_type,
            message_q,
            pgm_stack_cnt,
            &message_key,
            &error_code );
                              /****************************************/
                              /* Check that the call to QMHSNDPM      */
                              /* finished correctly.                  */
                              /****************************************/
  if (error_code.bytes_available != 0) {
        printf("Error in QMHOVPM : %s\n", error_code.message_id);
      }
   }
