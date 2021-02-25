/****************************************************************/
/*   INCLUDE NAME : TRGBUF                                      */
/*                                                              */
/*   DESCRIPTION  : THE INPUT TRIGGER BUFFER STRUCTURE FOR THE  */
/*                  USER'S TRIGGER PROGRAM.                     */
/*                                                              */
/*   LANGUAGE     : ILE C                                       */
/*                                                              */
/****************************************************************/
/****************************************************************/
/*   NOTE: THE FOLLOWING TYPE DEFINITION ONLY DEFINES THE FIXED */
/*         PORTION OF THE FORMAT. THE DATA AREA OF THE ORIGINAL */
/*         RECORD, NULL BYTE MAP OF THE ORIGINAL RECORD, THE    */
/*         NEW RECORD, AND THE NULL BYTE MAP OF THE NEW RECORD  */
/*         IS VARYING LENGTH AND IMMEDIATELY FOLLOWS WHAT IS    */
/*         DEFINED HERE.                                        */
/****************************************************************/
 TYPEDEF _PACKED STRUCT QDB_TRIGGER_BUFFER {
           CHAR  FILE_NAME[10];
           CHAR  LIBRARY_NAME[10];
           CHAR  MEMBER_NAME[10];
           CHAR  TRIGGER_EVENT[1];
           CHAR  TRIGGER_TIME[1];
           CHAR  COMMIT_LOCK_LEVEL[1];
           CHAR  RESERVED_1[3];
           INT   DATA_AREA_CCSID;
           CHAR  RESERVED_2]8];
           INT   OLD_RECORD_OFFSET;
           INT   OLD_RECORD_LEN;
           INT   OLD_RECORD_NULL_BYTE_MAP;
           INT   OLD_RECORD_NULL_BYTE_MAP_LEN;
           INT   NEW_RECORD_OFFSET;
           INT   NEW_RECORD_LEN;
           INT   NEW_RECORD_NULL_BYTE_MAP;
           INT   NEW_RECORD_NULL_BYTE_MAP_LEN;
      } QDB_TRIGGER_BUFFER_T;
