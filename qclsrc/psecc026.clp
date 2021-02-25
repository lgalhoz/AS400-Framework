/******************************************************************************/
/*                                                                            */
/*  FTP SERVER REQUEST VALIDATION EXIT PROGRAM FOR WINSPOOL/400.              */
/*                                                                            */
/*  ADDITIONAL NOTES:                                                         */
/*  1. WHEN THE APPLICATION ID IS 1 (FTP SERVER) AND THE OPERATION ID IS      */
/*     0 (SESSION INITIALIZATION), THE JOB IS RUNNING UNDER THE QTCP          */
/*     USER PROFILE WHEN THE EXIT PROGRAM IS CALLED.  IN ALL OTHER CASES,     */
/*     THE JOB IS RUNNING UNDER THE USER'S PROFILE.                           */
/*  2. IT IS HIGHLY RECOMMENDED THAT THE EXIT PROGRAM BE CREATED IN A LIBRARY */
/*     WITH *PUBLIC AUTHORITY SET TO *EXCLUDE, AND THAT THE EXIT PROGRAM      */
/*     ITSELF BE GIVEN A *PUBLIC AUTHORITY OF *EXCLUDE.  THE FTP SERVER       */
/*     ADOPTS THE AUTHORITY NECESSARY TO CALL THE EXIT PROGRAM.               */
/*  3. IT IS POSSIBLE TO USE THE SAME EXIT PROGRAM FOR BOTH THE FTP CLIENT    */
/*     AND SERVER REQUEST VALIDATION EXIT POINTS.  HOWEVER, THIS PROGRAM      */
/*     DOES NOT TAKE THE CLIENT CASE INTO ACCOUNT.                            */
/*                                                                            */
/******************************************************************************/
TSTREQCL:   PGM        PARM(&APPIDIN &OPIDIN &USRPRF &IPADDRIN +
                         &IPLENIN &OPINFOIN &OPLENIN &ALLOWOP)

 /* DECLARE INPUT PARAMETERS */
       DCL        VAR(&APPIDIN) TYPE(*CHAR) LEN(4) /* +
                    APPLICATION ID                     */
       DCL        VAR(&OPIDIN) TYPE(*CHAR) LEN(4) /* OPERATION +
                    ID                       */
       DCL        VAR(&USRPRF) TYPE(*CHAR) LEN(10) /* USER +
                    PROFILE                       */
       DCL        VAR(&IPADDRIN) TYPE(*CHAR) /* REMOTE IP +
                    ADDRESS                  */
       DCL        VAR(&IPLENIN) TYPE(*CHAR) LEN(4) /* LENGTH +
                    OF IP ADDRESS               */
       DCL        VAR(&OPLENIN) TYPE(*CHAR) LEN(4) /* LENGTH +
                    OF OPERATION-SPECIFIC INFO. */
       DCL        VAR(&OPINFOIN) TYPE(*CHAR) LEN(9999) /* +
                    OPERATION-SPECIFIC INFORMATION     */
       DCL        VAR(&ALLOWOP) TYPE(*CHAR) LEN(4) /* ALLOW +
                    (OUTPUT) */

 /* DECLARE LOCAL COPIES OF PARAMETERS (IN FORMAT USABLE BY CL) */
       DCL        VAR(&APPID) TYPE(*DEC) LEN(1 0)
       DCL        VAR(&OPID) TYPE(*DEC) LEN(1 0)
       DCL        VAR(&IPLEN) TYPE(*DEC) LEN(5 0)
       DCL        VAR(&IPADDR) TYPE(*CHAR)
       DCL        VAR(&OPLEN) TYPE(*DEC) LEN(5 0)
       DCL        VAR(&OPINFO) TYPE(*CHAR) LEN(9999)
       DCL        VAR(&PATHNAME) TYPE(*CHAR) LEN(9999) /* +
                    UPPERCASE PATH NAME               */

 /* DECLARE VALUES FOR ALLOW(1) AND NOALLOW(0) */
            DCL        VAR(&ALLOW)  TYPE(*DEC) LEN(1 0) VALUE(1)
            DCL        VAR(&NOALLOW)  TYPE(*DEC) LEN(1 0) VALUE(0)

  /* DECLARE REQUEST CONTROL BLOCK FOR QLGCNVCS (CONVERT CASE) API: */
  /* CONVERT TO UPPERCASE BASED ON JOB CCSID  */
             DCL        VAR(&CASEREQ)  TYPE(*CHAR) LEN(22) +
                          VALUE(X'00000001000000000000000000000000000+
                          000000000')
             DCL        VAR(&ERROR)  TYPE(*CHAR) LEN(4) +
                          VALUE(X'00000000')
  /* ON UNHANDLED ERRORS, EXIT PROGRAM AND REFUSE FTP OPERATION */
        MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERRORS))

  /* ASSIGN INPUT PARAMETERS TO LOCAL COPIES */
             CHGVAR     VAR(&APPID)  VALUE(%BINARY(&APPIDIN))
             CHGVAR     VAR(&OPID)  VALUE(%BINARY(&OPIDIN))
             CHGVAR     VAR(&IPLEN)  VALUE(%BINARY(&IPLENIN))
             CHGVAR     VAR(&IPADDR)  VALUE(%SUBSTRING(&IPADDRIN 1 &IPLEN))
             CHGVAR     VAR(&OPLEN)  VALUE(%BINARY(&OPLENIN))

/* HANDLE OPERATION SPECIFIC INFORMATION FIELD (WHICH IS VARIABLE   */
 /* LENGTH                                                           */
            IF         COND(&OPLEN = 0) THEN(CHGVAR VAR(&OPINFO)  +
                         VALUE(' '))
            ELSE       CMD(CHGVAR VAR(&OPINFO)  VALUE(%SST(&OPINFOIN +
                         1 &OPLEN)))

 /* OPERATION ID 0 (INCOMING CONNECTION): REJECT IF CONNECTION IS COMING        */
 /* THROUGH INTERFACE 9.8.7.6, ACCEPT OTHERWISE.  (THE ADDRESS IS JUST AN       */
 /* EXAMPLE.)  THIS CAPABILITY COULD BE USED TO ONLY ALLOW INCOMING CONNECTIONS */
 /* FROM AN INTERNAL NETWORK AND REJECT THEM FROM THE "REAL" INTERNET, IF       */
 /* THE CONNECTION TO THE INTERNET WERE THROUGH A SEPARATE IP INTERFACE.        */
 /* NOTE: FOR FTP SERVER, OPERATION 0 IS ALWAYS UNDER QTCP PROFILE.             */
            IF         COND(&OPID = 0) THEN(DO)
              IF       COND(&OPINFO = '9.8.7.6') THEN(CHGVAR +
                         VAR(%BINARY(&ALLOWOP))  VALUE(&NOALLOW))
              ELSE     CMD(CHGVAR VAR(%BINARY(&ALLOWOP))  +
                         VALUE(&ALLOW))
              GOTO     CMDLBL(END)
            ENDDO

 /* REJECT DIRECTORY/LIBRARY DELETION OPERATION */
            IF     COND(&OPID = 2) THEN(DO)
                   CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&NOALLOW)
                   RETURN
            ENDDO

 /* REJECT CHANGE DIRECTORY OPERATION */
            IF     COND(&OPID = 3) THEN(DO)
                   CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&NOALLOW)
                   RETURN
            ENDDO

 /* REJECT LIST DIRECTORY OPERATIONS */
            IF     COND(&OPID = 4) THEN(DO)
                   CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&NOALLOW)
                   RETURN
            ENDDO

 /* REJECT FILE DELETION OPERATION */
            IF     COND(&OPID = 5) THEN(DO)
                    CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&NOALLOW)
                    RETURN
             ENDDO

  /* REJECT RECEIVE FILE FROM SERVER OPERATION */
  /* EXCEPT FOR THE WINSPOOL/4000 WORK FILE    */
             IF     COND(&OPID = 6) THEN(DO)

                    /* IF FILE IS WINOUTQ IN QTEMP, ALLOW DOWNLOAD */
                    IF         COND(&OPINFO *EQ +
                       '/QSYS.LIB/QTEMP.LIB/WINOUTQ.FILE') THEN(DO)
                        CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&ALLOW)
                    RETURN
                    ENDDO

                    /* IF FILE NOT WINOUTQ IN QTEMP, REJECT DOWNLOAD */
                    CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&NOALLOW)
                    RETURN

             ENDDO

  /* REJECT SEND FILE TO SERVER OPERATIONS */
             IF     COND(&OPID = 7) THEN(DO)
                    CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&NOALLOW)
                    RETURN
             ENDDO

  /* REJECT RENAME FILE OPERATIONS */
             IF     COND(&OPID = 8) THEN(DO)
                    CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&NOALLOW)
                    RETURN
             ENDDO

  /* ALLOW WINSPOOL/400 CL COMMAND EXECUTION OPERATIONS */
              IF     COND(&OPID = 9) THEN(DO)
         SNDMSG     MSG(&OPINFO) TOUSR(RSCHOEN)
                     /* IF COMMAND IS A WINSPOOL COMMAND, ALLOW IT */
                     IF         COND(%SST(&OPINFO 1 16) *EQ +
                        'WINSPOOL/WSPL016') THEN(DO)
                         CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&ALLOW)
                     RETURN
                     ENDDO

                     IF         COND(%SST(&OPINFO 1 16) *EQ +
                        'WINSPOOL/WSPL019') THEN(DO)
                         CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&ALLOW)
                     RETURN
                     ENDDO

             IF         COND(%SST(&OPINFO 1 16) *EQ +
                        'WINSPOOL/WSPL020') THEN(DO)
                         CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&ALLOW)
                     RETURN
                     ENDDO

                     IF         COND(%SST(&OPINFO 1 16) *EQ +
                        'WINSPOOL/WSPL021') THEN(DO)
                         CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&ALLOW)
                     RETURN
                     ENDDO

                     /* REJECT ALL NON-WINSPOOL/400 RELATED COMMANDS */
                     CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&NOALLOW)
                     RETURN
              ENDDO

  /***************************************************/
  /* NORMAL END OF PROGRAM                           */
  /***************************************************/
              RETURN

  /***************************************************/
  /* IF UNHANDLED ERRORS OCCUR, REFUSE FTP OPERATION */
  /***************************************************/
  ERRORS:     CHGVAR     VAR(%BINARY(&ALLOWOP)) VALUE(&NOALLOW)

  END:        ENDPGM
