/*******************************************************************/
/* Programa..: PPRDC007                                          */
/* Objectivo.: Implementar Activadores                             */
/*******************************************************************/
          PGM

          DCL  VAR(&MODULO) TYPE(*CHAR) LEN(3) VALUE('PRD')
          DCL  VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
          DCL  VAR(&OBJLIB) TYPE(*CHAR) LEN(10)

          CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MODULO *CAT 'DBS')
          CHGVAR VAR(&OBJLIB) VALUE('#JM' *CAT &MODULO *CAT 'OBJ')

          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDT001) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP003) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP008) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP009) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP014) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP011) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP016) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP018) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP010) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP012) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP013) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP015) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP017) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP022) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)

          ENDPGM
