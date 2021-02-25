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

          /*******************************************************/
          /* ACTUALIZAR TABELA DE COBERTURAS          [FPRPD008] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FCATP114) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FCATP114) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RCATT114) +
                       RPLTRG(*YES) TRG(INS_CAT114) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FCATP114) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RCATT114) +
                       RPLTRG(*YES) TRG(CHG_CAT114) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FCATP114) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RCATT114) +
                       RPLTRG(*YES) TRG(DEL_CAT114)

          ENDPGM
