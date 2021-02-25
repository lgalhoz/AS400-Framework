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
          /* ACTUALIZAR INVENTÁRIO APÓLICES           [FPRPT001] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDT001) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDT001) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT001) +
                       RPLTRG(*YES) TRG(INS_PRD001) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDT001) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT001) +
                       RPLTRG(*YES) TRG(CHG_PRD001) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDT001) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT001) +
                       RPLTRG(*YES) TRG(DEL_PRD001)

          /*******************************************************/
          /* ACTUALIZAR TABELA AUTOMOVEL              [FPRPD003] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP003) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP003) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT003) +
                       RPLTRG(*YES) TRG(INS_PRD003) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP003) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT003) +
                       RPLTRG(*YES) TRG(CHG_PRD003) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP003) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT003) +
                       RPLTRG(*YES) TRG(DEL_PRD003)

          /*******************************************************/
          /* ACTUALIZAR TABELA DE COBERTURAS          [FPRPD008] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP008) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP008) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT008) +
                       RPLTRG(*YES) TRG(INS_PRD008) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP008) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT008) +
                       RPLTRG(*YES) TRG(CHG_PRD008) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP008) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT008) +
                       RPLTRG(*YES) TRG(DEL_PRD008)

          /*******************************************************/
          /* ACTUALIZAR TABELA DE CLAUSULAS E CONDIÇÕE[FPRDP009] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP009) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP009) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT009) +
                       RPLTRG(*YES) TRG(INS_PRD009) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP009) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT009) +
                       RPLTRG(*YES) TRG(CHG_PRD009) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP009) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT009) +
                       RPLTRG(*YES) TRG(DEL_PRD009)

          /*******************************************************/
          /* ACTUALIZAR TABELA DE LIMITES E FRANQUIAS [FPRDP014] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP014) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP014) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT014) +
                       RPLTRG(*YES) TRG(INS_PRD014) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP014) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT014) +
                       RPLTRG(*YES) TRG(CHG_PRD014) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP014) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT014) +
                       RPLTRG(*YES) TRG(DEL_PRD014)

          /*******************************************************/
          /* ACTUALIZAR TABELA DE RISCOS              [FPRDP011] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP011) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP011) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT011) +
                       RPLTRG(*YES) TRG(INS_PRD011) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP011) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT011) +
                       RPLTRG(*YES) TRG(CHG_PRD011) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP011) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT011) +
                       RPLTRG(*YES) TRG(DEL_PRD011)

          /*******************************************************/
          /* ACTUALIZAR TABELA ESTADO CONSERVAÇÃO     [FPRDP016] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP016) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP016) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT016) +
                       RPLTRG(*YES) TRG(INS_PRD016) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP016) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT016) +
                       RPLTRG(*YES) TRG(CHG_PRD016) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP016) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT016) +
                       RPLTRG(*YES) TRG(DEL_PRD016)

          /*******************************************************/
          /* ACTUALIZAR TABELA PARAMETROS EDIFICIO    [FPRDP018] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP018) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP018) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT018) +
                       RPLTRG(*YES) TRG(INS_PRD018) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP018) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT018) +
                       RPLTRG(*YES) TRG(CHG_PRD018) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP018) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT018) +
                       RPLTRG(*YES) TRG(DEL_PRD018)

          /*******************************************************/
          /* ACTUALIZAR TABELA NOTA DESCRITIVA        [FPRDP010] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP010) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP010) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT010) +
                       RPLTRG(*YES) TRG(INS_PRD010) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP010) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT010) +
                       RPLTRG(*YES) TRG(CHG_PRD010) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP010) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT010) +
                       RPLTRG(*YES) TRG(DEL_PRD010)

          /*******************************************************/
          /* ACTUALIZAR TABELA TEXTO NOTA DESCRITIVA  [FPRDP012] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP012) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP012) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT012) +
                       RPLTRG(*YES) TRG(INS_PRD012) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP012) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT012) +
                       RPLTRG(*YES) TRG(CHG_PRD012) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP012) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT012) +
                       RPLTRG(*YES) TRG(DEL_PRD012)

          /*******************************************************/
          /* ACTUALIZAR TABELA DETALHE HABITAÇÃO (ND) [FPRDP013] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP013) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP013) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT013) +
                       RPLTRG(*YES) TRG(INS_PRD013) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP013) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT013) +
                       RPLTRG(*YES) TRG(CHG_PRD013) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP013) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT013) +
                       RPLTRG(*YES) TRG(DEL_PRD013)

          /*******************************************************/
          /* ACTUALIZAR TABELA DETALHE CONDOMINIO(ND) [FPRDP015] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP015) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP015) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT015) +
                       RPLTRG(*YES) TRG(INS_PRD015) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP015) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT015) +
                       RPLTRG(*YES) TRG(CHG_PRD015) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP015) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT015) +
                       RPLTRG(*YES) TRG(DEL_PRD015)

          /*******************************************************/
          /* ACTUALIZAR TABELA DETALHE CONDOMINIO(ND) [FPRDP017] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP017) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP017) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT017) +
                       RPLTRG(*YES) TRG(INS_PRD017) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP017) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT017) +
                       RPLTRG(*YES) TRG(CHG_PRD017) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP017) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT017) +
                       RPLTRG(*YES) TRG(DEL_PRD017)

          /*******************************************************/
          /* ACTUALIZAR TABELA DE CLAUSULAS E CONDIÇÕE[FPRDP009] */
          /*******************************************************/
          MONMSG     MSGID(CPF32C6 CPD32E7 CPF9C32)
          RMVPFTRG   FILE(&DBSLIB/FPRDP022) TRGTIME(*ALL) +
                       TRGEVENT(*ALL)
          ADDPFTRG   FILE(&DBSLIB/FPRDP022) TRGTIME(*BEFORE) +
                       TRGEVENT(*INSERT) PGM(&OBJLIB/RPRDT022) +
                       RPLTRG(*YES) TRG(INS_PRD022) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP022) TRGTIME(*BEFORE) +
                       TRGEVENT(*UPDATE) PGM(&OBJLIB/RPRDT022) +
                       RPLTRG(*YES) TRG(CHG_PRD022) ALWREPCHG(*YES)
          ADDPFTRG   FILE(&DBSLIB/FPRDP022) TRGTIME(*AFTER) +
                       TRGEVENT(*DELETE) PGM(&OBJLIB/RPRDT022) +
                       RPLTRG(*YES) TRG(DEL_PRD022)

          ENDPGM
