/*******************************************************************/
/* Programa..: PSINC006                                          */
/* Objectivo.: Implementar Activadores                             */
/*******************************************************************/
          PGM

          DCL  VAR(&MODULO) TYPE(*CHAR) LEN(3) VALUE('PRD')
          DCL  VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
          DCL  VAR(&OBJLIB) TYPE(*CHAR) LEN(10)

          CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MODULO *CAT 'DBS')
          CHGVAR VAR(&OBJLIB) VALUE('#JM' *CAT &MODULO *CAT 'OBJ')

             /*******************************************************/
             /* ACTUALIZAR PROCESSOS DE SINISTRO AS400              */
             /*******************************************************/
             RMVPFTRG   FILE(QS36F/G.SINPRO) TRGTIME(*ALL) +
                          TRGEVENT(*ALL)
             ADDPFTRG   FILE(QS36F/G.SINPRO) TRGTIME(*AFTER) +
                          TRGEVENT(*INSERT) PGM(&OBJLIB/RSINT001) +
                          RPLTRG(*YES) TRG(ADD_GSINPRO)
             ADDPFTRG   FILE(QS36F/G.SINPRO) TRGTIME(*AFTER) +
                          TRGEVENT(*UPDATE) PGM(&OBJLIB/RSINT001) +
                          RPLTRG(*YES) TRG(CHG_GSINPRO)

             /*******************************************************/
             /* ACTUALIZAR INTERVENIENTES DO AS400                  */
             /*******************************************************/
             RMVPFTRG   FILE(QS36F/G.SININT) TRGTIME(*ALL) +
                          TRGEVENT(*ALL)
             ADDPFTRG   FILE(QS36F/G.SININT) TRGTIME(*AFTER) +
                          TRGEVENT(*INSERT) PGM(&OBJLIB/RSINT003) +
                          RPLTRG(*YES) TRG(ADD_GSININT)
             ADDPFTRG   FILE(QS36F/G.SININT) TRGTIME(*AFTER) +
                          TRGEVENT(*UPDATE) PGM(&OBJLIB/RSINT003) +
                          RPLTRG(*YES) TRG(CHG_GSININT)

             /*******************************************************/
             /* ACTUALIZAR INTERVENIENTES DC NO MS.ACCESS           */
             /*******************************************************/
/*        RMVPFTRG   FILE(&DBSLIB/FSINP102) TRGTIME(*ALL) +       */
/*                     TRGEVENT(*ALL)                             */
/*        ADDPFTRG   FILE(&DBSLIB/FSINP102) TRGTIME(*BEFORE) +    */
/*                     TRGEVENT(*INSERT) PGM(&OBJLIB/RSINT003) +  */
/*                     RPLTRG(*YES) TRG(ADD_FSINP102) +           */
/*                     ALWREPCHG(*YES)                            */
/*        ADDPFTRG   FILE(&DBSLIB/FSINP102) TRGTIME(*AFTER) +     */
/*                     TRGEVENT(*INSERT) PGM(&OBJLIB/RSINT003) +  */
/*                     RPLTRG(*YES) TRG(ADD_FSINP102)             */
/*        ADDPFTRG   FILE(&DBSLIB/FSINP102) TRGTIME(*AFTER) +     */
/*                     TRGEVENT(*DELETE) PGM(&OBJLIB/RSINT003) +  */
/*                     RPLTRG(*YES) TRG(DEL_FSINP102)             */

             /*******************************************************/
             /* ACTUALIZAR INTERVENIENTES DM NO MS.ACCESS           */
             /*******************************************************/
 /*       RMVPFTRG   FILE(&DBSLIB/FSINP103) TRGTIME(*ALL) +       */
 /*                    TRGEVENT(*ALL)                             */

             /*******************************************************/
             /* ACTUALIZAR INTERVENIENTES DM_DC NO MS.ACCESS        */
             /*******************************************************/
 /*       RMVPFTRG   FILE(&DBSLIB/FSINP104) TRGTIME(*ALL) +       */
 /*                    TRGEVENT(*ALL)                             */

ENDPGM
