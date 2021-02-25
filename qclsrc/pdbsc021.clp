/*******************************************************************/
/* Programa..: PDBSC020                                            */
/* Objectivo.: Actualizar o programa de Serviço                    */
/*******************************************************************/
/* Observação: Executar sempre que realizar alguma alteração ou    */
/*             inclusão de um novo módulo.                         */
/*             Os programa tem de ser compilados para reflectir as */
/*             novas alterações.                                   */
/*******************************************************************/
             PGM
             DCL  VAR(&MOD) TYPE(*CHAR) LEN(3) VALUE('PRD')
             DCL  VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
             DCL  VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
             DCL  VAR(&SRCLIB) TYPE(*CHAR) LEN(10)

             CHGVAR VAR(&OBJLIB) VALUE('#JM' *CAT &MOD *CAT 'OBJ')
             CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MOD *CAT 'DBS')
             CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &MOD *CAT 'SRC')


             /****************************************************************/
             /* VALIDAR SE EXISTE O DIRECTORIO                               */
             /****************************************************************/
             RTVOBJD    OBJ(&OBJLIB/JMDIR) OBJTYPE(*BNDDIR)
             MONMSG     MSGID(CPF9812 CPF9801) EXEC(DO)
             CRTBNDDIR  BNDDIR(&OBJLIB/JMDIR)
             ENDDO

             /******************************************************/
             /* Gerar os Módulos.                                  */
             /******************************************************/
             CRTRPGMOD  MODULE(&OBJLIB/RGRLO011) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTSQLRPGI OBJ(&OBJLIB/RDBSO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          OBJTYPE(*MODULE) OUTPUT(*NONE) +
                          REPLACE(*YES) DBGVIEW(*SOURCE) +
                          SRTSEQ(*LANGIDSHR)

             CRTRPGMOD  MODULE(&OBJLIB/RDBSO011) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RDBSO012) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RDBSO013) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SDBSE022) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST) RSTDSP(*YES)
             CRTRPGMOD  MODULE(&OBJLIB/RDBSO022) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Gerar a biblioteca (Programa de Serviço)           */
             /******************************************************/
             RMVBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCDBS))
             DLTSRVPGM  SRVPGM(&OBJLIB/PRCDBS)
             MONMSG CPF2105
             CRTSRVPGM  SRVPGM(&OBJLIB/PRCDBS) +
                          MODULE(&OBJLIB/RGRLO011 &OBJLIB/RDBSO010 +
                          &OBJLIB/RDBSO011 &OBJLIB/RDBSO012 +
                          &OBJLIB/RDBSO013 &OBJLIB/RDBSO022) +
                           EXPORT(*ALL)
             ADDBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCDBS))
             ENDPGM
