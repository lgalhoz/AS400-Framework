/*******************************************************************/
/* Programa..: PSECC020                                            */
/* Objectivo.: Actualizar o programa de Serviço de Segurança,      */
/*             junto com a informação dos Módulos.                 */
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
             CRTSQLRPGI OBJ(&OBJLIB/RSECO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          OBJTYPE(*MODULE) OUTPUT(*NONE) +
                          REPLACE(*YES) DBGVIEW(*SOURCE)
             CRTDSPF    FILE(&OBJLIB/SSECE011) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOLIST *NOSOURCE) RSTDSP(*YES)
             CRTRPGMOD  MODULE(&OBJLIB/RSECO011) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)
             CRTDSPF    FILE(&OBJLIB/SSECE012) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOLIST *NOSOURCE) RSTDSP(*YES)
             CRTRPGMOD  MODULE(&OBJLIB/RSECO012) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RSECO014) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RSECO017) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SGRLE025) +
                          SRCFILE(&SRCLIB/QDDSSRC) SRCMBR(SGRLE025) +
                          OPTION(*NOSRC *NOLIST)
             CRTRPGMOD  MODULE(&OBJLIB/RGRLO025) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             /******************************************************/
             /* Gerar a biblioteca (Programa de Serviço)           */
             /******************************************************/
             RMVBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCSEC))
             DLTSRVPGM  SRVPGM(&OBJLIB/PRCSEC)
             MONMSG CPF2105
             CRTSRVPGM  SRVPGM(&OBJLIB/PRCSEC) +
                          MODULE(&OBJLIB/RSECO010 &OBJLIB/RSECO011 +
                          &OBJLIB/RSECO012 &OBJLIB/RSECO014 +
                          &OBJLIB/RSECO017 +
                          &OBJLIB/RGRLO025) EXPORT(*ALL) +
                          BNDDIR(&OBJLIB/JMDIR)

             /******************************************************/
             /* Actualiza directório de Funções                    */
             /******************************************************/
             ADDBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCSEC))
             ENDPGM
