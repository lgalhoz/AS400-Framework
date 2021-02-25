/*******************************************************************/
/* Programa..: PTBLC020                                            */
/* Objectivo.: Actualizar o programa de Servi�o de Tabelas         */
/*             Gerais.                                             */
/*******************************************************************/
/* Observa��o: Executar sempre que realizar alguma altera��o ou    */
/*             inclus�o de um novo m�dulo.                         */
/*             Os programa tem de ser compilados para reflectir as */
/*             novas altera��es.                                   */
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
             /* Gerar os M�dulos.                                  */
             /******************************************************/
             CRTSQLRPGI OBJ(&OBJLIB/RTBLO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          OBJTYPE(*MODULE) OUTPUT(*NONE) +
                          REPLACE(*YES) DBGVIEW(*SOURCE)
             CRTSQLRPGI OBJ(&OBJLIB/RTBLO011) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          OBJTYPE(*MODULE) OUTPUT(*NONE) +
                          REPLACE(*YES) DBGVIEW(*SOURCE)
             CRTDSPF    FILE(&OBJLIB/STBLE012) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST) RSTDSP(*YES)
             CRTRPGMOD  MODULE(&OBJLIB/RTBLO012) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)
             CRTDSPF    FILE(&OBJLIB/STBLE013) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST) RSTDSP(*YES)
             CRTRPGMOD  MODULE(&OBJLIB/RTBLO013) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RTBLO014) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SDBSE022) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST) RSTDSP(*YES)
             CRTRPGMOD  MODULE(&OBJLIB/RDBSO022) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)
             /******************************************************/
             /* Gerar a biblioteca (Programa de Servi�o)           */
             /******************************************************/
             RMVBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCTBL))
             DLTSRVPGM  SRVPGM(&OBJLIB/PRCTBL)
             MONMSG CPF2105
             CRTSRVPGM  SRVPGM(&OBJLIB/PRCTBL) +
                          MODULE(&OBJLIB/RTBLO010 &OBJLIB/RTBLO011 +
                          &OBJLIB/RTBLO012 &OBJLIB/RTBLO013 +
                          &OBJLIB/RTBLO014 RDBSO022) +
                          EXPORT(*ALL) +
                          BNDDIR(&OBJLIB/JMDIR)
             ADDBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCTBL))
             ENDPGM
