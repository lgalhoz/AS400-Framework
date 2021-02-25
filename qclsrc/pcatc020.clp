/*******************************************************************/
/* Programa..: PCATC020                                            */
/* Objectivo.: Actualizar o programa de Servi�o                    */
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
             CRTRPGMOD  MODULE(&OBJLIB/RCATO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)
             CRTDSPF    FILE(&OBJLIB/SCATE014) +
                          SRCFILE(&SRCLIB/QDDSSRC) RSTDSP(*YES) +
                          OPTION(*NOSRC *NOLIST)
             CRTRPGMOD  MODULE(&OBJLIB/RCATO014) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Gerar a biblioteca (Programa de Servi�o)           */
             /******************************************************/
             RMVBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCCAT))
             DLTSRVPGM  SRVPGM(&OBJLIB/PRCCAT)
             MONMSG CPF2105
             CRTSRVPGM  SRVPGM(&OBJLIB/PRCCAT) +
                          MODULE(&OBJLIB/RCATO010 &OBJLIB/RCATO014) +
                          EXPORT(*ALL)
             ADDBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCCAT))
             ENDPGM
