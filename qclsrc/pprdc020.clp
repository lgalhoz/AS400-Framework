/*******************************************************************/
/* Programa..: PPRDC020                                            */
/* Objectivo.: Actualizar programas de Produ��o                    */
/*******************************************************************/
/* Observa��o: Executar sempre que realizar alguma altera��o ou    */
/*             inclus�o de um novo m�dulo.                         */
/*             Os programa tem de ser compilados para reflectir as */
/*             novas altera��es.                                   */
/*******************************************************************/
             PGM
             DCL  VAR(&MOD) TYPE(*CHAR) LEN(3) VALUE('PRD')
             DCL  VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
             DCL  VAR(&SRCLIB) TYPE(*CHAR) LEN(10)

             CHGVAR VAR(&OBJLIB) VALUE('#JM' *CAT &MOD *CAT 'OBJ')
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
             CRTRPGMOD  MODULE(&OBJLIB/RPRDO035) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Gerar a biblioteca (Programa de Servi�o)           */
             /******************************************************/
             RMVBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCPRD))
             DLTSRVPGM  SRVPGM(&OBJLIB/PRCPRD)
             MONMSG     MSGID(CPF2105)
             CRTSRVPGM  SRVPGM(&OBJLIB/PRCPRD) +
                          MODULE(&OBJLIB/RPRDO035) EXPORT(*ALL)
             ADDBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCPRD))
             ENDPGM
