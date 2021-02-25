/*******************************************************************/
/* Programa..: PENTC020                                            */
/* Objectivo.: Actualizar o programa de Serviço de Tabelas         */
/*             Gerais.                                             */
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
             CRTDSPF    FILE(&OBJLIB/SENTE012) +
                          SRCFILE(&SRCLIB/QDDSSRC) RSTDSP(*YES) +
                          OPTION(*NOSRC *NOLIST)
             CRTRPGMOD  MODULE(&OBJLIB/RENTO012) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SENTE013) +
                          SRCFILE(&SRCLIB/QDDSSRC) RSTDSP(*YES) +
                          OPTION(*NOSRC *NOLIST)
             CRTRPGMOD  MODULE(&OBJLIB/RENTO013) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)
             CRTRPGMOD  MODULE(&OBJLIB/RENTO014) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)
             CRTRPGMOD  MODULE(&OBJLIB/RENTO016) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Gerar a biblioteca (Programa de Serviço)           */
             /******************************************************/
             RMVBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCENT))
             DLTSRVPGM  SRVPGM(&OBJLIB/PRCENT)
             MONMSG CPF2105
             CRTSRVPGM  SRVPGM(&OBJLIB/PRCENT) +
                          MODULE(&OBJLIB/RENTO012 &OBJLIB/RENTO013 +
                          &OBJLIB/RENTO014 &OBJLIB/RENTO016) +
                          EXPORT(*ALL) +
                          BNDDIR(&OBJLIB/JMDIR)
             ADDBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCENT))
             ENDPGM
