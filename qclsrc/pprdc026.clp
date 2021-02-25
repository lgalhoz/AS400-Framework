/*******************************************************************/
/* Programa..: PPRDC020                                            */
/* Objectivo.: Actualizar programas de Produção                    */
/*******************************************************************/
/* Observação: Executar sempre que realizar alguma alteração ou    */
/*             inclusão de um novo módulo.                         */
/*             Os programa tem de ser compilados para reflectir as */
/*             novas alterações.                                   */
/*******************************************************************/
             PGM
             DCL        VAR(&MOD) TYPE(*CHAR) LEN(3) VALUE('PRD')
             DCL        VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
             DCL        VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
             DCL        VAR(&SRCLIB) TYPE(*CHAR) LEN(10)

             CHGVAR     VAR(&DBSLIB) VALUE('#JM' *CAT &MOD *CAT 'DBS')
             CHGVAR     VAR(&OBJLIB) VALUE('#JM' *CAT &MOD *CAT 'OBJ')
             CHGVAR     VAR(&SRCLIB) VALUE('#JM' *CAT &MOD *CAT 'SRC')

             /******************************************************/
             /* Gerar os Módulos.                                  */
             /******************************************************/
             CRTDSPF    FILE(&OBJLIB/FPRDE100) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO100) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE104) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO104) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE105) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO105) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE107) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO107) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE102) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO102) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE103) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO103) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE106) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO106) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO014) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             ENDPGM
