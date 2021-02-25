/*******************************************************************/
/* Programa..: PSECC025                                            */
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
             CRTBNDRPG  PGM(&OBJLIB/RSECO002) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(RSECO002) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FSECE001) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PSECO001) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PSECO001) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FSECE002) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PSECO002) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PSECO002) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FSECE003) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PSECO003) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PSECO003) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FSECE004) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PSECO004) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PSECO004) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FSECE005) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PSECO005) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PSECO005) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FSECE006) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PSECO006) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PSECO006) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PSECO008) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PSECO008) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RSECO016) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(RSECO016) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             ENDPGM
