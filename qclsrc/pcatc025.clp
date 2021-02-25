/*******************************************************************/
/* Programa..: PCATC025                                            */
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
             CRTDSPF    FILE(&OBJLIB/FCATE001) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO001) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE010) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE002) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO002) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE003) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO003) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE004) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO004) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE005) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO005) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE006) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO006) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE008) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO008) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE009) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO009) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE001) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO011) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE012) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO012) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE015) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO015) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RCATO008) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                           DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SCATE009) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RCATO009) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RCATO011) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RCATO012) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RCATO013) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RCATO020) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FCATE013) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PCATO013) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             ENDPGM
