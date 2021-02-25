/*******************************************************************/
/* Programa..: PTBLC025                                            */
/* Objectivo.: Actualizar programas de Produ��o                    */
/*******************************************************************/
/* Observa��o: Executar sempre que realizar alguma altera��o ou    */
/*             inclus�o de um novo m�dulo.                         */
/*             Os programa tem de ser compilados para reflectir as */
/*             novas altera��es.                                   */
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
             /* Gerar os M�dulos.                                  */
             /******************************************************/
             CRTDSPF    FILE(&OBJLIB/FGRLE010) +
                          SRCFILE(&SRCLIB/QDDSSRC) SRCMBR(FGRLE010) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PGRLO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PGRLO010) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PGRLO001) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/PGRLO002) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/PGRLO205) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             ENDPGM
