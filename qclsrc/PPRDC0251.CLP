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


             /*========================================================================*/
             /* NOVO PROGRAMA DE IDD E ADC                                             */
             /*========================================================================*/
             CRTBNDRPG  PGM(SOURCE/TB2020R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(SOURCE/TB2021R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(SOURCE/TB2022R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(SOURCE/TB2023R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(SOURCE/TB2040R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTPF FILE(QTEMP/FCOBT232) SRCFILE(#JMPRDSRC/QDDSSRC)
             CRTBNDRPG  PGM(SOURCE/TB2041R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             ENDPGM
