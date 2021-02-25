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
             DCL        VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
             DCL        VAR(&SRCLIB) TYPE(*CHAR) LEN(10)
             CHGVAR     VAR(&OBJLIB) VALUE('#JM' *CAT &MOD *CAT 'OBJ')
             CHGVAR     VAR(&SRCLIB) VALUE('#JM' *CAT &MOD *CAT 'SRC')

             /******************************************************/
             /* Gerar os Módulos.                                  */
             /******************************************************/
             CRTBNDRPG  PGM(&OBJLIB/RSINO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(RSINO010) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(LTLUSIT/TRSF22) +
                          SRCFILE(LTLUSIT/QRPGLESRC) +
                          SRCMBR(TRSF22) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(LTLUSIT/TRSF23) +
                          SRCFILE(LTLUSIT/QRPGLESRC) +
                          SRCMBR(TRSF23) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(LTLUSIT/TRSF24) +
                          SRCFILE(LTLUSIT/QRPGLESRC) +
                          SRCMBR(TRSF24) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(LTLUSIT/TRSF25) +
                          SRCFILE(LTLUSIT/QRPGLESRC) +
                          SRCMBR(TRSF25) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(LTLUSIT/TRSF26) +
                          SRCFILE(LTLUSIT/QRPGLESRC) +
                          SRCMBR(TRSF26) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(LTLUSIT/TRSF27) +
                          SRCFILE(LTLUSIT/QRPGLESRC) +
                          SRCMBR(TRSF27) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(LTLUSIT/TRSF60) +
                          SRCFILE(LTLUSIT/QRPGLESRC) +
                          SRCMBR(TRSF60) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

      /*     CRTBNDRPG  PGM(LTLUSIT/TRSF61) +               */
      /*                  SRCFILE(LTLUSIT/QRPGLESRC) +      */
      /*                  SRCMBR(TRSF61) DBGVIEW(*LIST) +   */
      /*                  OUTPUT(*NONE) REPLACE(*YES)       */

             ENDPGM
