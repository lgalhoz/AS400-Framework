/*******************************************************************/
/* Programa..: PINTC025                                            */
/* Objectivo.: Gerar Programas de Interfaces                       */
/*******************************************************************/
/* Observação: Executar sempre que realizar alguma alteração ou    */
/*             inclusão de um novo módulo.                         */
/*             Os programa tem de ser compilados para reflectir as */
/*             novas alterações.                                   */
/*******************************************************************/
             PGM
             DCL  VAR(&MODULO) TYPE(*CHAR) LEN(3) VALUE('PRD')
             DCL  VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
             DCL  VAR(&SRCLIB) TYPE(*CHAR) LEN(10)

             CHGVAR VAR(&OBJLIB) VALUE('#JM' *CAT &MODULO *CAT 'OBJ')
             CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &MODULO *CAT 'SRC')

             /******************************************************/
             /* Grupo de Interfaces - Apólices e Coberturas        */
             /******************************************************/
             CRTBNDRPG  PGM(&OBJLIB/PINTO100) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO100) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO101) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO101) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO102) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO102) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO103) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO103) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO104) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO104) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Grupo de Interfaces - Pessoas, Empresas, Entidades */
             /******************************************************/
             CRTBNDRPG  PGM(&OBJLIB/PINTO200) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO200) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO201) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO201) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO202) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO202) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO203) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO203) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO204) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO204) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO205) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO205) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Grupo de Interfaces - Recibos                      */
             /******************************************************/
             CRTBNDRPG  PGM(&OBJLIB/PINTO301) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO301) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Grupo de Interfaces - Sinistros Automóvel          */
             /******************************************************/
             CRTBNDRPG  PGM(&OBJLIB/PINTO401) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO401) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO402) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO402) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO403) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO403) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO404) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO404) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO405) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO405) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO406) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO406) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Grupo de Interfaces - Sinistros Outros Ramos       */
             /******************************************************/
             CRTBNDRPG  PGM(&OBJLIB/PINTO501) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO401) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Grupo de Interfaces - Informação de Co-Seguro      */
             /******************************************************/
             CRTBNDRPG  PGM(&OBJLIB/PINTO601) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO601) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PINTO602) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO602) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Grupo de Interfaces - Cobranças Diárias            */
             /******************************************************/
             CRTBNDRPG  PGM(&OBJLIB/PINTO800) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO800) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /******************************************************/
             /* Grupo de Interfaces - Cobranças Diárias            */
             /******************************************************/
             CRTBNDRPG  PGM(&OBJLIB/PINTO800) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          SRCMBR(PINTO800) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             /******************************************************/
             /* Grupo de Interfaces - Cobranças Diárias            */
             /******************************************************/

             CHGVAR     VAR(&SRCLIB) VALUE('LTLUSIT')
             CHGVAR     VAR(&OBJLIB) VALUE('LTLUSIT')
             CRTBNDRPG  PGM(&OBJLIB/TRBEN) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/TRSAP) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTPF      FILE(QTEMP/OFSEG) SRCFILE(LTLUSIT/QDDSSRC) +
                          SIZE(*NOMAX)
             CRTLF      FILE(QTEMP/GCOBGRX6) SRCFILE(LIB.COBN/QDDSSRC)
             CRTLF      FILE(QTEMP/GBASEGX4) SRCFILE(SOURCE/QDDSSRC)
             CRTLF      FILE(QTEMP/GBASEGX3) SRCFILE(SOURCE/QDDSSRC)
             CRTBNDRPG  PGM(&OBJLIB/TRSF01) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/TRSF01AP) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/TRSF01ISP) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES) +
                          FIXNBR(*ZONED) ALWNULL(*YES)

             ENDPGM
