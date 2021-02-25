/*******************************************************************/
/* Programa..: PGRLC020                                            */
/* Objectivo.: Actualizar o programa de Serviço                    */
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
             CRTRPGMOD  MODULE(&OBJLIB/RGRLO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO012) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO013) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO015) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO016) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SGRLE017) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST) RSTDSP(*YES)
             CRTRPGMOD  MODULE(&OBJLIB/RGRLO017) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO018) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO019) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             DSPFD      FILE(#JMDEVSRC/QCLSRC) TYPE(*BASATR) +
                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/FH456SX)
             CRTRPGMOD  MODULE(&OBJLIB/RGRLO020) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)
             DLTF       FILE(QTEMP/FH456SX)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO022) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO023) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO024) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SGRLE025) +
                          SRCFILE(&SRCLIB/QDDSSRC) SRCMBR(SGRLE025) +
                          OPTION(*NOSRC *NOLIST)
             CRTRPGMOD  MODULE(&OBJLIB/RGRLO025) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO027) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO028) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO029) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO030) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTRPGMOD  MODULE(&OBJLIB/RGRLO032) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          DBGVIEW(*SOURCE) OUTPUT(*NONE) +
                          REPLACE(*YES)

             CRTCMOD    MODULE(&OBJLIB/RGRLB001) +
                          SRCFILE(&SRCLIB/QRPGLESRC) +
                          OPTION(*STDINC) REPLACE(*YES)

             /******************************************************/
             /* Gerar a biblioteca (Programa de Serviço)           */
             /******************************************************/
             RMVBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCGRL))
             RMVBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((QHTTPSVR/QTMHCGI))
             DLTSRVPGM  SRVPGM(&OBJLIB/PRCGRL)
             MONMSG CPF2105
             CRTSRVPGM  SRVPGM(&OBJLIB/PRCGRL) +
                          MODULE(&OBJLIB/RGRLO010 &OBJLIB/RGRLO012 +
                          &OBJLIB/RGRLO015 &OBJLIB/RGRLO013 +
                          &OBJLIB/RGRLO017 &OBJLIB/RGRLO018 +
                          &OBJLIB/RGRLO019 &OBJLIB/RGRLO020 +
                          &OBJLIB/RGRLO022 &OBJLIB/RGRLO023 +
                          &OBJLIB/RGRLO024 &OBJLIB/RGRLO027 +
                          &OBJLIB/RGRLO028 &OBJLIB/RGRLB001 +
                          &OBJLIB/RGRLO029 &OBJLIB/RGRLO030 +
                          &OBJLIB/RGRLO032 &OBJLIB/RGRLO016) +
                          EXPORT(*ALL) BNDDIR(&OBJLIB/JMDIR)
             ADDBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((&OBJLIB/PRCGRL))
             ADDBNDDIRE BNDDIR(&OBJLIB/JMDIR) OBJ((QHTTPSVR/QTMHCGI))
             ENDPGM
