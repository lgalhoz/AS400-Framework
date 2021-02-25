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
             CRTSQLRPGI OBJ(&OBJLIB/RPRDO001) +
                          SRCFILE(&SRCLIB/QRPGLESRC) OUTPUT(*NONE) +
                          CLOSQLCSR(*ENDMOD) REPLACE(*YES) +
                          DBGVIEW(*SOURCE)

             CRTDSPF    FILE(&OBJLIB/FPRDE001) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO001) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE002) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO002) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE010) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/PPRDO009) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE012) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO012) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE108) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO108) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE109) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO109) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE110) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO110) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE100) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO100) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE101) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO101) +
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

             CRTDSPF    FILE(&OBJLIB/FPRDE106) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO106) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE107) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO107) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO111) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO111) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO112) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             RTVOBJD    OBJ(&DBSLIB/GEXAP) OBJTYPE(*FILE)
             MONMSG     MSGID(CPF9812) EXEC(CRTPF +
                          FILE(&DBSLIB/GEXAP) SRCFILE(&SRCLIB/QS36DDS))
             CRTBNDRPG  PGM(&OBJLIB/RPRDO008) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO009) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO016) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO003) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO011) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO012) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO013) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO014) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO015) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/FPRDE015) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO016) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO017) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(&OBJLIB/RPRDO018) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SPRDE103) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO103) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SPRDE104) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO104) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SPRDE105) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO105) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(&OBJLIB/SPRDE004) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO004) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO106) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO107) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO019) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(&OBJLIB/RPRDT001) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
    /*    CRTBNDRPG  PGM(&OBJLIB/RPRDT002) +                 */
    /*          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +  */
    /*          OUTPUT(*NONE) REPLACE(*YES)                  */


/*******************************************************************/
/*                                                                 */
/*   P A R T E       A N T I G A                                   */
/*                                                                 */
/*******************************************************************/
             CRTBNDRPG  PGM(SOURCE/GP010) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(SOURCE/GP010X) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(SOURCE/GP015) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(SOURCE/GP015X) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(SOURCE/FIXPRDT002) +
                          SRCFILE(SOURCE/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(SOURCE/GP045) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)

             CRTBNDRPG  PGM(LIB.SD/SD804) SRCFILE(LIB.SD/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)

     /*      CRTBNDRPG  PGM(LGALHOZ/GP045) +               */
     /*                   SRCFILE(LGALHOZ/QRPGLESRC) +     */
     /*                   SRCMBR(GP045) DBGVIEW(*LIST) +   */
     /*                   OUTPUT(*NONE) REPLACE(*YES)      */

     /*      CRTBNDRPG  PGM(SOURCE/SF005) +                */
     /*                   SRCFILE(SOURCE/QRPGLESRC) +      */
     /*                   SRCMBR(SF005) DBGVIEW(*LIST) +   */
     /*                   OUTPUT(*NONE) REPLACE(*YES)      */

             /*========================================================================*/
             /* ACTUAL PROGRAMA DE ACIDENTES PESSOAIS                                  */
             /*========================================================================*/
             CRTPF      FILE(QTEMP/OPLSEGT) SRCFILE(SOURCE/QDDSSRC) +
                          OPTION(*NOSRC)
             MONMSG     MSGID(CPF2869 CPF2817 CPF5813 CPF7302)
             CRTLF      FILE(QTEMP/OPLSEGT1) SRCFILE(SOURCE/QDDSSRC) +
                          OPTION(*NOSRC)
             MONMSG     MSGID(CPF2869 CPF2817 CPF5813 CPF7302)
             CRTBNDRPG  PGM(SOURCE/AP6000R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(SOURCE/AP6051R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)

             /*========================================================================*/
             /* NOVO PROGRAMA DE ACIDENTES PESSOAIS                                    */
             /*========================================================================*/
             CRTBNDRPG  PGM(SOURCE/AP6002R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(SOURCE/AP6004R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(SOURCE/AP6053R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(SOURCE/AP6042R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(SOURCE/AP6012R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)

             /*========================================================================*/
             /* NOVO PROGRAMA DE IDD E ADC                                             */
             /*========================================================================*/
        /*   CRTBNDRPG  PGM(SOURCE/TB2020R) SRCFILE(SOURCE/QRPGLESRC) +       */
        /*                DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)          */
        /*   CRTBNDRPG  PGM(SOURCE/TB2021R) SRCFILE(SOURCE/QRPGLESRC) +       */
        /*                DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)          */
        /*   CRTBNDRPG  PGM(SOURCE/TB2022R) SRCFILE(SOURCE/QRPGLESRC) +       */
        /*                DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)          */
        /*   CRTBNDRPG  PGM(SOURCE/TB2023R) SRCFILE(SOURCE/QRPGLESRC) +       */
        /*                DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)          */
        /*   CRTBNDRPG  PGM(SOURCE/TB2040R) SRCFILE(SOURCE/QRPGLESRC) +       */
        /*                DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)          */
        /*   CRTPF FILE(QTEMP/FCOBT232) SRCFILE(#JMPRDSRC/QDDSSRC)            */
        /*   MONMSG     MSGID(CPF2869 CPF2817 CPF5813 CPF7302)                */
        /*   CRTBNDRPG  PGM(SOURCE/TB2041R) SRCFILE(SOURCE/QRPGLESRC) +       */
        /*                DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)          */
             ENDPGM
