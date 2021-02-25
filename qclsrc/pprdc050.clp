             PGM
             DCL        VAR(&OBJLIB) TYPE(*CHAR) LEN(10) VALUE('#JMPRDOBJ')
             DCL        VAR(&DBSLIB) TYPE(*CHAR) LEN(10) VALUE('#JMPRDDBS')
             DCL        VAR(&SRCLIB) TYPE(*CHAR) LEN(10) VALUE('#JMPRDSRC')


             WRKACTJOB  SBS(QHTTPSVR) JOB(JOSEMATA)
             /* ENDTCPSVR  SERVER(*HTTP) HTTPSVR(JOSEMATA) */
             /* CALL PGM(#JMPRDSRC/PGRLC020) */
             /* CALL PGM(#JMPRDSRC/PPRDC020) */

             /*=============================================================*/
             /* MANUTEN��O DE NOTAS DESCRITIVAS                             */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE102) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO102) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* MANUTEN��O DE LIMITES E FRANQUIAS                           */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE103) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO103) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* SELEC��O DE COBERTURAS                                      */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE104) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO104) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* RISCOS DA AP�LICE                                           */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE105) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO105) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* CLAUSULAS E CONDI��ES ESPECIAIS                             */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE106) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO106) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* AVALIA��O DO RISCO DO IM�VEL                                */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE107) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO107) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* ESCRITOR DE ACTAS/AP�LICES                                  */
             /*=============================================================*/
             RTVOBJD    OBJ(&DBSLIB/GEXAP) OBJTYPE(*FILE)
             MONMSG     MSGID(CPF9812) EXEC(CRTPF +
                          FILE(&DBSLIB/GEXAP) SRCFILE(&SRCLIB/QS36DDS))
             CRTBNDRPG  PGM(&OBJLIB/RPRDO008) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] HEADER                                              */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO009) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] SUBHEADER                                           */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO010) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] NOTAS DESCRITIVAS                                   */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO011) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] LIMITES E FRANQUIAS                                 */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO012) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] CONDI��ES ESPECIAIS - CL�USULAS                     */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO013) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] COBERTURAS E GARANTIAS                              */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO014) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] COMENT�RIOS E OBSERVA��ES                           */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO015) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] TEXTO FINAL                                         */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO016) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] ITENS (NOTA DESCRITIVA) CONDOMINIO                  */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO017) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] NATUREZA/USO DO IM�VEL                              */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO018) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [ACTAS] ANEXOS A COBERTURAS COMPLEMENTARES                  */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO019) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [HTTP] DISPONIBILIZAR ACTA                                  */
             /*=============================================================*/
             CRTBNDRPG  PGM(&OBJLIB/RPRDO023) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [NOTA DESCRITIVA] HABITA��O                                */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/SPRDE103) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO103) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)


             /*=============================================================*/
             /* [NOTA DESCRITIVA] GEN�RICO                                  */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/SPRDE104) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO104) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [NOTA DESCRITIVA] HABITA��O (CONDOMINIO)                    */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/SPRDE105) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO105) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /* STRTCPSVR  SERVER(*HTTP) HTTPSVR(JOSEMATA) */
             WRKACTJOB  SBS(QHTTPSVR) JOB(JOSEMATA)
             ENDPGM
