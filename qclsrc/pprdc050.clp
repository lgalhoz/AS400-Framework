             PGM
             DCL        VAR(&OBJLIB) TYPE(*CHAR) LEN(10) VALUE('#JMPRDOBJ')
             DCL        VAR(&DBSLIB) TYPE(*CHAR) LEN(10) VALUE('#JMPRDDBS')
             DCL        VAR(&SRCLIB) TYPE(*CHAR) LEN(10) VALUE('#JMPRDSRC')


             WRKACTJOB  SBS(QHTTPSVR) JOB(JOSEMATA)
             /* ENDTCPSVR  SERVER(*HTTP) HTTPSVR(JOSEMATA) */
             /* CALL PGM(#JMPRDSRC/PGRLC020) */
             /* CALL PGM(#JMPRDSRC/PPRDC020) */

             /*=============================================================*/
             /* MANUTENÇÃO DE NOTAS DESCRITIVAS                             */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE102) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO102) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* MANUTENÇÃO DE LIMITES E FRANQUIAS                           */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE103) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO103) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* SELECÇÃO DE COBERTURAS                                      */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE104) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO104) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* RISCOS DA APÓLICE                                           */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE105) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO105) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* CLAUSULAS E CONDIÇÕES ESPECIAIS                             */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE106) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO106) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* AVALIAÇÃO DO RISCO DO IMÓVEL                                */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/FPRDE107) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/PPRDO107) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* ESCRITOR DE ACTAS/APÓLICES                                  */
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
             /* [ACTAS] CONDIÇÕES ESPECIAIS - CLÁUSULAS                     */
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
             /* [ACTAS] COMENTÁRIOS E OBSERVAÇÕES                           */
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
             /* [ACTAS] NATUREZA/USO DO IMÓVEL                              */
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
             /* [NOTA DESCRITIVA] HABITAÇÃO                                */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/SPRDE103) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO103) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)


             /*=============================================================*/
             /* [NOTA DESCRITIVA] GENÉRICO                                  */
             /*=============================================================*/
             CRTDSPF    FILE(&OBJLIB/SPRDE104) +
                          SRCFILE(&SRCLIB/QDDSSRC) +
                          OPTION(*NOSRC *NOLIST)
             CRTBNDRPG  PGM(&OBJLIB/RPRDO104) +
                          SRCFILE(&SRCLIB/QRPGLESRC) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)

             /*=============================================================*/
             /* [NOTA DESCRITIVA] HABITAÇÃO (CONDOMINIO)                    */
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
