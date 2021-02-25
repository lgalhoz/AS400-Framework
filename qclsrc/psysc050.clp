             /*******************************************************************/
             /* PROGRAMA...: PDBSC050                                           */
             /* DESCRIÇÃO..: Limpar Receptores Diários.                         */
             /*                                                                 */
             /* OBSERVAÇÃO.: Este programa deve correr diariamente e tem como   */
             /*              Missão limpara os receptores diários, deve ser     */
             /*              executado, no final de todos os JOB's              */
             /*******************************************************************/
             PGM
             DCL        VAR(&JMLIBNAM) TYPE(*CHAR) LEN(10) VALUE('#JMPRDDBS')

             /*******************************************************************/
             /* Desligar as Sessões que estão a prender o Diário                */
             /*******************************************************************/
             CALL PGM(QGPL/ENDLCKJOBS)

             /*******************************************************************/
             /* Desligar todos os ficheiros do Diário                           */
             /*******************************************************************/
             ENDJRNPF   FILE(*ALL) JRN(&JMLIBNAM/JMDBS)

             /*******************************************************************/
             /* Limpar os Diários                                               */
             /*******************************************************************/
             DLTJRN     JRN(&JMLIBNAM/JMDBS)
             DLTJRNRCV  JRNRCV(&JMLIBNAM/JMDBS*) DLTOPT(*IGNINQMSG)
             MONMSG     MSGID(CPA0701 CPF2125)
             CRTJRNRCV  JRNRCV(&JMLIBNAM/JMDBS)
             CRTJRN     JRN(&JMLIBNAM/JMDBS) JRNRCV(&JMLIBNAM/JMDBS)

             /*******************************************************************/
             /* Ligar todos os ficheiros ao Diário                              */
             /*******************************************************************/
             CALL       PGM(#JMPRDSRC/PSYSC051)
             /* STRJRNPF   FILE(&JMLIBNAM/*ALL) JRN(&JMLIBNAM/JMDBS) + */
             /*             OMTJRNE(*OPNCLO)                           */
             MONMSG     MSGID(CPA0701 CPF7030)
             ENDPGM
