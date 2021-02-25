             /*******************************************************************/
             /* PROGRAMA...: PDBSC050                                           */
             /* DESCRI��O..: Limpar Receptores Di�rios.                         */
             /*                                                                 */
             /* OBSERVA��O.: Este programa deve correr diariamente e tem como   */
             /*              Miss�o limpara os receptores di�rios, deve ser     */
             /*              executado, no final de todos os JOB's              */
             /*******************************************************************/
             PGM
             DCL        VAR(&JMLIBNAM) TYPE(*CHAR) LEN(10) VALUE('#JMPRDDBS')

             /*******************************************************************/
             /* Desligar as Sess�es que est�o a prender o Di�rio                */
             /*******************************************************************/
             CALL PGM(QGPL/ENDLCKJOBS)

             /*******************************************************************/
             /* Desligar todos os ficheiros do Di�rio                           */
             /*******************************************************************/
             ENDJRNPF   FILE(*ALL) JRN(&JMLIBNAM/JMDBS)

             /*******************************************************************/
             /* Limpar os Di�rios                                               */
             /*******************************************************************/
             DLTJRN     JRN(&JMLIBNAM/JMDBS)
             DLTJRNRCV  JRNRCV(&JMLIBNAM/JMDBS*) DLTOPT(*IGNINQMSG)
             MONMSG     MSGID(CPA0701 CPF2125)
             CRTJRNRCV  JRNRCV(&JMLIBNAM/JMDBS)
             CRTJRN     JRN(&JMLIBNAM/JMDBS) JRNRCV(&JMLIBNAM/JMDBS)

             /*******************************************************************/
             /* Ligar todos os ficheiros ao Di�rio                              */
             /*******************************************************************/
             CALL       PGM(#JMPRDSRC/PSYSC051)
             /* STRJRNPF   FILE(&JMLIBNAM/*ALL) JRN(&JMLIBNAM/JMDBS) + */
             /*             OMTJRNE(*OPNCLO)                           */
             MONMSG     MSGID(CPA0701 CPF7030)
             ENDPGM
