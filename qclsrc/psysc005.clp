             /*================================================================*/
             /* Programa..: PSYSC005                                           */
             /* Objectivo.: Gerir o Portal da José Mata                        */
             /*      I ==> Inicial a nova Versão                               */
             /*      F ==> Termina a nova Versão                               */
             /*      S ==> Inicia a Versão Antiga                              */
             /*      E ==> Termina a Versão Antiga                             */
             /*================================================================*/
             PGM        PARM(&MODO)
             DCL        VAR(&MODO) TYPE(*CHAR) LEN(1)

             /*================================================================*/
             /*                                                                */
             /*           INICIAR O PORTAL (NOVA VERSÃO) ==> (I)nicio          */
             /*                                                                */
             /*================================================================*/
             IF         COND(&MODO *EQ 'I') THEN(DO)
             SBMJOB     CMD(QSH CMD('cd +
                          /liferay/tomcat-7.0.42/bin; export +
                          JAVA_HOME="/QOpenSys/QIBM/ProdData/JavaVM/j+
                          dk60/64bit"; ./startup.sh')) +
                          JOB(STR_LIFERA) JOBQ(QINTER) USER(LIFERAY)
             ENDDO
             /*================================================================*/
             /*                                                                */
             /*           TERMINAR O PORTAL (NOVA VERSÃO) ==> (F)im            */
             /*                                                                */
             /*================================================================*/
             IF         COND(&MODO *EQ 'F') THEN(DO)
             SBMJOB     CMD(QSH CMD('cd +
                          /liferay/tomcat-7.0.42/bin; export +
                          JAVA_HOME="/QOpenSys/QIBM/ProdData/JavaVM/j+
                          dk60/64bit"; ./shutdown.sh')) +
                          JOB(STR_LIFERA) JOBQ(QINTER) USER(LIFERAY)
             ENDDO
             /*================================================================*/
             /*                                                                */
             /*           INICIAR O PORTAL (VERSÃO ANTIGA) ==> (S)tart         */
             /*                                                                */
             /*================================================================*/
             IF         COND(&MODO *EQ 'S') THEN(DO)
             SBMJOB     CMD(QSH CMD('cd +
                          /opt/liferay__/tomcat-6.0.18/bin; export +
                          JAVA_HOME="/QOpenSys/QIBM/ProdData/JavaVM/j+
                          dk60/64bit"; ./startup.sh')) +
                          JOB(STR_LIFERA) JOBQ(QINTER) USER(LIFERAY)
             ENDDO
             /*================================================================*/
             /*                                                                */
             /*           TERMINAR O PORTAL (VERSÃO ANTIGA) ==> (E)nd          */
             /*                                                                */
             /*================================================================*/
             IF         COND(&MODO *EQ 'E') THEN(DO)
             SBMJOB     CMD(QSH CMD('cd +
                          /opt/liferay__/tomcat-6.0.18/bin; export +
                          JAVA_HOME="/QOpenSys/QIBM/ProdData/JavaVM/j+
                          dk60/64bit"; ./shutdown.sh')) +
                          JOB(STR_LIFERA) JOBQ(QINTER) USER(LIFERAY)
             ENDDO
             ENDPGM
