             /*================================================================*/
             /* Programa..: PSYSC005                                           */
             /* Objectivo.: Gerir o Portal da Jos� Mata                        */
             /*      I ==> Inicial a nova Vers�o                               */
             /*      F ==> Termina a nova Vers�o                               */
             /*      S ==> Inicia a Vers�o Antiga                              */
             /*      E ==> Termina a Vers�o Antiga                             */
             /*================================================================*/
             PGM        PARM(&MODO)
             DCL        VAR(&MODO) TYPE(*CHAR) LEN(1)

             /*================================================================*/
             /*                                                                */
             /*           INICIAR O PORTAL (NOVA VERS�O) ==> (I)nicio          */
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
             /*           TERMINAR O PORTAL (NOVA VERS�O) ==> (F)im            */
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
             /*           INICIAR O PORTAL (VERS�O ANTIGA) ==> (S)tart         */
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
             /*           TERMINAR O PORTAL (VERS�O ANTIGA) ==> (E)nd          */
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
