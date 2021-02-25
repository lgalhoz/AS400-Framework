             /*======================================================================*/
             /* PROGRAMA.: CONFIG.CLP                                                */
             /* DESCRIÇÃO: Preparar um subsitema dedicado aos desenvolvimentos em    */
             /*            JSP e Servlets dentro do AS400.                           */
             /*            Para o efeito vamos utilizar o Apache400 com o TOM CAT    */
             /*======================================================================*/
             PGM

             /*---+------------------------------------------------------------------*/
             /* 1 | Create the job queue object.                                     */
             /*---+------------------------------------------------------------------*/
             CRTJOBQ    JOBQ(TOMCATLIB/TOMCATJOBQ) TEXT('TOMCAT JOBQ')

             /*---+------------------------------------------------------------------*/
             /* 2 | Create the job description with the routing data and request     */
             /*   | data to call the startup program:                                */
             /*---+------------------------------------------------------------------*/
             CRTJOBD    JOBD(TOMCATLIB/TOMCATJOBD) +
                          JOBQ(TOMCATLIB/TOMCATJOBQ) TEXT('TOMCAT +
                          JOBDESCRIPTION') USER(MYSQL) +
                          RTGDTA('MYSQL') RQSDTA('CALL +
                          TOMCATLIB/STRTOMCAT')

             /*---+------------------------------------------------------------------*/
             /* 3 |  Create a class:                                                 */
             /*---+------------------------------------------------------------------*/
             CRTCLS     CLS(TOMCATLIB/TOMCATCLS) RUNPTY(50) +
                          TEXT('TOMCAT CLASS')

             /*---+------------------------------------------------------------------*/
             /* 4 |  Create the subsystem description:                               */
             /*---+------------------------------------------------------------------*/
             CRTSBSD    SBSD(TOMCATLIB/TOMCATSBS) POOLS((1 *BASE)) +
                          TEXT('TOMCAT SUBSYSTEM')

             /*---+------------------------------------------------------------------*/
             /* 5 | Add a job queue entry to link the job queue that we created      */
             /*   | previously to the subsystem:                                     */
             /*---+------------------------------------------------------------------*/
             ADDJOBQE   SBSD(TOMCATLIB/TOMCATSBS) +
                          JOBQ(TOMCATLIB/TOMCATJOBQ) MAXACT(*NOMAX)

             /*---+------------------------------------------------------------------*/
             /* 6 | Add two routing entries to ensure that the job routing is        */
             /*   | carried out:                                                     */
             /*---+------------------------------------------------------------------*/
             ADDRTGE    SBSD(TOMCATLIB/TOMCATSBS) SEQNBR(100) +
                          CMPVAL(TOMCAT) PGM(QCMD) +
                          CLS(TOMCATLIB/TOMCATCLS)
             ADDRTGE    SBSD(TOMCATLIB/TOMCATSBS) SEQNBR(999) +
                          CMPVAL(*ANY) PGM(QCMD)
             /*---+------------------------------------------------------------------*/
             /* 7 | Add an autostart job to the subsystem that will be called when   */
             /*   | the subsystem is started:                                        */
             /*---+------------------------------------------------------------------*/
             ADDAJE     SBSD(TOMCATLIB/TOMCATSBS) JOB(AUTOSTART) +
                          JOBD(TOMCATLIB/TOMCATJOBD)
             ENDPGM
