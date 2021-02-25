             PGM

             SBMJOB     CMD(CALL PGM(QP2SHELL) +
                          PARM('/usr/local/mysql/bin/mysqlmanager' +
                          '--run-as-service')) JOB(MYSQLD) +
                          JOBD(MYSQLLIB/MYSQLJOBD) +
                          JOBQ(MYSQLLIB/MYSQLJOBQ)
             ENDPGM
