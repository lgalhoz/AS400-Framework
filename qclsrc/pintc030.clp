             /********************************************************/
             /* PROGRAMA.: PINTC030                                  */
             /* OBJECTIVO: Realizar a troca automática de ficheiros  */
             /*            entre a Lusitania e a Jose Mata.          */
             /********************************************************/
             PGM
             DCL        VAR(&TYPE) TYPE(*CHAR) LEN(1)
             DCL        VAR(&TIME) TYPE(*CHAR) LEN(10)
             DCL        VAR(&TIMER) TYPE(*CHAR) LEN(2)
             DCL        VAR(&HRINI) TYPE(*CHAR) LEN(6)
             DCL        VAR(&DTINI) TYPE(*CHAR) LEN(6)

             /*******************************************************/
             /* TYPE = 1 (INTER); TYPE = 0 (BATCH)                  */
             /*******************************************************/
             RTVJOBA    TYPE(&TYPE)
 REPLY:      RTVSYSVAL  SYSVAL(QTIME) RTNVAR(&TIME)
             CHGVAR     VAR(&TIMER) VALUE(%SST(&TIME 1 2))

             /*===============================================*/
             /* ESTABELECER A LIGAÇÃO (ACTIVAR LINHA)         */
             /*===============================================*/
             STRTCPIFC  INTNETADR('10.11.8.100')
             MONMSG     MSGID(TCP1B12)

             /*===============================================*/
             /* OBTER OS INTERFACES DO SHARE LUSITANIA        */
             /*===============================================*/
             QSH        CMD('sftp -b +
                          /interfaces/GetFromLusitania.cmd +
                          jmata@172.16.1.3')
             DLTSPLF    FILE(QPRINT) SPLNBR(*LAST)

             /*===============================================*/
             /* COLOCAR FICHEIROS NO SHARE LUSITANIA          */
             /*===============================================*/
             QSH        CMD('sftp -b +
                          /interfaces/SendToLusitania.cmd +
                          jmata@172.16.1.3')
             QSH  CMD('cp /interfaces/pendente/* /interfaces/enviado/')
             QSH  CMD('rm /interfaces/pendente/*')
             DLTSPLF    FILE(QPRINT) SPLNBR(*LAST)

             /*===============================================*/
             /* FECHAR A LIGAÇÃO (DESACTIVAR LINHA)           */
             /*===============================================*/
             ENDTCPIFC  INTNETADR('10.11.8.100')

             /*===============================================*/
             /* Tratar Interfaces                             */
             /*  PGRLO015 -> Acesso a um servidor externo FTP */
             /*  PGRLO016 -> Acesso ao ambiente PASE          */
             /*===============================================*/
             SBMJOB     CMD(CALL PGM(#JMPRDOBJ/PGRLO015)) +
                          JOB(PGRLO015) JOBQ(QINTER)
             SBMJOB     CMD(CALL PGM(#JMPRDOBJ/PGRLO016)) +
                          JOB(PGRLO016) JOBQ(QINTER)

             /*===============================================*/
             /* LIGAR RECIBOS ÀS APÓLICE E CRIAR SE NÃO EXIST */
             /*===============================================*/
             CALL PGM(#JMPRDOBJ/PINTC032)

             /*=====================================================*/
             /* Repetir o Processo ao fim de 1 hora até às 19H00    */
             /*=====================================================*/
             IF         COND(&TYPE *EQ '0') THEN(DO)
                IF         COND(&TIMER *LT '19') THEN(DO)
                   DLYJOB     DLY(3600) /* Aguardar 1 Hora */
                   GOTO       CMDLBL(REPLY)
                ENDDO
             ENDDO
             ENDPGM
