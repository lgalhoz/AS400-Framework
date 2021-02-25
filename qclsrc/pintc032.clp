             /********************************************************/
             /* PROGRAMA.: PINTC032                                  */
             /* OBJECTIVO: Ligar Recibos das Companhias a Apólices   */
             /*            e Recibos José Mata.                      */
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
             /* TRATAR INTERFACES RECEBIDOS DA LUSITANIA      */
             /*===============================================*/
             /* CALL PGM(#JMPRDOBJ/PINTO020) /* Associar Recibo Standard à Apólice  */
             /* CALL PGM(#JMPRDOBJ/PINTO022) /* Associar Recibo Standard ao Recibos */
             /* CALL PGM(#JMPRDOBJ/PINTO023) /* Actualizar o estado dos Recibos   */
             /* CALL PGM(#JMPRDOBJ/PINTO024) /* Criar Recibos em José Mata, Lda     */
             /* CALL PGM(#JMPRDOBJ/PINTO026) /* Facturar Recibos em José Mata, Lda  */
             /* CALL PGM(#JMPRDOBJ/PINTO028) /* Cobrar Recibos em José Mata, Lda    */

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
