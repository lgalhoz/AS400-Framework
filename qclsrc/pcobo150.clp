             /*===========================================================*/
             /* Programa..: PCOBO150                                      */
             /* Descrição.: Identificar os recibos que vão vencer no dia  */
             /*             útil, seguinte.                               */
             /*             Adicionalmente executa um programa de         */
             /*             auditoria, para avaliar se existem recibos de */
             /*             oferta seleccionados.                         */
             /*===========================================================*/
             PGM
             CALL       PGM(#JMPRDOBJ/RCOBO200)

             RUNQRY     QRY(LIB.SD/RECOF001)
             RUNQRY     QRY(LIB.SD/RECOF002)
             RUNQRY     QRY(LIB.SD/RECOF003) /* Recibos                                  */

             RUNQRY     QRY(LIB.SD/RECOF004)
             RUNQRY     QRY(LIB.SD/RECOF005) /* Contratos de Outros Ramos                */
             RUNQRY     QRY(LIB.SD/RECOF006) /* Contratos de Automóvel                   */
             RUNQRY     QRY(LIB.SD/RECOF007) /* Contratos de Outros Ramos Temporárias    */
             RUNQRY     QRY(LIB.SD/RECOF008) /* Contratos de Automóvel Temporárias       */

             ENDPGM
