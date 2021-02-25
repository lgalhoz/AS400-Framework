             /*===========================================================*/
             /* Programa..: PCOBO150                                      */
             /* Descri��o.: Identificar os recibos que v�o vencer no dia  */
             /*             �til, seguinte.                               */
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
             RUNQRY     QRY(LIB.SD/RECOF006) /* Contratos de Autom�vel                   */
             RUNQRY     QRY(LIB.SD/RECOF007) /* Contratos de Outros Ramos Tempor�rias    */
             RUNQRY     QRY(LIB.SD/RECOF008) /* Contratos de Autom�vel Tempor�rias       */

             ENDPGM
