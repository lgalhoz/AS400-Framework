   PGM
   /*========================================================*/
   /* Limpar ficheiros de Sinistros                          */
   /*========================================================*/
   /* CLRPFM FILE(#JMPRDDBS/FSINT100) */
   /* CLRPFM FILE(#JMPRDDBS/FSINT101) */
   /* CLRPFM FILE(#JMPRDDBS/FSINT102) */
   /* CLRPFM FILE(#JMPRDDBS/FSINT118) */
   /* CLRPFM FILE(#JMPRDDBS/FSINT128) */

   /*========================================================*/
   /* Executar                                               */
   /*========================================================*/
   CLRPFM FILE(QTEMP/FINTT030)
   CALL PGM(#JMPRDOBJ/PSINT998) PARM('QS36F/FICI150824')

   /*========================================================*/
   /* Executar                                               */
   /*========================================================*/
   ENDDBG
   STRDBG PGM(#JMPRDOBJ/PINTO019)
   CALL PGM(#JMPRDOBJ/PINTO019) PARM('FICI150824' X'F0' 'D' '' '')

   ENDPGM
