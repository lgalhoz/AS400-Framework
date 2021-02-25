             PGM
             /*===================================================*/
             /* NORMALIZAR INFRORMAÇÃO DOS PROCESSOS              */
             /*===================================================*/
             /* CLRPFM FILE(#JMPRDDBS/FSINT100)   */
             /* CLRPFM FILE(#JMPRDDBS/FSINT101)   */
             /* CLRPFM FILE(#JMPRDDBS/FSINT102)   */
             CALL       PGM(#JMPRDOBJ/PSINT100)

             /*===================================================*/
             /* NORMALIZAR INFORMAÇÃO SOBRE AS PROVISÕES          */
             /*===================================================*/
             /* CLRPFM FILE(#JMPRDDBS/FSINT118)   */
             /* CLRPFM FILE(#JMPRDDBS/FSINT128)   */

             ENDDBG
             STRDBG     PGM(#JMPRDOBJ/PSINT102)
             CALL       PGM(#JMPRDOBJ/PSINT102)
             ENDPGM
