/*******************************************************************/
/* Programa..: PINTC020                                            */
/* Objectivo.: Actualizar o programa de Servi�o de Interfaces      */
/*******************************************************************/
/* Observa��o: Executar sempre que realizar alguma altera��o ou    */
/*             inclus�o de um novo m�dulo.                         */
/*             Os programa tem de ser compilados para reflectir as */
/*             novas altera��es.                                   */
/*******************************************************************/
          PGM
          /******************************************************/
          /* Gerar os M�dulos.                                  */
          /******************************************************/
          CRTRPGMOD  MODULE(#JMPRDOBJ/RINTO020) +
                       SRCFILE(#JMPRDSRC/QRPGLESRC) +
                       DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

          /******************************************************/
          /* Gerar a biblioteca (Programa de Servi�o)           */
          /******************************************************/
          RMVBNDDIRE BNDDIR(#JMPRDOBJ/JMINT) OBJ((#JMPRDOBJ/PRCINT))
          DLTSRVPGM  SRVPGM(#JMPRDOBJ/PRCINT)
          CRTSRVPGM  SRVPGM(#JMPRDOBJ/PRCINT) +
                     MODULE(#JMPRDOBJ/RINTO020) EXPORT(*ALL) +
                     BNDDIR(#JMPRDOBJ/JMDIR)
          ADDBNDDIRE BNDDIR(#JMPRDOBJ/JMINT) OBJ((#JMPRDOBJ/PRCINT))
          ENDPGM
