/*******************************************************************/
/* Programa..: PINTC020                                            */
/* Objectivo.: Actualizar o programa de Serviço de Interfaces      */
/*******************************************************************/
/* Observação: Executar sempre que realizar alguma alteração ou    */
/*             inclusão de um novo módulo.                         */
/*             Os programa tem de ser compilados para reflectir as */
/*             novas alterações.                                   */
/*******************************************************************/
          PGM
          /******************************************************/
          /* Gerar os Módulos.                                  */
          /******************************************************/
          CRTRPGMOD  MODULE(#JMPRDOBJ/RINTO020) +
                       SRCFILE(#JMPRDSRC/QRPGLESRC) +
                       DBGVIEW(*SOURCE) OUTPUT(*NONE) REPLACE(*YES)

          /******************************************************/
          /* Gerar a biblioteca (Programa de Serviço)           */
          /******************************************************/
          RMVBNDDIRE BNDDIR(#JMPRDOBJ/JMINT) OBJ((#JMPRDOBJ/PRCINT))
          DLTSRVPGM  SRVPGM(#JMPRDOBJ/PRCINT)
          CRTSRVPGM  SRVPGM(#JMPRDOBJ/PRCINT) +
                     MODULE(#JMPRDOBJ/RINTO020) EXPORT(*ALL) +
                     BNDDIR(#JMPRDOBJ/JMDIR)
          ADDBNDDIRE BNDDIR(#JMPRDOBJ/JMINT) OBJ((#JMPRDOBJ/PRCINT))
          ENDPGM
