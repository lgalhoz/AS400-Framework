      *=====================================================================
      * Rotina....: RVNCP010
      *=====================================================================
      * Objectivo.: Imprimir Avisos de Lançamento
      * Descrição.: Para o efeito recebe o número do Recibo para o qual
      *             deseja emitir o Aviso de Lançamento, o numero do Aviso
      *             é o mesmo da Guia.
      *=====================================================================
     FRECIBO    IF   F  326     6AIDISK    KEYLOC(5)
     F                                     USROPN
     D*=====================================================================
     D
     D*==> Invocar API de comandos
     D Shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D WFile           S             10A
     D WIcErro         S               N
     D
     D WRecibos        DS                  QUALIFIED DIM(20)
     D    WNumOrdem
     D    WRamo
     D    WApolice
     D    WDtIni
     D    WDtFim
     D    WTipo
     D    WValor
     D
     I*=====================================================================
     IRECIBO    AA
     I                                  1    4 0Key
     I                                  5   10 0Ordem
     I                                 11   20 0Apol
     I                                 29   31 0Comp
     I                                 32   37 0Segurado
     I                                 38   40 0Ramo
     I                             P   53   59 2Premio
     I                             P   60   64 2Comissao
     I                             P  147  151 2Com00
     I                             P  152  153 0Med01                           Mediador S/Cobrança
     I                             P  154  158 2Com01                           Comissão S/Cobrança
     I                             P  159  160 0Med02                           Mediador C/Cobrança
     I                             P  161  165 2Com02                           Comissão C/Cobrança
     I                             P  168  171 2NDev
     I                                180  181 0Mes
     C*=====================================================================
     C
      /FREE

       //---------------------------------------------------
       // Actualizar lista de trabalho com Recibos Activos
       //---------------------------------------------------
       WFile = 'SD.1.RCA';
       ExSr ReadRecibo;

       //---------------------------------------------------
       // Actualizar lista de trabalho com Recibos Activos
       //---------------------------------------------------
       WFile = 'SD.1.RCA';
       ExSr ReadRecibo;

       //---------------------------------------------------
       // Actualizar lista de trabalho com Recibos Historico
       //---------------------------------------------------
       WFile = 'SD.1.RCM';
       ExSr ReadRecibo;

       *InLr = *ON;
       //============================================================
       // Subrotina: ReadRecibo
       // Objectivo: Actualizar listagem com os Recibos
       //=============================================================
       BEGSR ReadRecibo;
          EXSR OpenFile; // Abrir o ficheiro de Recibos SD.1.RCA
          IF (NOT WIcErro);
             OPEN Recibo;
             READ Recibo;
             Dow (NOT %EOF);


                READ Recibo;
             EndDo;
             Close Recibo;
       ENDIF;



       ENDSR;

       //============================================================
       // Subrotina: OpenFile
       // Objectivo: Abrir o ficheiro de recibos para Leitura
       //=============================================================
       BEGSR OpenFile;
          monitor;
             WIcErro = *OFF;
             WStdCmd = 'OVRDBF FILE(RECIBO) TOFILE/QS36F/'+ %TRIM(WFile)+')';
             shell(WStdCmd:%Size(WStdLen));
          on-error;
             // Formatar código do Erro
             WIcErro = *ON;
          endmon;
       ENDSR;


      /END-FREE
