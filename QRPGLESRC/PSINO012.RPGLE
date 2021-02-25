     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PSINO012
     H* Descrição.: Devolve a Sinistralidade de todos os contratos
     H*             Automóvel de um Segurado.
     H* Observação: Conversão do Codigo PHP (Frotas_V1.o.php)
     H*
     H* CALL PGM(PSINO012) PARM('3390' '00000000' '00000000' ' ')
     H*===========================================================
     FFGRLP200  IF   E           K DISK    USROPN PREFIX(GRL_)
     FTMPFILE   UF A F  400        DISK    USROPN EXTFILE(WFileName)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Registo a inserir no ficheiro
     D R#TEMP          DS                  QUALIFIED
     D   Texto                 1    400
     D
     D*==>  Campos de Trabalho
     D WCodRamo        S              2S 0
     D WNuApolice      S              7S 0
     D WNumSegurado    S              8S 0
     D WDatInicio      S              8S 0
     D WDatTermo       S              8S 0
     D WCodErro        S              7A
     D
     D WSessProc       S              5S 0
     D WRecProc        S              5S 0
     D WSessEXT        S              5S 0
     D WRecord         S              5S 0
     D
     D WIDS            S              1A
     D WNumOrdSin      S              9S 0
     D WDatSinistro    S              8S 0
     D WDatReabert     S              8S 0
     D WCodSinitro     S             15A
     D WNome           S             40A
     D WMatricula      S             20A
     D WTipo           S              8A
     D WNSeq           S              5S 0
     D WAnoProc        S              4S 0
     D WRecupera       S             11S 2
     D WPagamento      S             11S 2
     D WProvisao       S             11S 2
     D WCustoProc      S             11S 2
     D WPendentes      S             11S 2
     D WCodCausa       S              2A
     D WDescCausa      S             30A
     D
     D WFileName       S             20A
     D WPcName         S             40A
     D WCodTransf      S              2S 0
     D
     D*==> Definição das Constantes
     D OR#CompSeg      C                   'CompSeg'
     D OR#DemoComp     C                   'Demo Comp'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNumSegurado
     C                   PARM                    WDatInicio
     C                   PARM                    WDatTermo
     C                   PARM                    WCodErro
      /FREE
       //------------------------------------------------------------------------
       // Validar os Parâmetros Recebidos
       //------------------------------------------------------------------------
       EXSR ValParametros;
       IF (WCodErro <> *BLANKS);
          RETURN;
       ENDIF;


       //------------------------------------------------------------------------
       // Obter Processos para a Apólice
       //------------------------------------------------------------------------
       EXSR GetProcAUT;
       EXSR SaveResultado;

       RETURN;

       //=================================================================================
       // Subrotina..: ValParametros
       // Descrição..: Validar os Parâmetros Recebidos
       //=================================================================================
       BEGSR ValParametros;
          WCodErro  = *BLANKS;

          //------------------------------------------------------------------------
          // Validar a Existência do Segurado
          //------------------------------------------------------------------------
          WSessEXT = $SetFiles('GFSEG');
          $SetCriteria('(NSEG = ' + %CHAR(WNumSegurado) + ')');
          $SetFields('NOM');
          WRecord = 1;
          IF ($GetRecord(WRecord) > *ZEROS);
             WNome = $GetField('NOM');
          ELSE;
             WCodErro = 'PRDXXXX';   // Segurado inexistente
          ENDIF;
          $RmvDBSession(WSessExt);

          //------------------------------------------------------------------------
          // Validar o Período
          //------------------------------------------------------------------------
          IF (WCodErro <> *BLANKS);
             IF (WDatInicio > WDatTermo);
                WCodErro = 'PRD0175';
                LEAVESR;
             ENDIF;
          ENDIF;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProcAUT
       //  Descrição.: Obter a Informação sobre os Processo Automóvel
       //=================================================================================
       BEGSR GetProcAUT;
          EXSR EscreveHeader;
          WSessProc = $SetFiles('FSINT100');
          $SetCriteria('(IPROC = ''P'') AND +
                        (NSEGUR = ' + %CHAR(WNumSegurado) + ')');
          $SetFields('NORDSIN,DTSIN,DTINIC,DTFIM,DTRINI,NSEQ,AAPROC,+
                      RAMO,APOLICE,NSEGUR,CAUSA');
          $SetSort('AAPROC,NSEQ');
          WRecProc = 1;
          DOW ($GetRecord(WRecProc) > *ZEROS);
             WNumOrdSin = $GetFieldNum('NORDSIN');
             EXSR GetIDSProc;
             WCodRamo     = $GetFieldNum('RAMO');
             WNuApolice   = $GetFieldNum('APOLICE');
             EXSR GetMatricula;
             WCodCausa    = $GetField('CAUSA');
             WDatSinistro = $GetFieldNum('DTSIN');
             WDatInicio   = $GetFieldNum('DTINIC');
             WDatTermo    = $GetFieldNum('DTFIM');
             WDatReabert  = $GetFieldNum('DTRINI');
             WNSeq        = $GetFieldNum('NSEQ');
             WAnoProc     = $GetFieldNum('AAPROC');
             WCodSinitro  = %CHAR(WNSeq) + '/' + %CHAR(WAnoProc);
             WTipo = *BLANKS;
             EXSR GetProvisao;  // Provisões
             EXSR GetPagamento; // Pagamentos e Recuperações
             IF (WIDS = 'S');
                WTipo = 'IDS D';
                WRecupera = *ZEROS;
                IF (WDatTermo = *ZEROS);
                   IF (WPagamento > WProvisao);
                      WProvisao = WPagamento;
                   ENDIF;
                ELSE;
                   WProvisao = WPagamento;
                ENDIF;
             ENDIF;
             WPendentes = WProvisao - WPagamento;
             WCustoProc = WProvisao - WRecupera;
             EXSR GetDescCausa;
             EXSR EscreveDetalhe;
             WRecProc += 1;
          ENDDO;
          $RmvDBSession(WSessProc);
       ENDSR;

       //=================================================================================
       // Subrotina..: GetDescCausa
       // Descrição..:
       //=================================================================================
       BEGSR GetDescCausa;
          IF (WCodCausa = '01');
             WDescCausa = 'COLISAO';
          ELSEIF (WCodCausa = '02');
             WDescCausa = 'CHOQUE';
          ELSEIF (WCodCausa = '03');
             WDescCausa = 'ATROPELAMENTO';
          ELSEIF (WCodCausa = '04');
             WDescCausa = 'ATROPELAMENTO';
          ELSEIF (WCodCausa = '05');
             WDescCausa = 'QUEBRA VIDROS';
          ELSEIF (WCodCausa = '06');
             WDescCausa = 'CAPOTAMENTO';
          ELSEIF (WCodCausa = '07');
             WDescCausa = 'INCENDIO';
          ELSEIF (WCodCausa = '08');
             WDescCausa = 'FURTO TOTAL VIATURA';
          ELSEIF (WCodCausa = '09');
             WDescCausa = 'OUITROS';
          ELSEIF (WCodCausa = '10');
             WDescCausa = 'FURTO VIATURA';
          ELSEIF (WCodCausa = '11');
             WDescCausa = 'RCSP  ACTOS MALICIOSOS';
          ELSEIF (WCodCausa = '12');
             WDescCausa = 'RCSP  FENOM NATUREZA';
          ELSEIF (WCodCausa = '13');
             WDescCausa = 'DESPISTE';
          ELSEIF (WCodCausa = '14');
             WDescCausa = 'ROUBO VIATURA CARJACKING';
          ELSEIF (WCodCausa = '21');
             WDescCausa = 'QUEDA DE RAIO';
          ELSEIF (WCodCausa = '22');
             WDescCausa = 'EXPLOSÃO';
          ELSEIF (WCodCausa = '88');
             WDescCausa = 'AO ABRIGO IDS';
          ELSE;
             WDescCausa = '(' + WCodCausa + ') ?????';
          ENDIF;
       ENDSR;

       //=================================================================================
       // Subrotina..: EscreveHeader
       // Descrição..: Escrever Cabeçalho
       //=================================================================================
       BEGSR EscreveHeader;
          //-----------------------------------------------
          // Criar ficheiro para receber resultado
          //-----------------------------------------------
          WCodTransf = 18;
          WFileName = 'TMPFILE';
          WPcName = 'SEG'+%TRIM(%EDITW(WCodRamo:'0  '))+
                    %CHAR(WNumSegurado)+'.csv';
          WCodErro  = $CrtFile(WFileName:400);
          IF (WCodErro <> *BLANKS);
             LEAVESR;
          ENDIF;
          OPEN TMPFILE;

          //-----------------------------------------------
          // Criar ficheiro para receber resultado
          //-----------------------------------------------
          R#TEMP.Texto = 'Ramo;Tipo;Número Processo;Data Sinistro;+
                          Inicio;Termo;Reabertura;Apólice;Matricula;+
                          Segurado;Provisões;Pagamentos;Recuperações;+
                          Custo Processo;Pagamentos Pendentes;+
                          Causa Sinistro';
          WRITE TMPFILE R#TEMP;
       ENDSR;

       //=================================================================================
       // Subrotina..: EscreveDetalhe
       // Descrição..: Escrever Detalhe
       //=================================================================================
       BEGSR EscreveDetalhe;
          R#TEMP.Texto = 'Automovel;' + // %CHAR(WCodRamo) + ';'+
                         WTipo + ';'+
                         WCodSinitro + ';'+
                         %EDITW(WDatSinistro:'    -  -  ') + ';'+
                         %EDITW(WDatInicio:'    -  -  ') + ';'+
                         %EDITW(WDatTermo:'    -  -  ') + ';'+
                         %EDITW(WDatReabert:'    -  -  ') + ';'+
                         %CHAR(WNuApolice) + ';'+
                         WMatricula + ';'+
                         WNome + ';'+
                         %EDITW(WProvisao: '   .   .   ,  ')+';'+
                         %EDITW(WPagamento:'   .   .   ,  ')+';'+
                         %EDITW(WRecupera: '   .   .   ,  ')+';'+
                         %EDITW(WCustoProc: '   .   .   ,  ')+';'+
                         %EDITW(WPendentes:'   .   .   ,  ')+';'+
                         WDescCausa;
          WRITE TMPFILE R#TEMP;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProvisao
       // Descrição..:
       //=================================================================================
       BEGSR GetProvisao;
          WProvisao = *ZEROS;
          WSessEXT = $SetFiles('FSINT118');
          $SetCriteria('(NORDSIN = ' + %CHAR(WNumOrdSin) + ')');
          $SetFields('VALOR');
          WRecord = 1;
          DOW ($GetRecord(WRecord) > *ZEROS);
             WProvisao += $GetFieldNum('VALOR');
             WRecord += 1;
          ENDDO;
          $RmvDBSession(WSessEXT);
       ENDSR;

       //=================================================================================
       // Subrotina..: GetPagamento
       // Descrição..: Obter Pagamentos e Recuperações
       //=================================================================================
       BEGSR GetPagamento;
          WPagamento = *ZEROS;
          WRecupera  = *ZEROS;
          WSessEXT = $SetFiles('FSINT128');
          $SetCriteria('(NORDSIN = ' + %CHAR(WNumOrdSin) + ')');
          $SetFields('VALOR,TIPMOV');
          WRecord = 1;
          DOW ($GetRecord(WRecord) > *ZEROS);
             IF ($GetField('TIPMOV') = '221');
                WRecupera  += $GetFieldNum('VALOR');
             ELSEIF ($GetField('TIPMOV') <> '221');
                WPagamento += $GetFieldNum('VALOR');
             ENDIF;
             WRecord += 1;
          ENDDO;
          $RmvDBSession(WSessEXT);
       ENDSR;

       //=================================================================================
       // Subrotina..: GetMatricula
       // Descrição..: Obter a Matricula
       //=================================================================================
       BEGSR GetMatricula;
          WMatricula = *BLANKS;
          WSessEXT = $SetFiles('FSINT102');
          $SetCriteria('(NORDSIN = ' + %CHAR(WNumOrdSin) + ')');
          $SetFields('MATRIC');
          WRecord = 1;
          IF ($GetRecord(WRecord) > *ZEROS);
             WMatricula = $GetField('MATRIC');
          ENDIF;
          $RmvDBSession(WSessEXT);
       ENDSR;

       //=================================================================================
       // Subrotina..: GetIDSProc
       // Descrição..: Obter processo IDS
       //=================================================================================
       BEGSR GetIDSProc;
          WIDS = 'N';
          WSessEXT = $SetFiles('FSINT101');
          $SetCriteria('(NORDSIN = ' + %CHAR(WNumOrdSin) + ')');
          $SetFields('IDS');
          WRecord = 1;
          IF ($GetRecord(WRecord) > *ZEROS);
             WIDS = $GetField('IDS');
          ENDIF;
          $RmvDBSession(WSessEXT);
       ENDSR;

       //=================================================================================
       // Subrotina..: SaveResultado
       // Descrição..: Guardar o resultado num Temporario
       //=================================================================================
       BEGSR SaveResultado;
          CLOSE TMPFILE;

          //---------------------------------------------------
          // Enviar os ficheros para o servidor e abrir
          //---------------------------------------------------
          IF ($SendFTP(WCodTransf:'QTEMP':WFileName:%TRIM(WPcName)) <> *BLANKS);
             $ShowDialog('PRD0156');
          ELSE;
             MONITOR;
                OPEN FGRLP200;
                Chain (WCodTransf) RGRLP200;
                WPcName = '\\'+%TRIM(GRL_IPSERVER)+'\'+
                           %TRIM($StrReplace('/':'\':%TRIM(GRL_NoPasta)))+
                           %TRIM(WPcName);
                $ShellExec('strpco');
                $ShellExec('strpccmd pccmd('''+
                                    %TRIM(WPcName)+''') pause(*NO)');
             ON-ERROR;
             ENDMON;
             CLOSE FGRLP200;
             $ShowDialog('SIN0001':WPcName);
          ENDIF;
       ENDSR;
      /END-FREE
