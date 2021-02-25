     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR':'JMTMP')
     H*==================================================================
     H* Programa..: RCOBO120
     H* Descrição.: Ler ficheiros XML, com recurso a funções RPG ILE.
     H*
     H*==================================================================
     FTBIDDGF   UF A E           K DISK
     FTBIDDEF   UF   E           K DISK
     FSD1RE2    IF   E           K DISK
     FTBIDDPL3  IF   E           K DISK    RENAME(TBIDDP:RBIDDPL3)
     FSDCG2     UF   F   80        DISK    EXTFILE('SD.1.CG2')
     F*TB2201M   O    E             PRINTER USROPN
     F*==================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SGRLO018
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Formato de Registo da Númeração de Cartas Agradecimento
     D SdCG2Rec        DS
     D  CA_Doc001              1      2S 0
     D  CA_Res001              3     29A
     D  CA_Doc002             30     35S 0
     D  CA_Res002             36     50A
     D  CA_Doc003             51     56S 0
     D  CA_Res003             57     80A
     D
     D*==> Registo XML a Processar
     D LST#Lote        DS                  QUALIFIED
     D   NomFile                     10S 0 INZ(*ZEROS)
     D   MsgOriginal                 15A   INZ(*BLANKS)
     D   ValTotEnv                   13S 3 INZ(*ZEROS)
     D   QtdTotEnv                    4S 0 INZ(*ZEROS)
     D   ValTotRec                   13S 3 INZ(*ZEROS)
     D   QtdTotRec                    4S 0 INZ(*ZEROS)
     D   CodRetLote                   4A   INZ(*BLANKS)
     D   CodRetFile                   4A   INZ(*ZEROS)
     D   Rec                               LIKEDS(LST#Recibo) DIM(10)
     D
     D LST#Recibo      DS                  QUALIFIED
     D   ADCEnv                      11S 0 INZ(*ZEROS)
     D   DatADCRec                    8S 0 INZ(*ZEROS)
     D   ADCRec                      11S 0 INZ(*ZEROS)
     D   IBANEnv                     25A   INZ(*BLANKS)
     D   IBANRec                     25A   INZ(*BLANKS)
     D   CodSwift                    10A   INZ(*BLANKS)
     D   Nome                        50A   INZ(*BLANKS)
     D   Ramo                         2S 0 INZ(*ZEROS)
     D   Apolice                      7S 0 INZ(*ZEROS)
     D   Recibo                       9S 0 INZ(*ZEROS)
     D   ValRec                      10S 2 INZ(*ZEROS)
     D   ValEnv                      13S 2 INZ(*ZEROS)
     D   DatPago                      8S 0 INZ(*ZEROS)
     D   NumOrdem                     6S 0 INZ(*ZEROS)
     D   SitIDD                       1A   INZ(*BLANKS)
     D   SitAct                       1S 0 INZ(*BLANKS)
     D   SitRec                       4A   INZ(*BLANKS)
     D   Origem                       1A
     D   IsEmpty                       N
     D
     D*==> Campos de Trabalho
     D WOrigem         S              1A
     D WIdx            S              5S 0
     D WIdxRec         S              5S 0
     D WFileName       S            140A
     D WServer         S              5S 0
     D WMsgErro        S              7A
     D WCodFile        S             10S 0
     D WVariavel       S             40A
     D WNumCarta       S              6S 0
     D WQtdRegis       S              4S 0
     D WQtdEnviada     S              5S 0
     D WTotEnviada     S             13S 2
     D WTotPago        S             13S 2
     D
     D*==> Constantes
     D KTE_ENVIADO     C                   '3'
     D KTE_CONFIRMADO  C                   '4'
     D KTE_SUSPENDO    C                   '5'
     D
     D KTE#ActCobrado  C                   2
     D KTE#ActAnulado  C                   3
     D KTE#ActPendent  C                   0
     D KTE#RecCobrado  C                   '000'
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WFileName
     C                   PARM                    WServer
     C                   PARM                    WMsgErro
      /FREE

       //------------------------------------------------------
       // Transferir conteudo do ficheiro e validar se o
       // ficheiro XML já foi processado
       //------------------------------------------------------
       WMsgErro = $OpenXMLData(WFileName:WServer);
       IF (WMsgErro = *BLANKS);
          $GotoXMLTag('OrgnlGrpInfAndSts');
          LST#Lote.NomFile     = $GetXMLNumero('OrgnlMsgId':*BLANKS:'FN');

          //------------------------------------------------------
          // Validar se o ficheiro existe ou se foi processado
          //------------------------------------------------------
          EXSR ValFile; // Validar se o ficheiro já foi processado
          IF (WMsgErro <> *BLANKS);
             RETURN;
          ENDIF;

          //------------------------------------------------------
          // Validar se a informação está coerente
          //------------------------------------------------------
          EXSR ValData; // Validar se o ficheiro já foi processado
          IF (WMsgErro <> *BLANKS);
             RETURN;
          ENDIF;

          //------------------------------------------------------
          // Recepcionar os registos
          //------------------------------------------------------
          $GotoXMLTag('OrgnlPmtInfAndSts.StsRsnInf.Rsn');
          LST#Lote.CodRetLote  = $GetXMLAlfa('Prtry');
          IF (LST#Lote.CodRetLote <> 'M000');
             WMsgErro = 'COB0027'; // Ocorreram erros no procesamento do lote
             RETURN;
          ENDIF;

          $GotoXMLTag('OrgnlPmtInfAndSts');
          LST#Lote.CodRetFile  = $GetXMLAlfa('Prtry');
          IF (LST#Lote.CodRetLote <> 'L000');
             WMsgErro = 'COB0028'; // Ocorreram erros no procesamento dos registos
             RETURN;
          ENDIF;

          //---------------------------------------------
          // Actualizar detalhe dos registos
          //---------------------------------------------
          WIdxRec   = *ZEROS;
          WQtdRegis = $GetXMLQtdReg('TxInfAndSts');
          DOW (WIdxRec < WQtdRegis);
             WIdxRec += 1;
             $GotoXMLTag('OrgnlPmtInfAndSts.TxInfAndSts':WIdxRec);
             EXSR FillDetail;
          ENDDO;

          //----------------------------------------------------
          // Analisar os registos e actualizar a base de dados
          //---------------------------------------------------
          EXSR Processar;
       ENDIF;
       *INLR = *ON;

       //===============================================================
       // Subrotina.: ValFile
       // Descrição.: Validar se o ficheiro já foi processado
       //===============================================================
       BEGSR ValFile;
          WMsgErro = *BLANKS;
          WCodFile = LST#Lote.NomFile;

          //--------------------------------------------------
          // Validar se o ficheiro já foi processado
          //--------------------------------------------------
          CHAIN (WCodFile) TBIDDE;
          IF (%FOUND(TBIDDEF));
             IF (TETIPF <> 'XML');
                WMsgErro = 'COB0020'; // O ficheiro recebido não é XML
                LEAVESR;
             ENDIF;
             IF (TEDATR <> *ZEROS);
                WMsgErro = 'COB0022'; // O ficheiro já foi processado
                LEAVESR;
             ENDIF;
          ELSE;
             WMsgErro = 'COB0021'; // Não existe este ficheiro
          ENDIF;

          //--------------------------------------------------
          // Carregar Lista com os Recibos constantes do XML
          //--------------------------------------------------
          WQtdEnviada = *ZEROS;
          WTotEnviada = *ZEROS;
          WIdx        = 1;
          SETLL (WCodFile) RBIDDPL3;
          READE (WCodFile) RBIDDPL3;
          DOW (NOT %EOF(TBIDDPL3));
             WQtdEnviada += 1;
             LST#Lote.Rec(WIdx).DatADCRec = *ZEROS;
             LST#Lote.Rec(WIdx).ADCRec    = *ZEROS;
             LST#Lote.Rec(WIdx).IBANEnv   = *BLANKS;
             LST#Lote.Rec(WIdx).IBANRec   = *BLANKS;
             LST#Lote.Rec(WIdx).CodSwift  = *BLANKS;
             LST#Lote.Rec(WIdx).Nome      = *BLANKS;
             LST#Lote.Rec(WIdx).Ramo      = *ZEROS;
             LST#Lote.Rec(WIdx).Apolice   = *ZEROS;
             LST#Lote.Rec(WIdx).ValRec    = *ZEROS;
             LST#Lote.Rec(WIdx).DatPago   = *ZEROS;
             LST#Lote.Rec(WIdx).SitRec    = *BLANKS;
             CHAIN (54:%CHAR(IDREC)) SDRECF1;
             IF (NOT %FOUND(SD1RE2));
                WMsgErro = 'COB0039';
                LEAVESR;
             ENDIF;
             LST#Lote.Rec(WIdx).ADCEnv   = IDNADC;
             LST#Lote.Rec(WIdx).Recibo   = IDREC;
             LST#Lote.Rec(WIdx).ValEnv   = IDVAL;
             LST#Lote.Rec(WIdx).NumOrdem = NORD;
             LST#Lote.Rec(WIdx).SitAct   = CODSIT;
             LST#Lote.Rec(WIdx).SitIDD   = IDSIT;
             LST#Lote.Rec(WIdx).Origem   = 'E'; // Enviada
             LST#Lote.Rec(WIdx).IsEmpty  = *OFF;
             WIdx += 1;
             LST#Lote.Rec(WIdx).IsEmpty  = *ON;
             WQtdEnviada += IDVAL;
             READE (WCodFile) RBIDDPL3;
          ENDDO;
       ENDSR;

       //===============================================================
       // Subrotina.: ValData
       // Descrição.: Validar se o registos do ficheiro estão coerentes
       //===============================================================
       BEGSR ValData;
          LST#Lote.MsgOriginal = $GetXMLAlfa('OrgnlMsgNmId');
          LST#Lote.ValTotEnv   = $GetXMLNumero('OrgnlCtrlSum');
          LST#Lote.QtdTotEnv   = $GetXMLNumero('OrgnlNbOfTxs');
          IF (WQtdEnviada <> LST#Lote.QtdTotEnv);
             WMsgErro = 'COB0023'; // A quantidade enviada é diferente
             LEAVESR;
          ELSEIF (WTotEnviada <> LST#Lote.ValTotEnv);
             WMsgErro = 'COB0024'; // O Valor total enviado não coincide
             LEAVESR;
          ENDIF;

          $GotoXMLTag('OrgnlPmtInfAndSts');
          LST#Lote.ValTotRec   = $GetXMLNumero('OrgnlCtrlSum');
          LST#Lote.QtdTotRec   = $GetXMLNumero('OrgnlNbOfTxs');
          IF (WQtdEnviada <> LST#Lote.QtdTotRec);
             WMsgErro = 'COB0025'; // A quantidade enviada é diferente
             LEAVESR;
          ELSEIF (WTotEnviada <> LST#Lote.ValTotRec);
             WMsgErro = 'COB0026'; // O Valor total enviado não coincide
             LEAVESR;
          ENDIF;
       ENDSR;

       //===============================================================
       // Subrotina.: FillDetail
       // Descrição.: Carregar lista de recibos com o retorno do banco
       //===============================================================
       BEGSR FillDetail;
          WIdx = 1;
          WOrigem    = 'R'; // Recebida
          DOW (NOT LST#Lote.Rec(WIdx).IsEmpty);
             IF (LST#Lote.Rec(WIdx).Recibo=$GetXMLNumero('Ustrd':*BLANKS:'RC'));
                WOrigem = 'P'; // Processada
                LEAVE;
             ENDIF;
             WIdx += 1;
          ENDDO;

          LST#Lote.Rec(WIdx).ADCRec = $GetXMLNumero('MndtId':*BLANKS:'AU');
          LST#Lote.Rec(Widx).Origem = WOrigem;
          LST#Lote.Rec(WIdx).DatADCRec =$GetXMLNumero('DtOfSgntr':*BLANKS:'DT');
          LST#Lote.Rec(WIdx).IBANRec = $GetXMLAlfa('IBAN');
          LST#Lote.Rec(WIdx).CodSwift = $GetXMLAlfa('BIC');
          LST#Lote.Rec(WIdx).Nome = $GetXMLAlfa('Nm');
          LST#Lote.Rec(WIdx).Ramo = $GetXMLNumero('Ustrd':*BLANKS:'RM');
          LST#Lote.Rec(WIdx).Apolice = $GetXMLNumero('Ustrd':*BLANKS:'AP');
          LST#Lote.Rec(WIdx).Recibo = $GetXMLNumero('Ustrd':*BLANKS:'RC');
          LST#Lote.Rec(WIdx).ValRec =
                        $GetXMLNumero('InstdAmt Ccy=''EUR''':'InstdAmt');
          LST#Lote.Rec(WIdx).DatPago=$GetXMLNumero('ReqdColltnDt':*BLANKS:'DT');
          LST#Lote.Rec(WIdx).SitRec    = $GetXMLAlfa('Cd');
          EXSR Processar;
       ENDSR;

       //===============================================================
       // Subrotina.: Processar
       // Descrição.: Analisar a consistência dos dados recebidos e
       //             actualizar a informação. Imprime também o resultado
       //             da operação, numa formulário a enviar à Tesouraria
       //===============================================================
       BEGSR Processar;
          //-------------------------------------------------
          // Actualizar informação sobre o lote processado
          //-------------------------------------------------
          WTotPago = *ZEROS;
          CHAIN 1 SDCG2 SdCG2Rec;
          WNumCarta = CA_Doc003 + 1;
          CHAIN (WNumCarta) TBIDDG;
          IF (%FOUND(TBIDDGF));
             WMsgErro = 'COB0042';
             RETURN;
          ENDIF;

          //-------------------------------------------------
          // Localizar o recibo
          //-------------------------------------------------
          IdxRec = 1;
          DOW (NOT LST#Lote.Rec(WIdxRec).IsEmpty);
             WMsgErro = *BLANKS;
             //--------------------------------------------------
             // Validar se o registo foi processado
             //--------------------------------------------------
             IF (LST#Lote.Rec(WIdxRec).Origem = 'E');
                WMsgErro = 'COB0032'; // O registo não foi retornado
                // Actualizar o estado
             ELSEIF (LST#Lote.Rec(WIdxRec).Origem = 'R');
                WMsgErro = 'COB0033'; // O registo recebido não foi enviado
                // Actualizar o estado
             //--------------------------------------------------
             // Validar a ADC Enviada
             //--------------------------------------------------
             ELSEIF (LST#Lote.Rec(WidxRec).ADCRec <>
               LST#Lote.Rec(WIdxRec).ADCEnv);
                WMsgErro = 'COB0034'; // A ADC enviada é diferente da recebida
                // Actualizar o estado
             //--------------------------------------------------
             // Validar o valor do recibo
             //--------------------------------------------------
             ELSEIF (LST#Lote.Rec(WidxRec).ValRec <>
               LST#Lote.Rec(WIdxRec).ValEnv);
                WMsgErro = 'COB0035'; // A ADC enviada é diferente da recebida
                // Actualizar o estado
             //--------------------------------------------------
             // Validar o IBAN usado para a Cobrança
             //--------------------------------------------------
             ELSEIF (LST#Lote.Rec(WidxRec).IBANRec <>
               LST#Lote.Rec(WIdxRec).IBANEnv);
                WMsgErro = 'COB0036'; // O IBAN enviado é diferente da recebido
                // Actualizar o estado
             //--------------------------------------------------
             // Validar o estado do recibo
             //--------------------------------------------------
             ELSEIF (LST#Lote.Rec(WidxRec).SitAct = KTE#ActCobrado);
                IF (LST#Lote.Rec(WIdxRec).SitRec = KTE#RecCobrado);
                   WMsgErro = 'COB0037'; // O Recibo já foi Cobrado em José Mata
                   // Actualizar o estado
                ENDIF;
             //--------------------------------------------------
             // Validar o estado do recibo
             //--------------------------------------------------
             ELSEIF (LST#Lote.Rec(WidxRec).SitAct <> KTE#ActPendent);
                IF (LST#Lote.Rec(WIdxRec).SitRec = KTE#RecCobrado);
                   WMsgErro = 'COB0038'; // O Recibo foi Anulado em José Mata
                   // Actualizar o estado
                ENDIF;
             ELSE;
                // Actualizar a informação do registo
                WTotPago += LST#Lote.Rec(WidxRec).ValRec;
             ENDIF;
             WIdxRec += 1;
          ENDDO;

          CA_Doc003 = WNumCarta;
          UPDATE SDCG2 SdCG2Rec;
          IGVAL = WTotPago;
          IGNAGR = WNumCarta;
          WRITE TBIDDG;
          // Actualziar TBIDDEF;
       ENDSR;

       //===============================================================
       // Subrotina.: UpdateStatus
       // Descrição.: Actualizar o estado do recibo
       //===============================================================
       BEGSR SSocessar;
       ENDSR;
      /END-FREE
