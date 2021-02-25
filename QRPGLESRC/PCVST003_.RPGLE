     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *=======================================================
      * Autor .....: Luis Galhoz
      * Data ......: 2013/07/17
      * Programa ..: PPRDT003
      * Objectivo .: Actualizar Informação de todas as Apólices
      *=======================================================
     FBASEAPOL  IF   F 1000        DISK    USROPN
     FFPRDP008  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Coberturas
     FFPRDP010  IF A E           K DISK    PREFIX(PRD_) COMMIT                  Notas Descritivas
     FFPRDP011  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Riscos da Apólice
     FFPRDP012  IF A E           K DISK    PREFIX(PRD_) COMMIT                  Texto das N.Descritivas
     FFPRDP009  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Cláusulas e Condições
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura de dados do Registo
     D RBASE         E DS                  ExtName(GBASEDIO)
     D                                     Prefix(BAS_)
     D
     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D
     D*==> Actualizar a nova estrutura
     D UpdNewDB        PR
     D   CdProd                       5S 0 CONST
     D   DatCriacao                   8S 0 CONST
     D   Registo                       *   CONST
     D
     D*==> Campos de Trabalho
     D WCodSessao      S              5S 0 INZ(*ZEROS)
     D WNumRegis       S              5S 0 INZ(*ZEROS)
     D WDatRegisto     S              8S 0 INZ(*ZEROS)
     D WCdProd         S              5S 0
     I*========================================================
     IBASEAPOL  AA  11
     I                                  1 1000  RBASE
     C*========================================================
      /FREE
        //------------------------------------------------------
        // Actualizar o adicionar novos Registos
        //------------------------------------------------------
        WCodSessao = $SetFiles('FGRLP023');
        $SetFields('CDFILE,NOLIBL,NOFILE,CHAR(DTCRIA),CHAR(HRCRIA)');
        $SetCriteria('CDFILE=21 OR CDFILE=22');
        $SetSort('DTCRIA #DESC, HRCRIA #DESC');
        WNumRegis = 1;
        DOW ($GetRecord(WNumRegis) > *ZEROS);
           MONITOR;
              WStdCmd = 'OVRDBF FILE(BASEAPOL) +
                         TOFILE('+%TRIM($GetField('NOLIBL'))+'/'+
                                  %TRIM($GetField('NOFILE'))+')';
              WStdLen = %LEN(WStdCmd);
              EXEC(WStdCmd:WStdLen);
              OPEN BASEAPOL;
              EXSR UpdApolices;
              CLOSE BASEAPOL;
           ON-ERROR;
           ENDMON;
           WNumRegis += 1;
        ENDDO;

        *INLR = *ON;
        RETURN;

        //======================================================
        // Subrotina..: UpdApolices
        // Objectivo..: Actualizar nova estrutura das Apólices
        //======================================================
        BEGSR UpdApolices;
           READ BASEAPOL;
           DOW (NOT %EOF());
              UpdNewDB(BAS_RAMO:WDatRegisto);
              READ BASEAPOL;
           ENDDO;
        ENDSR;

      /END-FREE
     P*==========================================================
     P* Processo..: UpdClausula
     P* Objectivo.: Actualziar as Clausulas da Apólice
     P*==========================================================
     P UpdClausula     B                   EXPORT
     D                 PI             5S 0
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdItem                   3A   CONST
     D   PRM_Alias                   10A   CONST
     D   PRM_Natureza                 1A   CONST
     D WNuSequ         S              5S 0 INZ(*ZEROS)
     D WSessClaus      S              5S 0 INZ(*ZEROS)
     D*==========================================================
      /FREE
        WNuSequ = *ZEROS;
        IF (PRM_CdProd = 5) OR (PRM_CdProd = 11);
           EXSR GetOldSet1;
        ELSE;
           EXSR GetOldSet2;
        ENDIF;
        EXSR GetRecord;
        RETURN WNuSequ;

       //========================================================
       // Subrotina.: GetRecord
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR GetRecord;
          //----------------------------------------------------------------
          // Obter o Registo para a Cobertura activo na data
          //----------------------------------------------------------------
          WSessClaus = $SetFiles('FPRDP009');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDRAMO='''+%CHAR(OR_RAMO)+''') +
                    AND (NUAPOL='''+%CHAR(OR_Apol)+''') +
                    AND (CDITEM='''+PRM_CdItem+ ''')');
          IF ($getRecord(1:1) = *ZEROS);
             IF (WNuSequ = 9999) AND (NOT WIcDisplay);
                PRD_CDRAMO   = OR_Ramo;
                PRD_NUAPOL   = OR_Apol;
                PRD_CDITEM   = PRM_CdItem;
                WRITE RPRDP009;
                $getRecord(1:1);
             ENDIF;
          ENDIF;

          MONITOR;
             WNuSequ = %DEC($GetField('NUSEQU'):5:0);
          ON-ERROR;
             WNuSequ = *ZEROS;
          ENDMON;
          $RmvDBSession(WSessClaus);
       ENDSR;

       //========================================================
       // Subrotina.: GetOldSet1
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR GetOldSet1;
          WNuSequ = *ZEROS;
          IF (PRM_Natureza = 'E'); // Condições Especiais
             IF (OR_CE001 = 'S') AND (%TRIM(PRM_ALIAS) = '001');
                WNuSequ = 9999;
             ELSEIF (OR_CE002 = 'S') AND (%TRIM(PRM_ALIAS) = '002');
                WNuSequ = 9999;
             ELSEIF (OR_CE003 = 'S') AND (%TRIM(PRM_ALIAS) = '003');
                WNuSequ = 9999;
             ELSEIF (OR_CE004 = 'S') AND (%TRIM(PRM_ALIAS) = '004');
                WNuSequ = 9999;
             ENDIF;
          ELSEIF (PRM_Natureza = 'A'); // Cláusulas Aplicáveis
             IF (OR_CA030 = 'S') AND (%TRIM(PRM_ALIAS) = '030');
                WNuSequ = 9999;
             ELSEIF (OR_CA031 = 'S') AND (%TRIM(PRM_ALIAS) = '031');
                WNuSequ = 9999;
             ELSEIF (OR_CA100 = 'S') AND (%TRIM(PRM_ALIAS) = '100');
                WNuSequ = 9999;
             ELSEIF (OR_CA101 = 'S') AND (%TRIM(PRM_ALIAS) = '101');
                WNuSequ = 9999;
             ELSEIF (OR_CA102 = 'S') AND (%TRIM(PRM_ALIAS) = '102');
                WNuSequ = 9999;
             ENDIF;
          ELSEIF (PRM_Natureza = 'D'); // Cláusulas Diversas
             IF (OR_CD103 = 'S') AND (%TRIM(PRM_ALIAS) = '103');
                WNuSequ = 9999;
             ELSEIF (OR_CD130 = 'S') AND (%TRIM(PRM_ALIAS) = '130');
                WNuSequ = 9999;
             ELSEIF (OR_CD131 = 'S') AND (%TRIM(PRM_ALIAS) = '131');
                WNuSequ = 9999;
             ELSEIF (OR_CD132 = 'S') AND (%TRIM(PRM_ALIAS) = '132');
                WNuSequ = 9999;
             ELSEIF (OR_CD133 = 'S') AND (%TRIM(PRM_ALIAS) = '133');
                WNuSequ = 9999;
             ELSEIF (OR_CD134 = 'S') AND (%TRIM(PRM_ALIAS) = '134');
                WNuSequ = 9999;
             ELSEIF (OR_CD135 = 'S') AND (%TRIM(PRM_ALIAS) = '135');
                WNuSequ = 9999;
             ELSEIF (OR_CD136 = 'S') AND (%TRIM(PRM_ALIAS) = '136');
                WNuSequ = 9999;
             ELSEIF (OR_CD137 = 'S') AND (%TRIM(PRM_ALIAS) = '137');
                WNuSequ = 9999;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: GetOldSet2
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR GetOldSet2;
          WNuSequ = *ZEROS;
          IF (PRM_ALIAS <> *BLANKS);
             IF (OR_CL01 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL02 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL03 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL04 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL05 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL06 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
     P UpdClausula     E

     P*=======================================================
     P* Processo..: UpdRisco
     P* Objectivo.: Validar se o Risco está contratado para a
     P*             Apólice.
     P*=======================================================
     P UpdRisco        B                   EXPORT
     D                 PI             5S 0
     D   PRM_CdRisco                  5S 0 CONST
     D WSessRisco      S              5S 0 INZ(*ZEROS)
     D WIsNew          S               N   INZ(*OFF)
     D WCdTexto        S              5S 0 INZ(*ZEROS)
     D WDsTexto        S            100A   INZ(*BLANKS)
     D WNuSequ         S              5S 0 INZ(*ZEROS)
     D WCodProduto     S              5S 0 INZ(*ZEROS)
     D WVLRISCOALF     S             14A
     D*=======================================================
      /FREE
        WNuSequ = *ZEROS;
        WIsNew  = *OFF;
        EXSR ApplyOldSett;
        RETURN WNuSequ;

       //========================================================
       // Subrotina.: ApplyOldSett
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR ApplyOldSett;
          WNuSequ = *ZEROS;
          IF (OR_CR1 = PRM_CdRisco) OR (OR_CR2 = PRM_CdRisco) OR
             (OR_CR3 = PRM_CdRisco);
             WNuSequ = 9999;
             IF (OR_CR1 = PRM_CdRisco);
                WVLRISCO = OR_CAP1;
                WTXRISCO = OR_TX1;
             ELSEIF (OR_CR2 = PRM_CdRisco);
                WVLRISCO = OR_CAP2;
                WTXRISCO = OR_TX2;
             ELSEIF (OR_CR3 = PRM_CdRisco);
                WVLRISCO = OR_CAP3;
                WTXRISCO = OR_TX3;
             ENDIF;
          ENDIF;

          //----------------------------------------------------------------
          // Obter o Registo para a Cobertura activo na data
          //----------------------------------------------------------------
          WSessRisco  = $SetFiles('FPRDP011');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDRAMO='''+%CHAR(OR_Ramo)+''') +
                    AND (NUAPOL='''+%CHAR(OR_Apol)+''') +
                  AND (CDRISCO ='''+%CHAR(PRM_Cdrisco)+ ''')')   ;
          IF ($getRecord() = *ZEROS);
             IF (WNuSequ = 9999) AND NOT (WIcDisplay);
                PRD_CDRAMO  = OR_Ramo;
                PRD_NUAPOL  = OR_Apol;
                PRD_CDRISCO = PRM_CdRisco;
                PRD_IMCPRISC= WVLRISCO;
                PRD_TXRISCO = WTXRISCO;
                WIsNew  = *ON;
                WRITE RPRDP011;
                $getRecord();
             ENDIF;
          ENDIF;

          WNuSequ = $GetFieldNum('NUSEQU');
          WVLRISCO = $GetFieldNum('IMCPRISC');
          WTXRISCO = $GetfieldNum('TXRISCO');
          $RmvDBSession(WSessRisco);

          //----------------------------------------------------
          // Formata valor na lista de ecrã
          //----------------------------------------------------
          IF (WNuSequ > *ZEROS);
             IF (WVLRISCO > *ZEROS);
                WVLRISCOALF = %EDITW(WVLRISCO:'   .   .   ,  ');
                IF (WIsNew);
                   //--> Adicionar Registo correspondende nas Notas Descritivas
                   EXSR AddNotaDescritiva;
                ENDIF;
             ELSE;
                WVLRISCOALF = ATR#Blink + '?? Capital ??';
             ENDIF;
          ELSE;
             WVLRISCOALF = *BLANKS;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: AddNotaDescritiva
       // Objectivo.: Adicionar uma nota Descritiva
       //========================================================
       BEGSR AddNotaDescritiva;
          WCdTexto = *ZEROS;
          WSessRisco  = $SetFiles('FCATP105 A INNER JOIN FCATP106 B +
                                   ON (A.CDPROD = B.CDPROD) AND +
                                   (A.CDTEXTO = B.CDTEXTO)');
          $AddDBDate($IntToDate(WDatInicio));
          $SetFields('FCATP105':'A');
          $SetFields('FCATP106':'B':*OFF);
          $SetCriteria('(A.CDPROD='''+%CHAR(WCodProduto)+''') +
                  AND (A.CDRISCO ='''+%CHAR(PRM_Cdrisco)+ ''') +
                  AND (A.ICDEFEIT = ''S'')');
          IF ($getRecord() > *ZEROS);
             WCdTexto = $GetFieldNum('B.CDTEXTO');
             WDSTexto = $GetField('B.DSTEXTO');
          ENDIF;
          $RmvDBSession(WSessRisco);

          //-------------------------------------------------
          // Actualizar informação da Nota Descritiva
          //-------------------------------------------------
          IF (WCdTexto > *ZEROS);
             PRD_CDRAMO  = OR_Ramo;
             PRD_NUAPOL  = OR_Apol;
             PRD_NUSEQU  = *ZEROS;
             PRD_NUSQSUP = *ZEROS;
             PRD_CDTEXTO = WCdTexto;
             WRITE RPRDP010;

             PRD_CDRAMO = OR_Ramo;
             PRD_NUAPOL = OR_Apol;
             PRD_NUSEQU = $GetNextKey('FPRDP010':'NUSEQU':'(CDRAMO='''+
                          %TRIM(%CHAR(OR_Ramo)) + ''') AND (NUAPOL='''+
                          %TRIM(%CHAR(OR_Apol)) + ''')');
             PRD_NUSEQU  -= 1;
             PRD_ICDETAIL = 'N';
             PRD_DSDETAIL = WDsTexto;
             PRD_CPSEGURO = WVLRISCO;
             WRITE RPRDP012;
          ENDIF;
       ENDSR;
      /END-FREE
     P UpdRisco        E

     P*=======================================================
     P* Processo..: UpdCobertura
     P* Objectivo.: Validar se a Cobertura está contratada para
     P*             a Apólices (trabalha com as duas versões das
     P*             Tabelas). Actualizar nova Tabela, partindo
     P*             do que encontrar na antiga.
     P*=======================================================
     P UpdCobertura    B                   EXPORT
     D                 PI             5S 0
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_Alias                   10A   CONST
     D   PRM_Natureza                 1A   CONST
     D
     D WSessCobert     S              5S 0 INZ(*ZEROS)
     D WNuSequ         S              5S 0 INZ(*ZEROS)
     D*=======================================================
      /FREE
        WNuSequ = *ZEROS;
        IF (PRM_CdProd = 5) OR (PRM_CdProd = 11);
           EXSR GetOldSet1;
        ELSEIF (PRM_CdProd = 50) OR (PRM_CdProd = 51);
           EXSR GetOldSet3;
        ELSE;
           EXSR GetOldSet2;
        ENDIF;
        EXSR GetRecord;
        RETURN WNuSequ;

       //========================================================
       // Subrotina.: GetRecord
       // Objectivo.: Obter o registo existente na nova estrutura
       //             de dados.
       //========================================================
       BEGSR GetRecord;
          //----------------------------------------------------------------
          // Obter o Registo para a Cobertura activo na data
          //----------------------------------------------------------------
          WSessCobert = $SetFiles('FPRDP008');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDRAMO='''+%CHAR(OR_RAMO)+''') +
                    AND (NUAPOL='''+%CHAR(OR_Apol)+''') +
                  AND (CDCOBERT='''+%CHAR(PRM_CdCobert)+ ''')');
          IF ($getRecord() = *ZEROS);
             IF (WNuSequ = 9999) AND (NOT WIcDisplay);
                PRD_CDRAMO   = OR_Ramo;
                PRD_NUAPOL   = OR_Apol;
                PRD_CDCOBERT = PRM_CdCobert;
                WRITE RPRDP008;
                $getRecord();
             ENDIF;
          ENDIF;
          MONITOR;
             WNuSequ = $GetFieldNum('NUSEQU');
          ON-ERROR;
             WNuSequ = *ZEROS;
          ENDMON;
          $RmvDBSession(WSessCobert);
       ENDSR;

       //========================================================
       // Subrotina.: GetOldSet1
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR GetOldSet1;
          WNuSequ = *ZEROS;
          IF (OR_CF01 = 'S') AND (%TRIM(PRM_ALIAS) = '01');
             WNuSequ = 9999;
          ELSEIF (OR_CF02 = 'S') AND (%TRIM(PRM_ALIAS) = '02');
             WNuSequ = 9999;
          ELSEIF (OR_CF03 = 'S') AND (%TRIM(PRM_ALIAS) = '03');
             WNuSequ = 9999;
          ELSEIF (OR_CF04 = 'S') AND (%TRIM(PRM_ALIAS) = '04');
             WNuSequ = 9999;
          ELSEIF (OR_CF05 = 'S') AND (%TRIM(PRM_ALIAS) = '05');
             WNuSequ = 9999;
          ELSEIF (OR_CF06 = 'S') AND (%TRIM(PRM_ALIAS) = '06');
             WNuSequ = 9999;
          ELSEIF (OR_CF08 = 'S') AND (%TRIM(PRM_ALIAS) = '08');
             WNuSequ = 9999;
          ELSEIF (OR_CF09 = 'S') AND (%TRIM(PRM_ALIAS) = '09');
             WNuSequ = 9999;
          ELSEIF (OR_CF10 = 'S') AND (%TRIM(PRM_ALIAS) = '10');
             WNuSequ = 9999;
          ELSEIF (OR_CF11 = 'S') AND (%TRIM(PRM_ALIAS) = '11');
             WNuSequ = 9999;
          ELSEIF (OR_CF12 = 'S') AND (%TRIM(PRM_ALIAS) = '12');
             WNuSequ = 9999;
          ELSEIF (OR_CF13 = 'S') AND (%TRIM(PRM_ALIAS) = '13');
             WNuSequ = 9999;
          ELSEIF (OR_CF07 = 'S') AND (%TRIM(PRM_ALIAS) = '07');
             WNuSequ = 9999;
          ELSEIF (OR_CF0800 = 'S') AND (%TRIM(PRM_ALIAS) = '07');
             WNuSequ = 9999;
          ELSEIF (OR_CF0830 = 'S') AND (%TRIM(PRM_ALIAS) = '0830');
             WNuSequ = 9999;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: GetOldSet2
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR GetOldSet2;
          WNuSequ = *ZEROS;
          IF (PRM_ALIAS <> *BLANKS);
             IF (OR_CL01 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL02 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL03 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL04 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL05 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL06 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: GetOldSet3
       // Objectivo.: Obter as Coberturas existentes para o Seguro
       //             Automóvel.
       //========================================================
       BEGSR GetOldSet3;
       ENDSR;
      /END-FREE
     P UpdCobertura    E
