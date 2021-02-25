     H ALWNULL(*USRCTL)  BNDDIR('JMDIR':'JMINT') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RINTO050
     H* Descrição.: Gerar interfaces para os Mediadores, para o efeito
     H*             Localizar os ficheiros temporários gerados nos
     H*             Sistema, e por cada novo que localizar, procede
     H*             à extracção dos registos.
     H*
     H*===========================================================
     F*==> Temporários a Processar
     FBASETMPA  IF   F  700     9AIDISK    KEYLOC(1) EXTFILE(WFileDia) USROPN
     FBASETMPO  IF   F  700     9AIDISK    KEYLOC(1) EXTFILE(WFileDia) USROPN
     FBASETMPT  IF   F  435     9AIDISK    KEYLOC(1) EXTFILE(WFileDia) USROPN
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SINTO010

     D*==> Actualizar Lista de Campos
     D ActLstField     PR
     D   Nome                        20A   CONST
     D   Valor                      100A   CONST
     D   Reset                         N   CONST OPTIONS(*NOPASS)
     D
     D*==> Formato de Registo da Apólices
     D R#DIO         E DS                  EXTNAME(GBASEDIO) QUALIFIED
     D R#DIA         E DS                  EXTNAME(GBASEDIA) QUALIFIED
     D R#DIT         E DS                  EXTNAME(GBASEDIT) QUALIFIED
     D
     D*==> Campos de Trabalho
     D WCodProcess     S              5S 0
     D WCodFile        S              5S 0
     D WTipFile        S              3A
     D WFileDia        S             21A
     D WDatFile        S              8S 0
     D WCodErro        S              7A
     D
     D WIdx            S              3S 0
     D WTipInter       S              3A
     D WCodMedia       S              9S 0
     D WCurSess        S              3S 0
     D WAlias          S              3S 0
     D WSesMediad      S              5S 0
     D
     D*==> Declaração de Constantes
     D TIP#Automovel   C                   'AAU'
     D TIP#Outros      C                   'AOR'
     D TIP#Transport   C                   'ATR'
     D INT#Apolice     C                   'APL'
     D INT#Recibos     C                   'REC'
     D INT#Segurado    C                   'SEG'
     D INT#Apolices    C                   'APL'
     D STS#Aberto      C                   'OPN'
     D STS#Fechado     C                   'CLO'
     D MED#Standard    C                   1
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProcess
     C                   PARM                    WCodFile
     C                   PARM                    WFileDia
     C                   PARM                    WTipFile
     C                   PARM                    WDatFile
     C                   PARM                    WCodErro
      /FREE
        //-----------------------------------------------------------------
        // Seleccionar os Mediadores com instruções para gerar Interfaces
        //-----------------------------------------------------------------
        WIdx       = 1;
        WSesMediad = $SetFiles('FINTP050');
        $SetCriteria('(ISAPOLIC = ''S'') AND +
                      (DTFIMSPN <= '''+%CHAR(WDatFile)+''') +
                      AND (CDMEDIA <> 1)');
        DOW ($GetRecord(WIdx) > *ZEROS);
           WCodErro  = *BLANKS;
           WCodMedia = $GetFieldNum('CDMEDIA');
           WAlias    = $GetFieldNum('ALIAS');
           WTipInter = INT#Apolices;
           WCodErro  = $StrIntMedia(WCodMedia:WCodProcess:WCodFile:WTipInter);
           IF (WCodErro = *BLANKS);
              EXSR ReadTemporario;
              IF (WCodErro = *BLANKS);
                 WCodErro = $EndIntMedia(WCodMedia:WTipInter);
              ENDIF;
           ENDIF;
           WIdx += 1;
        ENDDO;
        $RmvDBSession(WSesMediad);
        RETURN;

        //================================================================
        // Subrotina.: ReadTemporario
        // objectivo.: Ler o ficheiro Temporário para identificar registos
        //================================================================
        BEGSR ReadTemporario;
           IF (WTipFile = TIP#Automovel);
              OPEN BASETMPA;
              EXSR ReadAutomovel;
              CLOSE BASETMPA;
           ELSEIF (WTipFile = TIP#Outros);
              OPEN BASETMPO;
              EXSR ReadOutros;
              CLOSE BASETMPO;
           ELSEIF (WTipFile = TIP#Transport);
              OPEN BASETMPT;
              EXSR ReadTransport;
              CLOSE BASETMPT;
           ENDIF;
        ENDSR;

        //================================================================
        // Subrotina.: ReadAutomovel
        // objectivo.: Ler Informação especifica do Ramos Automovel
        //================================================================
        BEGSR ReadAutomovel;
           READ BASETMPA R#DIA;
           DOW (NOT %EOF(BASETMPA));
              IF (R#DIA.MED1 = WAlias) OR (R#DIA.JMM1 = WAlias) OR
                (R#DIA.JMM2 = WAlias);
                 WCurSess = $GetIntMedia(WCodMedia:WTipInter);

                 $AddFldIntRec(WCurSess:'RAMO':%CHAR(R#DIA.RAMO):*ON);
                 $AddFldIntRec(WCurSess:'APOL':%CHAR(R#DIA.APOL));
                 $AddFldIntRec(WCurSess:'NSEG':%CHAR(R#DIA.NSEG));
                 $AddFldIntRec(WCurSess:'SEG':R#DIA.SEG);

                 $AddIntRecMed(WCodMedia:WtipInter);
              ENDIF;
              READ BASETMPA R#DIA;
           ENDDO;
        ENDSR;

        //================================================================
        // Subrotina.: ReadOutros
        // objectivo.: Ler Informação Específica dos Outros Ramos
        //================================================================
        BEGSR ReadOutros;
           READ BASETMPO R#DIO;
           DOW (NOT %EOF(BASETMPO));
              IF (R#DIO.MED1 = WAlias) OR (R#DIO.JMM1 = WAlias) OR
                (R#DIO.JMM2 = WAlias);
                 WCurSess = $GetIntMedia(WCodMedia:WTipInter);

                 $AddFldIntRec(WCurSess:'RAMO':%CHAR(R#DIO.RAMO):*ON);
                 $AddFldIntRec(WCurSess:'APOL':%CHAR(R#DIO.APOL));
                 $AddFldIntRec(WCurSess:'NSEG':%CHAR(R#DIO.NSEG));
                 $AddFldIntRec(WCurSess:'SEG':R#DIO.SEG);

                 $AddIntRecMed(WCodMedia:WtipInter);
              ENDIF;
              READ BASETMPO R#DIO;
           ENDDO;
        ENDSR;

        //================================================================
        // Subrotina.: ReadTransport
        // objectivo.: Ler Informação Especifica do Ramo de Transportes
        //================================================================
        BEGSR ReadTransport;
           READ BASETMPT R#DIT;
           DOW (NOT %EOF(BASETMPT));
              IF (R#DIT.MED1 = WAlias) OR (R#DIT.JMM1 = WAlias) OR
                (R#DIT.JMM2 = WAlias);
                 WCurSess = $GetIntMedia(WCodMedia:WTipInter);

                 $AddFldIntRec(WCurSess:'RAMO':%CHAR(R#DIT.RAMO):*ON);
                 $AddFldIntRec(WCurSess:'APOL':%CHAR(R#DIT.APOL));
                 $AddFldIntRec(WCurSess:'NSEG':%CHAR(R#DIT.NSEG));
                 $AddFldIntRec(WCurSess:'SEG':R#DIT.SEG);

                 $AddIntRecMed(WCodMedia:WtipInter);
              ENDIF;
              READ BASETMPT R#DIT;
           ENDDO;
        ENDSR;
      /END-FREE
