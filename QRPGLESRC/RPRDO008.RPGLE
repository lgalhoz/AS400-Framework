     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO008
     H* Descrição.: Bloco que devolve o texto de detalhe a escrever nas
     H*             actas.
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO014
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrDio)
     D RBASEDIA      E DS                  ExtName(GBASEDIA)
     D                                     Prefix(AU_)
     D                                     Based(ptrDia)
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D
     D*==> Lista de Textos a escrever
     D WIdx            S              5S 0 INZ(*ZEROS)
     D
     D*==> Lista de Textos a escrever
     D #LinhasTxt      DS                  QUALIFIED DIM(300) BASED(pLinhasTxt)
     D   Tipo                         2A
     D   Exception                     N
     D   CdIdent                      8A
     D   Texto                      500A
     D   Valor                       20A
     D   CdMascara                    2A
     D   IsEMpty                       N
     D WIdxLine        S              5S 0 INZ(*ZEROS)
     D WMaxEntries     S              5S 0 INZ(50)
     D WLinha          S            120A   INZ(*BLANKS)
     D WNaturezaBk     S              2A
     D WNatureza       S              2A
     D WDatSelect      S              8S 0
     D WGEXAP          S               *
     D WGBaseDio       S               *
     D
     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D RetrText        PR                  EXTPGM(DynamicCall)
     D   CdProd                       5S 0 CONST
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D   Data                         8S 0 CONST
     D   CodErro                      7A   CONST
     D   ptrHist                       *   CONST Options(*NOPASS)
     D   ptrDiario                     *   CONST Options(*NOPASS)
     D
     D*==> Lista de Campos
     D DynamicCall     S             10A   INZ(*BLANKS)
     D WOrigem         S              1A
     D WCarrega        S              1A   INZ(*BLANKS)
     D PRM_Carrega     S                   LIKE(WCarrega)
     D WAnoMes         S              6S 0 INZ(*ZEROS)
     D PRM_AnoMes      S                   LIKE(WAnoMes)
     D PRM_Ramo        S                   LIKE(WRAMO)
     D PRM_Apolice     S                   LIKE(WApolice)
     D WDatEmissao     S              8S 0 INZ(*ZEROS)
     D PRM_DatEmissao  S                   LIKE(WDatEmissao)
     D WNomFilHist     S             10A   INZ(*BLANKS)
     D WNomFilAuto     S             10A   INZ(*BLANKS)
     D WNomFilOr       S             10A   INZ(*BLANKS)
     D WComando        S            300A   INZ(*BLANKS)
     D WCodProduto     S              5S 0 INZ(*ZEROS)
     D WRamoApolice    S              9A
     D WIcErro         S               N   INZ(*OFF)
     D WIcIniciar      S               N   INZ(*OFF)
     D WRegAct         S              5S 0 INZ(*ZEROS)
     D WSessCatalogo   S              5S 0 INZ(*ZEROS)
     D WCodErro        S              7A   INZ(*BLANKS)
     D PLargura        S              3S 0
     D WLargura        S              3S 0 INZ(*ZEROS)
     D
     D*==> Campos do ficheiro (Base/Diário/Histórico)
     D WHST_Ramo       S                   LIKE(HST_RR)
     D WHST_Apolice    S                   LIKE(HST_APOL)
     D WHST_Condom     S                   LIKE(HST_Condom)
     D
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyzç+
     D                                     ãáâàéèêíìîóòõôúùû'
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZÇ+
     D                                     ÃÁÂÀÉÈÊÍÌÎÓÒÕÔÚÙÛ'
     D*===================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WGEXAP
     C                   PARM                    WGBaseDio
     C                   PARM                    #RetTexto
     C                   PARM                    WNatureza
     C                   PARM                    WDatSelect
     C                   PARM                    PLargura
      /FREE
       //------------------------------------
       // Validar os parâmetros recebidos
       //------------------------------------
       ptrHist     = WGEXAP;
       ptrDio      = WGBaseDio;

       MONITOR;
          WLargura = PLargura;
       ON-ERROR;
          WLargura = *ZEROS;
       ENDMON;

       MONITOR;
          WHST_Ramo   = HST_RR;
          WHST_APolice= HST_APOL;
          WHST_Condom = HST_CONDOM;
       ON-ERROR;
          MONITOR;
             WHST_Ramo   = OR_RAMO;
             WHST_APolice= OR_APOL;
             WHST_Condom = OR_CONDOM;
          ON-ERROR;
             RETURN;
          ENDMON;
       ENDMON;

       $InzLista(0);
       WSessCatalogo = $SetFiles('FCATP118');
       $AddDBDate($IntToDate(WDatSelect));
       $SetSort('NUORDEM');
       EXSR getDetail;
       $RmvDBSession(WSessCatalogo);

       RETURN;

       //==================================================================
       // Subrotina..: getDetail
       // Objectivo..: Obter os blocos de texto a escrever na Acta
       //==================================================================
       BEGSR getDetail;
          IF (WHST_Ramo = 5);
             IF (WHST_Condom = 'C');
                WCodProduto = $GetCodProduto(WHST_Ramo:2);
             ELSE;
                WCodProduto = $GetCodProduto(WHST_Ramo:1);
             ENDIF;
          ELSE;
             WCodProduto = $GetCodProduto(WHST_Ramo);
          ENDIF;

          //---------------------------------------------------
          // Escrever o texto dos Itens
          //---------------------------------------------------
          WIcIniciar = *OFF;
          $SetCriteria('CDPROD = '+%CHAR(WCodProduto));
          WRegAct = 1;
          DOW ($GetRecord(WRegAct) > *ZEROS);
             WNaturezaBk = $GetField('CDNATURA');
             IF (WNatureza = '00') OR (WNatureza = WNaturezaBK);
                IF (NOT WIcIniciar);
                   WIcIniciar = *ON;
                   $InzLista(%DEC($GetField('CDPAGINA'):5:0));
                ENDIF;
                MONITOR;
                   DynamicCall = $GetFunction($GetFieldNum('CDFUNCT'));
                   WDatEmissao = WDatSelect;
                   RetrText(WCodProduto:WHST_Ramo:WHST_Apolice:WDatEmissao:
                            WCodErro:WGEXAP:WGBaseDio);
                   IF (WCodErro <> *BLANKS);
                      LEAVE;
                   ENDIF;
                ON-ERROR;
                   LEAVE;
                ENDMON;
             ENDIF;
             $SetDBSession(WSessCatalogo);
             WRegAct += 1;
          ENDDO;

          //---------------------------------------------------
          // Escrever os Anexos se existirem
          //---------------------------------------------------
          IF (WCodErro = *BLANKS);
             IF (WLargura > *ZEROS);
                #RetTexto = $ProcessTexto('T':WLargura);
             ELSE;
                #RetTexto = $ProcessTexto();
             ENDIF;
          ELSE;
             #RetTexto(1).Texto = $GetMessage(WCodErro);
             #RetTexto(1).IsEmpty = *OFF;
             #RetTexto(2).IsEmpty = *ON;
          ENDIF;
       ENDSR;
      /END-FREE
