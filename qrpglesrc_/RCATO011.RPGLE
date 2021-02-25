     H*=====================================================================
     H* Rotina....: RCATO011
     H* Objectivo.: Validar se a apólice sofreu a inclusão ou exclusão dos
     H*             riscos de edificio ou conetudo, e se os mesmo sofreram
     H*             aumentos de Capital durante a vigência do contrato.
     H*
     H* Parâmetros Entrada:
     H*             Ramo (2,0)       = Código do Ramo
     H*             Apólice (7,0)    = Número da Apólice
     H*             DatVencim (8,0)  = Formato (YYYYMMDD)
     H*             CapRisco01 (11,0) =
     H*             CapRisco02 (11,0) =
     H*             CapRisco03 (11,0) =
     H*
     H* Parâmetros Saída:
     H*             Código        = Resultado da Análise
     H*                              =0 -> Sem Variação
     H*                              >0 -> Com Aumento Capital
     H*             Data          = Data do Código
     H*
     H*=====================================================================
     H Option(*SrcStmt: *NoDebugIO) DftActGRP(*No)
     F*=====================================================================
     FGBASEDIO  IF   F  700     9AIDISK    KEYLOC(1) USROPN
     F*BASEDIO  IF   E           K DISK    USROPN RENAME(BASEDIFO:HISTORICO)
     F*                                    PREFIX(HS_)
     FGBASEGRO  IF   F  700     9AIDISK    KEYLOC(1) USROPN
     F*BASEGRO  IF   E           K DISK    USROPN RENAME(BASEGRFO:ANTERIOR)
     F*                                    PREFIX(OR_)
     D*===================================================================
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(HS_)
     D RBASEGRO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D GENDS           DS
     D  OffsetHdr                    10i 0
     D  OffsetHdr2                   10i 0
     D  NbrInList                    10i 0
     D  SizeEntry                    10i 0
     D
     D WDataElem       DS
     D    WDataElm             1      8S 0
     D    WAnoElm              1      4S 0
     D    WMesElm              5      6S 0
     D    WDiaElm              7      8S 0
     D    WAnoMesElm           1      6S 0
     D
     D Object          DS
     D  Name                   1     10A
     D  Library               11     20A
     D  Type                  21     30A
     D  Status                31     31A
     D  Attrib                32     41A
     D  Description           42     91A
     D  UserAttrib            92    101A
     D  Reserved             102    108A
     D  AspNumber            109    112B 0
     D  Owner                113    122A
     D  Domain               123    124A
     D  DatCria              125    132A
     D  DatAltera            133    140A
     D
     D*-- Invocar QWCCVTDT --------------------------------------------
     D #DatConv        PR                  EXTPGM('QWCCVTDT')
     D   InpFormat                   10A   CONST
     D   InVariavel                  20A   CONST OPTIONS(*VARSIZE)
     D   OutFormat                   10A   CONST
     D   OutVariavel                 20A   CONST OPTIONS(*VARSIZE)
     D   ErroCode                    20A   CONST OPTIONS(*VARSIZE)
     D
     D*-- Invocar um comando de AS/400 externo: -----------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*-- Listar os ficheiros existentes na Biblioteca: ---------------
     D ReadLib         PR                  ExtPgm( 'RCATO012' )
     D  NomLib                       10A   Const
     D
     D*-- Obter data de criação do objecto: ---------------
     D getDatObje      PR                  ExtPgm( 'RCATO013' )
     D  NomLib                       10A   Const
     D  NomObje                      10A   Const
     D  TipObje                      10A   Const
     D  DatCriacao                   14S 0 Const
     D
     D*-- Userspace pointer: ------------------------------------------
     D ReadUserSpace   PR                  ExtPgm( 'QUSRTVUS' )
     D  userspace                    20a   Const
     D  start                        10i 0 Const
     D  Length                       10i 0 Const
     D  Returned                  32767a         Options( *VarSize )
     D
     D*-- Declaração de Variáveis: --------------------------------------                          
     D Count           S              4  0
     D GenLen          S              8
     D StartPosit      S             10i 0
     D StartLen        S             10i 0
     D UserSpace       S             20    INZ('LISTOUTQS QTEMP')
     D WNomLib         S             10                                                            
     D WNomFile        S             10A
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WDatIniCont     S             14S 0 INZ(*ZEROS)
     D WDatFimCont     S             14S 0 INZ(*ZEROS)
     D
     D WRamoApolice    S              9S 0
     D WCodRamo        S              2S 0
     D WNumApol        S              7S 0
     D WDatVencim      S              8S 0
     D WCapRisc01      S             11S 0
     D WCapRisc02      S             11S 0
     D WCapRisc03      S             11S 0
     D WCodDecimal     S              2S 0
     D WLastDatProc    S              8S 0
     D
     D WDatCria        S             16A
     D WDatAltera      S             16A
     D WError          S             20A
     D
     D*------- Lista interna de registos de Capitais -----------------
     D #RegCap         DS                  QUALIFIED DIM(12)
     D   Ano                          4S 0
     D   Mes                          2S 0
     D   Carrega                      1A
     D   CapRisc01                   11S 0 INZ(*ZEROS)
     D   CapRisc02                   11S 0 INZ(*ZEROS)
     D   CapRisc03                   11S 0 INZ(*ZEROS)
     D   Codigo                       2S 0 INZ(*ZEROS)
     D   DatCria                     14S 0 INZ(*ZEROS)
     D   DatAltera                   14S 0 INZ(*ZEROS)
     D   NextRecord                   2S 0 INZ(*ZEROS)
     D WIdxReg         S              2S 0 INZ(*ZEROS)
     D IdxReg          S              2S 0 INZ(*ZEROS)
     D TotReg          S              2S 0 INZ(*ZEROS)
     D WBkData         S             14S 0 INZ(*ZEROS)
     D WPrevRecord     S              2S 0 INZ(*ZEROS)
     D WFirst          S              2S 0 INZ(*ZEROS)
     D WDspText        S             52A
     D*=====================================================================
     IGBASEDIO  AA  11
     I                                  1  700  RBASEDIO
     IGBASEGRO  AA  11
     I                                  1  700  RBASEGRO
     I*=====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodRamo
     C                   PARM                    WNumApol
     C                   PARM                    WDatVencim
     C                   PARM                    WCapRisc01
     C                   PARM                    WCapRisc02
     C                   PARM                    WCapRisc03
     C                   PARM                    WCodDecimal
     C                   PARM                    WLastDatProc
     C
      /FREE
        //-----------------------------------------------------------------
        // Formatar a variável para selecção dos períodos
        //-----------------------------------------------------------------
        EXSR getBaseLine;   // Obter data limite

        //---------------------------------------------------------------
        // Formatar a biblioteca de Histórico em função da data de inicio
        //---------------------------------------------------------------
        WNomLib = 'GUIAS'+ %CHAR(WAnoElm);
        EXSR ListFiles;  // Obter Histórico do ano anterior
        WAnoElm += 1;
        WNomLib = 'QS36F';
        EXSR ListFiles;  // Obter do ano corrente
        EXSR chkVariacao;

        *INLR = *On;

        //=================================================================
        // Subrotina.: getBaseLine
        // Objectivo.: Obter a data da última renovação.
        //
        //=================================================================
        BEGSR getBaseLine;
           MONITOR;
              //-------------------------------------
              // Formatar o mês dos Continuados
              //-------------------------------------
              WDataElm = WDatVencim;

              //-----------------------------------------
              // Obter a data mais recente a partir dos Continuados
              //-----------------------------------------
              WNomLib = 'GUIAS' + %CHAR(WAnoElm);
              WNomFile = 'BGRO' + %SUBST(%CHAR(WAnoMesElm):3:4);
              getDatobje(WNomLib : WNomFile : '*FILE' : WDatFimCont);
              IF (WDatFimCont = *ZEROS);
                 WNomLib = 'QS36F';
                 WNomFile = 'BGRO' + %SUBST(%CHAR(WAnoMesElm):3:4);
                 getDatobje(WNomLib : WNomFile : '*FILE' : WDatFimCont);
              ENDIF;

              //---------------------------------------------------
              // Se existirem continuados então obtém os Capitais
              // à data, salvaguardado no Ficheiro com a posição
              // das Apólices antes da Emissão de Continuados.
              //---------------------------------------------------
              IF (WDatFimCont <> *ZEROS);
                 MONITOR;
                    WStdCmd = 'OVRDBF FILE(GBASEGRO) TOFILE('+%TRIM(WNomLib)+
                              '/' + %TRIM(WNomFile) + ') LVLCHK(*NO)';
                    WStdLen = %LEN(WStdCmd);
                    shell(WStdCmd:WStdLen);
                    OPEN GBASEGRO;
                    WRamoApolice = (WCodRamo * 10000000) + WNumApol;
                    CHAIN WRamoApolice GBASEGRO;
                    IF (%FOUND);
                       IF (OR_CR1 = 1) OR (OR_CR1 = 2) OR (OR_CR1 = 3);
                          WCapRisc01 = OR_CAP1;
                       ENDIF;
                       IF (OR_CR2 = 1) OR (OR_CR2 = 2) OR (OR_CR2 = 3);
                          WCapRisc02 = OR_CAP2;
                       ENDIF;
                       IF (OR_CR3 = 1) OR (OR_CR2 = 3) OR (OR_CR3 = 3);
                          WCapRisc03 = OR_CAP3;
                       ENDIF;
                    ENDIF;
                    CLOSE GBASEGRO;
                 ON-ERROR;
                 ENDMON;
              ENDIF;

              //-----------------------------------------
              // Obter a data mais antiga a partir dos Continuados
              //-----------------------------------------
              WAnoElm -= 1;
              WNomLib = 'GUIAS' + %CHAR(WAnoElm);
              WNomFile = 'BGRO' + %SUBST(%CHAR(WAnoMesElm):3:4);
              getDatobje(WNomLib : WNomFile : '*FILE' : WDatIniCont);
              IF (WDatIniCont = *ZEROS);
                 WNomLib = 'QS36F';
                 WNomFile = 'BGRO' + %SUBST(%CHAR(WAnoMesElm):3:4);
                 getDatobje(WNomLib : WNomFile : '*FILE' : WDatIniCont);
              ENDIF;
           ON-ERROR;
           ENDMON;
        ENDSR;

        //=================================================================
        // Subrotina.: ListFiles
        // Objectivo.: Listar os ficheiros existentes numa dada Biblioteca
        //=================================================================
        BEGSR ListFiles;
           //--------------------------------------------------------------
           // Extrai objectos residentes na biblioteca                             
           //--------------------------------------------------------------
           ReadLib(WNomLib);
           StartPosit = 125;
           StartLen   = 16;
           ReadUserSpace(userspace : StartPosit : StartLen : GENDS);
           StartPosit = OffsetHdr + 1;
           StartLen = %size(Object);

           //---------------------------------------------
           // Procede à leitura dos registos
           //---------------------------------------------
           FOR count = 1 to  NbrInList;
              ReadUserSpace(userspace  : StartPosit : StartLen : Object);
              IF ((Attrib = 'PF') AND (%SUBST(Name:1:5) = 'GBDIO'));
                 WNomFile = Name;
                 #DatConv('*DTS':DatCria:'*YMD':WDatCria:WError);
                 #DatConv('*DTS':DatAltera:'*YMD':WDatAltera:WError);
                 WBkData = %DEC(%SUBST(WDatCria:2:12):14:0)+20000000000000;
                 IF (WBkData > WDatIniCont) AND
                     ((WDatFimCont = *ZEROS) OR (WBkData < WDatFimCont));
                     EXSR OpnFile;
                 ENDIF;
              ENDIF;
              StartPosit += SizeEntry;
           ENDFOR;
        ENDSR;

        //=================================================================
        // Subrotina..: opnFile
        // Objectivo..: Abrir o ficheiro de Histórico
        //=================================================================
        BEGSR opnFile;
           MONITOR;
              WStdCmd = 'OVRDBF FILE(GBASEDIO) TOFILE('+%TRIM(WNomLib)+
                        '/' + %TRIM(WNomFile) + ') LVLCHK(*NO)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
              OPEN GBASEDIO;
              WRamoApolice = (WCodRamo * 10000000) + WNumApol;
              CHAIN WRamoApolice GBASEDIO;
              IF (%FOUND);
                 IdxReg += 1;
                 #RegCap(IdxReg).Ano = WAnoElm;
                 #RegCap(IdxReg).Mes = %DEC(%SUBST(WNomFile:6:2):2:0);
                 #RegCap(IdxReg).Carrega = %SUBST(WNomFile:8:1);

                 IF (HS_CR1 = 1) OR (HS_CR1 = 2) OR (HS_CR1 = 3);
                    #RegCap(IdxReg).CapRisc01 = HS_CAP1;
                 ENDIF;
                 IF (HS_CR2 = 1) OR (HS_CR2 = 2) OR (HS_CR2 = 3);
                    #RegCap(IdxReg).CapRisc02 = HS_CAP2;
                 ENDIF;
                 IF (HS_CR3 = 1) OR (HS_CR2 = 3) OR (HS_CR3 = 3);
                    #RegCap(IdxReg).CapRisc03 = HS_CAP3;
                 ENDIF;

                 #RegCap(IdxReg).DatCria = %DEC(%SUBST(WDatCria:2:12):14:0)
                                           + 20000000000000;
                 #RegCap(IdxReg).DatAltera = %DEC(%SUBST(WDatAltera:2:12):14:0)
                                           + 20000000000000;
                 TotReg = IdxReg;
              ENDIF;
              CLOSE GBASEDIO;
           ON-ERROR;
           ENDMON;
        ENDSR;

        //=================================================================
        // Subrotina..: chkVariacao
        // Objectivo..: Validar aumentos e/ou inclusão de riscos
        //=================================================================
        BEGSR chkVariacao;
           WLastDatProc = *ZEROS;
           WCodDecimal = *ZEROS;
           WBkData = *ZEROS;
           IF (TotReg >= 1);
              Exsr sortLista; // Ordenar a Lista por data

              //----------------------------------------------
              // Analisar o histórico na Vigência do Contrato
              //----------------------------------------------
              IdxReg = WFirst;
              WIdxReg = *ZEROS;
              WCodDecimal = *ZEROS;
              DOU (WCodDecimal > *ZEROS);
                 IF (WCapRisc01 > #RegCap(IdxReg).CapRisc01);
                    WCodDecimal = WIdxReg; // Guardar a posição anterior
                 ENDIF;

                 IF (WCapRisc02 > #RegCap(IdxReg).CapRisc02);
                    WCodDecimal = WIdxReg; // Guardar a posição anterior
                 ENDIF;

                 IF (WCapRisc03 > #RegCap(IdxReg).CapRisc03);
                    WCodDecimal = WIdxReg; // Guardar a posição anterior
                 ENDIF;
                 WIdxReg = IdxReg;
                 IdxReg = #RegCap(IdxReg).NextRecord;
                 IF (IdxReg = WFirst);
                     LEAVE;
                 ENDIF;
              ENDDO;

              IF (WCodDecimal > *ZEROS);
                 WLastDatProc = #RegCap(WCodDecimal).DatCria / 1000000;
                 WCodDecimal = 5; // Aumento de Capital
              ELSE;
                 WCodDecimal = *ZEROS;
                 WLastDatProc = *ZEROS;
              ENDIF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: sortLista
        // Objectivo..: Ordenar a lista Interna por data de Alteração
        //              decrescente.
        //=================================================================
        BEGSR sortLista;
           WPrevRecord = *ZEROS;
           WFirst = *ZEROS;
           DOU (WBkData = *ZEROS);
              WBkData = *ZEROS;

              FOR IdxReg = 1 TO TotReg;
                 IF (#RegCap(IdxReg).DatCria > WBkData) AND
                      (#RegCap(IdxReg).NextRecord = *ZEROS);
                    WBkData = #RegCap(IdxReg).DatCria;
                    WIdxReg = IdxReg;
                 ENDIF;
              ENDFOR;

              IF (WPrevRecord = *ZEROS);
                 WFirst = WIdxReg;
              ELSE;
                 #RegCap(WPrevRecord).NextRecord = WIdxReg;
              ENDIF;
              #RegCap(WIdxReg).NextRecord = WFirst;
              WPrevRecord = WIdxReg;
           ENDDO;
        ENDSR;

      /END-FREE
