     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: PCVSO001
     H* Descrição.: Actualizar Ficheiros de Coberturas, Riscos, Avaliação
     H*             do Edificio, Condições e Cláusulas.
     H*==================================================================
     FFGRLP023  IF   E           K DISK    PREFIX(GRL_)
     FGBASEGRH  IF   F  700        DISK    USROPN
     FFPRDP008  UF A E           K DISK    COMMIT                               Coberturas
     FFPRDP009  UF A E           K DISK    COMMIT                               Clausulas e Condições
     FFPRDP016  UF A E           K DISK    COMMIT                               Estado de Conservação
     FFPRDP011  UF A E           K DISK    COMMIT                               Riscos
     FFPRDP030  UF A E           K DISK    COMMIT                               Histórico
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*================================================================
     D*--> Invocar um comando de AS/400 externo: -----------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Adicionar/Actualizar um registo de uma Cobertura
     D AddCobertura    PR
     D  Alias                        10A   CONST
     D  Valor                         1A   CONST
     D  Ramo                          2S 0 CONST
     D  Apolice                       7S 0 CONST
     D  Data                          8S 0 CONST
     D
     D*==> Adicionar Riscos da Apólice e Capitais
     D AddRiscos       PR
     D  Ramo                          2S 0 CONST
     D  Apolice                       7S 0 CONST
     D  Data                          8S 0 CONST
     D
     D*==> Adicionar Clausulas
     D AddClausula     PR
     D  TpItem                        1A   CONST
     D  Alias                        10A   CONST
     D  CdItem                        1A   CONST
     D  Ramo                          2S 0 CONST
     D  Apolice                       7S 0 CONST
     D  Data                          8S 0 CONST
     D
     D*==> Lista de Coberturas
     D Lst#Cobert      DS                  QUALIFIED DIM(100)
     D   CodProd                      5S 0 INZ(*ZEROS)
     D   Alias                       10A   INZ(*BLANKS)
     D   CdCobert                     5S 0 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Lista de Clausulas e Condições
     D Lst#Claus       DS                  QUALIFIED DIM(100)
     D   CodProd                      5S 0 INZ(*ZEROS)
     D   Alias                       10A   INZ(*BLANKS)
     D   CdItem                       3A   INZ(*BLANKS)
     D   TpItem                       1A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*--> Campos de Trabalho
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WNomLib         S             10A
     D WNomFile        S             10A
     D WSession        S              5S 0
     D WCdRamo         S              2S 0
     D WCdProd         S              5S 0
     D WCdCobert       S              5S 0
     D WCdItem         S              3A
     D WTpItem         S              1A
     D WAlias          S             10A
     D WNuapol         S              7S 0
     D WRegAct         S              5S 0
     D WFirstTime      S               N
     D WData           S              8S 0
     D WDatDMA         S              6S 0
     D WIdx            S              3S 0
     D WIcErro         S               N
     D*================================================================
     IGBASEGRH  AA
   64I                             S    1    2 0OR_RAMO                         RAMO
   64I                             S    3    9 0OR_APOL                         APOLICE
   65I                             S  179  184 0OR_DTINI                        DATA INIC. SEGURO (DMA)
   66I                             A  204  204  OR_CONDOM                       C-CONDOMINIO
   67I                             S  206  206 0OR_TALT                          "0" - SEGURO NOVO                  "1
   67I                             S  207  212 0OR_DALT                         DATA DA ALTER. (DMA)
   67I                             S  213  214 0OR_MALT                         MOTIVO DA ALTERAÇÃO
   67I                             P  215  216 0OR_ACTA                         N. ACTA
   70I                             S  217  217 0OR_CR1                          COD. RISCO-1
   70I                             P  218  223 0OR_CAP1                         CAPITAL RISCO-1
   70I                             P  224  226 4OR_TX1                          % TAXA RISCO-1
   70I                             P  227  231 2OR_PR1                          PRÉMIO RISCO-1
   70I                             S  232  232 0OR_CR2                          COD. RISCO-2
   70I                             P  233  238 0OR_CAP2                         CAPITAL RISCO-2
   70I                             P  239  241 4OR_TX2                          % TAXA RISCO-2
   70I                             P  242  246 2OR_PR2                          PRÉMIO RISCO-2
   71I                             S  247  247 0OR_CR3                          COD. RISCO-3
   71I                             P  248  253 0OR_CAP3                         CAPITAL RISCO-3
   71I                             P  254  256 4OR_TX3                          % TAXA RISCO-3
   71I                             P  257  261 2OR_PR3                          PRÉMIO RISCO-3
   71I                             P  262  267 0OR_CAPTOT                       TOTAL CAPITAL RISCOS
   72I                             A  533  533  OR_CF13                         COB FAC 13
   74I                             A  568  568  OR_CF01                         COB FAC 01
   74I                             A  569  569  OR_CF02                         COB FAC 02
   74I                             A  570  570  OR_CF03                         COB FAC 03
   74I                             A  571  571  OR_CF04                         COB FAC 04
   74I                             A  572  572  OR_CF05                         COB FAC 05
   74I                             A  573  573  OR_CF06                         COB FAC 06
   74I                             A  574  574  OR_CF07                         COB FAC 07
   75I                             A  575  575  OR_CF08                         COB FAC 08
   75I                             A  576  576  OR_CF09                         COB FAC 09
   75I                             A  577  577  OR_CF10                         COB FAC 10
   75I                             A  578  578  OR_CF11                         COB FAC 11
   75I                             A  579  579  OR_CF12                         COB FAC 12
   75I                             A  533  533  OR_CF13                         COB FAC 13
   77I                             A  621  621  OR_CF0800                       COB FAC 08.00
   77I                             A  622  622  OR_CF0830                       COB FAC 08.30
   75I                             A  534  534  OR_CE002                        COB ESP 002
   77I                             A  568  568  OR_CF01                         COB FAC 01
   77I                             A  569  569  OR_CF02                         COB FAC 02
   77I                             A  570  570  OR_CF03                         COB FAC 03
   77I                             A  571  571  OR_CF04                         COB FAC 04
   77I                             A  572  572  OR_CF05                         COB FAC 05
   77I                             A  573  573  OR_CF06                         COB FAC 06
   77I                             A  574  574  OR_CF07                         COB FAC 07
   77I                             A  575  575  OR_CF08                         COB FAC 08
   77I                             A  576  576  OR_CF09                         COB FAC 09
   77I                             A  577  577  OR_CF10                         COB FAC 10
   78I                             A  578  578  OR_CF11                         COB FAC 11
   78I                             A  579  579  OR_CF12                         COB FAC 12
   78I                             A  580  580  OR_CE001                        COB ESP 001
   78I                             A  581  581  OR_CE003                        COB ESP 003
   78I                             A  582  582  OR_CA030                        CLS APL 030
   78I                             A  583  583  OR_CA031                        CLS APL 031
   78I                             A  584  584  OR_CA100                        CLS APL 100
   78I                             A  585  585  OR_CA101                        CLS APL 101
   78I                             A  586  586  OR_CA102                        CLS APL 102
   78I                             A  587  587  OR_CD103                        CLS DIV 103
   79I                             A  588  588  OR_CD130                        CLS DIV 130
   79I                             A  589  589  OR_CD131                        CLS DIV 131
   79I                             A  590  590  OR_CD132                        CLS DIV 132
   79I                             A  591  591  OR_CD133                        CLS DIV 133
   79I                             A  592  592  OR_CD134                        CLS DIV 134
   79I                             A  593  593  OR_CD135                        CLS DIV 135
   79I                             A  594  594  OR_CD136                        CLS DIV 136
   79I                             A  595  595  OR_CD137                        CLS DIV 137
   80I                             A  623  623  OR_CF21                         COB FAC 21
   80I                             A  639  639  OR_CF22                         COB FAC 22
   81I                             A  640  640  OR_CE004                        COB ESP 004
     D*================================================================
      /FREE
        EXSR FillListas;

        // WNomLib  = 'QS36F';
        // WNomFile = 'GBASEGRO';
        // IF (%SUBST(WNomFile:1:5) = 'GBDIO');
        //    EXSR OpnFile;
        //    EXSR ActHistorico;
        // ENDIF;
        // READ RGRLP023;

        SETLL (*LOVAL) RGRLP023;
        READ RGRLP023;
        DOW (NOT %EOF(FGRLP023));
           WNomLib  = GRL_NOLIBL;
           WNomFile = GRL_NOFILE;
           IF (%SUBST(WNomFile:1:5) = 'GBDIO');
              EXSR OpnFile;
              EXSR ActHistorico;
           ENDIF;
           READ RGRLP023;
        ENDDO;

        *INLR = *ON;
        //=================================================================
        // Subrotina..: ActHistorico
        // Objectivo..: Actualizar Informação de Historico
        //=================================================================
        BEGSR ActHistorico;
           IF (NOT WIcErro);
              OPEN GBASEGRH;
              READ GBASEGRH;
              DOW NOT (%EOF(GBASEGRH));
                 //-----------------------------------------
                 // Determinar o Código de Produto
                 //-----------------------------------------
                 IF (OR_CONDOM = 'C');
                    WCdProd = 11;
                 ELSE;
                    WCdProd = 5;
                 ENDIF;

                 //-----------------------------------------
                 // Formatar data de Alteração
                 //-----------------------------------------
                 IF (WNomFile = 'GBASEGRO') OR (OR_DALT = *ZEROS);
                    WDatDMA = OR_DTINI;
                 ELSE;
                    WDatDMA = OR_DALT;
                 ENDIF;
                 WData = $CvtData(WDatDMA:'DMY':'YYMD');
                 EXSR ChgJobDate;

                 //-----------------------------------------
                 // Actualizar Histórico
                 //-----------------------------------------
                 WCdRamo = OR_Ramo;
                 WNuApol = OR_Apol;
                 EXSR UpdCoberturas;
                 AddRiscos(WCdRamo:WNuApol:WData);
                 EXSR UpdCondicoes;
                 EXSR UpdEstConserv;
                 READ GBASEGRH;
              ENDDO;
              CLOSE GBASEGRH;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: UpdCoberturas
        // Objectivo..: Actualizar Coberturas da Apólice
        //=================================================================
        BEGSR UpdCoberturas;
           AddCobertura('01':OR_CF01:WCdRamo:WNuApol:WData);
           AddCobertura('02':OR_CF02:WCdRamo:WNuApol:WData);
           AddCobertura('03':OR_CF03:WCdRamo:WNuApol:WData);
           AddCobertura('04':OR_CF04:WCdRamo:WNuApol:WData);
           AddCobertura('05':OR_CF05:WCdRamo:WNuApol:WData);
           AddCobertura('06':OR_CF06:WCdRamo:WNuApol:WData);
           AddCobertura('07':OR_CF07:WCdRamo:WNuApol:WData);
           AddCobertura('0800':OR_CF0800:WCdRamo:WNuApol:WData);
           AddCobertura('0830':OR_CF0830:WCdRamo:WNuApol:WData);
           AddCobertura('08':OR_CF08:WCdRamo:WNuApol:WData);
           AddCobertura('09':OR_CF09:WCdRamo:WNuApol:WData);
           AddCobertura('10':OR_CF10:WCdRamo:WNuApol:WData);
           AddCobertura('11':OR_CF11:WCdRamo:WNuApol:WData);
           AddCobertura('12':OR_CF12:WCdRamo:WNuApol:WData);
           AddCobertura('13':OR_CF13:WCdRamo:WNuApol:WData);
        ENDSR;

        //=================================================================
        // Subrotina..: UpdCondicoes
        // Objectivo..: Abrir o Condições e Clausulas Especiais
        //=================================================================
        BEGSR UpdCondicoes;
           AddClausula('E':'001':OR_CE001:WCdRamo:WNuApol:WData);
           AddClausula('E':'002':OR_CE002:WCdRamo:WNuApol:WData);
           AddClausula('E':'003':OR_CE003:WCdRamo:WNuApol:WData);
           AddClausula('E':'004':OR_CE004:WCdRamo:WNuApol:WData);
           AddClausula('A':'030':OR_CA030:WCdRamo:WNuApol:WData);
           AddClausula('A':'031':OR_CA031:WCdRamo:WNuApol:WData);
           AddClausula('A':'100':OR_CA100:WCdRamo:WNuApol:WData);
           AddClausula('A':'101':OR_CA101:WCdRamo:WNuApol:WData);
           AddClausula('A':'102':OR_CA102:WCdRamo:WNuApol:WData);
           AddClausula('D':'103':OR_CD103:WCdRamo:WNuApol:WData);
           AddClausula('D':'130':OR_CD130:WCdRamo:WNuApol:WData);
           AddClausula('D':'131':OR_CD131:WCdRamo:WNuApol:WData);
           AddClausula('D':'132':OR_CD132:WCdRamo:WNuApol:WData);
           AddClausula('D':'133':OR_CD133:WCdRamo:WNuApol:WData);
           AddClausula('D':'134':OR_CD134:WCdRamo:WNuApol:WData);
           AddClausula('D':'135':OR_CD135:WCdRamo:WNuApol:WData);
           AddClausula('D':'136':OR_CD136:WCdRamo:WNuApol:WData);
           AddClausula('D':'137':OR_CD137:WCdRamo:WNuApol:WData);
        ENDSR;

        //=================================================================
        // Subrotina..: UpdEstConserv
        // Objectivo..: Actualizar Estado de Conservação do Edificio
        //=================================================================
        BEGSR UpdEstConserv;
        ENDSR;

        //=================================================================
        // Subrotina..: FillListas
        // Objectivo..: Actualizar Listas de Trabalho
        //=================================================================
        BEGSR FillListas;
           //--------------------------------------------------
           // Actualizar Lista de Coberturas
           //--------------------------------------------------
           WSession = $SetFiles('FCATP102');
           $AddDBDate(%DATE());
           WRegAct = 1;
           DOW ($GetRecord(WRegAct) > *ZEROS);
              WAlias    = $GetField('ALIAS');
              WCDCobert = %DEC($GetField('CDCOBERT'):5:0);
              WCdProd   = %DEC($GetField('CDPROD'):5:0);
              IF ((WCdProd=5) OR (WCdProd=11)) AND ($GetField('ICOBRIGA')='N');
                 WIdx = 1;
                 DOW (NOT Lst#Cobert(WIdx).IsEmpty);
                    WIdx += 1;
                 ENDDO;
                 Lst#Cobert(WIdx).CodProd  = WCdProd;
                 Lst#Cobert(WIdx).Alias    = WAlias;
                 Lst#Cobert(WIdx).CdCobert = WCdCobert;
                 Lst#Cobert(WIdx).IsEmpty  = *OFF;
              ENDIF;
              WRegAct += 1;
           ENDDO;

           //--------------------------------------------------
           // Actualizar Lista de Clausulas
           //--------------------------------------------------
           WSession = $SetFiles('FCATP104');
           $AddDBDate(%DATE());
           WRegAct = 1;
           DOW ($GetRecord(WRegAct) > *ZEROS);
              WAlias  = $GetField('ALIAS');
              WCDItem = $GetField('CDITEM');
              WTpItem = $GetField('TPITEM');
              WCdProd = %DEC($GetField('CDPROD'):5:0);
              IF ((WCdProd=5) OR (WCdProd=11));
                 WIdx = 1;
                 DOW (NOT Lst#Cobert(WIdx).IsEmpty);
                    WIdx += 1;
                 ENDDO;
                 Lst#Claus(WIdx).CodProd = WCdProd;
                 Lst#Claus(WIdx).Alias   = WAlias;
                 Lst#Claus(WIdx).CdItem  = WCdItem;
                 Lst#Claus(WIdx).TpItem  = WTpItem;
                 Lst#Claus(WIdx).IsEmpty = *OFF;
              ENDIF;
              WRegAct += 1;
           ENDDO;
        ENDSR;

        //=================================================================
        // Subrotina..: chgJobDate
        // Objectivo..: Alterar a data do Job
        //=================================================================
        BEGSR chgjobDate;
           MONITOR;
              WStdCmd = 'CHGJOB +
                          DATE('+%TRIM(%EDITW(WDatDMA:'0      '))+') +
                          DATFMT(*DMY)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;
        ENDSR;

        //=================================================================
        // Subrotina..: opnFile
        // Objectivo..: Abrir o ficheiro de Histórico
        //=================================================================
        BEGSR opnFile;
           MONITOR;
              WIcErro = *OFF;
              WStdCmd = 'OVRDBF FILE(GBASEGRO) +
                         TOFILE('+%TRIM(WNomLib)+
                         '/' + %TRIM(WNomFile) + ') LVLCHK(*NO)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WIcErro = *ON;
           ENDMON;
        ENDSR;
       /END-FREE

     P*===============================================================
     P* Processo..: AddCobertura
     P* Objectivo.: Adicionar/Actualizar um registo de Cobertura
     P*===============================================================
     P AddCobertura    B                   EXPORT
     D AddCobertura    PI
     D  Alias                        10A   CONST
     D  Valor                         1A   CONST
     D  Ramo                          2S 0 CONST
     D  Apolice                       7S 0 CONST
     D  Data                          8S 0 CONST
     D
     D*==> Campos de Trabalho
     D WCdCobert       S              5S 0
     D WIcExiste       S               N
     D WNuSequ         S              5S 0
     D WSession        S              5S 0
     D*===============================================================
      /FREE
         IF (Ramo = 5);
            //----------------------------------------------
            // Obter código equivalente ao Alias
            //----------------------------------------------
            WCdCobert = *ZEROS;
            WIdx = 1;
            DOW (NOT Lst#Cobert(WIdx).IsEmpty);
               IF (Lst#Cobert(WIdx).CodProd = WCdProd) AND
                   (Lst#Cobert(WIdx).Alias = Alias);
                   WCdCobert = Lst#Cobert(WIdx).CdCobert;
                   LEAVE;
                ENDIF;
                WIdx += 1;
            ENDDO;

            //----------------------------------------------
            // Validar se já existe
            //----------------------------------------------
            WSession = $SetFiles('FPRDP008');
            $AddDBDate($IntToDate(Data));
            $SetCriteria('(CDRAMO='''+%CHAR(Ramo)+''') AND +
                          (NUAPOL='''+%CHAR(Apolice)+''') AND +
                          (CDCOBERT='''+%CHAR(WCdCobert)+''')');
            IF ($GetRecord(1) > *ZEROS);
               WNuSequ   = %DEC($GetField('NUSEQU'):5:0);
               WIcExiste = *ON;
            ELSE;
               WNuSequ   = *ZEROS;
               WIcExiste = *OFF;
            ENDIF;
            $RmvDBSession(WSession);

            IF (Valor = 'S') AND (NOT WIcExiste);
               IF (WCdCobert <> *ZEROS);
                  CDRAMO   = Ramo;
                  NUAPOL   = Apolice;
                  NUSEQU   = *ZEROS;
                  CDCOBERT = WCdCobert;
                  PRD00801 = $IntToDate(Data);
                  WRITE RPRDP008;
               ENDIF;
            ELSEIF (Valor = 'N') AND (WIcExiste);
               DELETE (Ramo:Apolice:WNuSeQu) RPRDP008;
            ENDIF;
         ENDIF;
      /END-FREE
     P AddCobertura    E

     P*===============================================================
     P* Processo..: AddRiscos
     P* Objectivo.: Adicionar/Actualizar Informação do Risco
     P*===============================================================
     P AddRiscos       B                   EXPORT
     D AddRiscos       PI
     D  Ramo                          2S 0 CONST
     D  Apolice                       7S 0 CONST
     D  Data                          8S 0 CONST
     D
     D*==> Campos de Trabalho
     D WSession        S              5S 0
     D WNuSequ         S              5S 0
     D WIcExiste       S               N
     D WCdRisco        S              5S 0
     D WCpRisco        S             11S 2
     D WPrRisco        S             11S 2
     D WTxRisco        S              5S 4
     D WRegAct         S              3S 0
     D*===============================================================
      /FREE
        IF (Ramo = 5);
           IF (OR_CR1 > *ZEROS);
              WCdRisco  = OR_CR1;
              WCpRisco = OR_CAP1;
              WTxRisco  = OR_TX1;
              EXSR AddRisco;
           ENDIF;
           IF (OR_CR2 > *ZEROS);
              WCdRisco  = OR_CR2;
              WCpRisco = OR_CAP2;
              WTxRisco  = OR_TX2;
              EXSR AddRisco;
           ENDIF;
           IF (OR_CR3 > *ZEROS);
              WCdRisco  = OR_CR3;
              WCpRisco = OR_CAP3;
              WTxRisco  = OR_TX3;
              EXSR AddRisco;
           ENDIF;

           WRegAct  = 1;
           WSession = $SetFiles('FPRDP011');
           $AddDBDate($IntToDate(Data));
           $SetCriteria('(CDRAMO='''+%CHAR(Ramo)+''') AND +
                         (NUAPOL='''+%CHAR(Apolice)+''') AND +
                         (CDRISCO='''+%CHAR(WCdRisco)+''')');
           DOW ($GetRecord(WRegAct) > *ZEROS);
              WNuSequ   = %DEC($GetField('NUSEQU'):5:0);
              WCdRisco  = %DEC($GetField('CDRISCO'):5:0);
              IF (WCdRisco <> OR_CR1) AND (WCdRisco <> OR_CR2) AND
                  (WCdRisco <> OR_CR3);
                 DELETE (Ramo:Apolice:WNuSequ) RPRDP011;
              ENDIF;
              WRegAct += 1;
           ENDDO;
           $RmvDBSession(WSession);
        ENDIF;

        //==========================================================
        // SubRotina.: AddRisco
        // Descrição.: Adicionar ou alterar um registo do Risco
        //==========================================================
        BEGSR AddRisco;
           //----------------------------------------------
           // Validar se já existe
           //----------------------------------------------
           WSession = $SetFiles('FPRDP011');
           $AddDBDate($IntToDate(Data));
           $SetCriteria('(CDRAMO='''+%CHAR(Ramo)+''') AND +
                         (NUAPOL='''+%CHAR(Apolice)+''') AND +
                         (CDRISCO='''+%CHAR(WCdRisco)+''')');
           IF ($GetRecord(1) > *ZEROS);
              WNuSequ   = %DEC($GetField('NUSEQU'):5:0);
              WIcExiste = *ON;
           ELSE;
              WNuSequ   = *ZEROS;
              WIcExiste = *OFF;
           ENDIF;
           $RmvDBSession(WSession);

           IF (WIcExiste);
              CHAIN (Ramo:Apolice:WNuSequ) RPRDP011;
              IMCPRISC = WCpRisco;
              TXRISCO  = WTxRisco;
              IMPRRISC = WPrRisco;
              UPDATE RPRDP011 %FIELDS(IMCPRISC:TXRISCO:IMPRRISC);
           ELSE;
              CDRAMO   = Ramo;
              NUAPOL   = Apolice;
              NUSEQU   = *ZEROS;
              CDRISCO  = WCdRisco;
              IMCPRISC = WCpRisco;
              TXRISCO  = WTxRisco;
              IMPRRISC = WPrRisco;
              WRITE RPRDP011;
           ENDIF;
        ENDSR;
      /END-FREE
     P AddRiscos       E

     P*===============================================================
     P* Processo..: AddClausula
     P* Objectivo.: Adicionar/Actualizar ou Eliminar Clausulas
     P*===============================================================
     P AddClausula     B                   EXPORT
     D AddClausula     PI
     D  TpItem                        1A   CONST
     D  Alias                        10A   CONST
     D  CdItem                        1A   CONST
     D  Ramo                          2S 0 CONST
     D  Apolice                       7S 0 CONST
     D  Data                          8S 0 CONST
     D*===============================================================
      /FREE
      /END-FREE
     P AddClausula     E
