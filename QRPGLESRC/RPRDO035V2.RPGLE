     H NOMAIN BNDDIR('JMDIR')
     H*===================================================================
     H* Biblioteca.: RPRDO035
     H* Objectivo..: Disponibilizar um conjunto de API's de Produção
     H*===================================================================
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SGRLO010

     D*==> Riscos da Apólice
     D LST#Risco       DS                  QUALIFIED DIM(10)
     D   CodRisco                     5S 0 INZ(*ZEROS)
     D   CapRisco                    11S 2 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Campos de Trabalho
     D   WCodCobert    S              5S 0
     D   WCodLimite    S              5S 0
     D*=======================================================

     P*=======================================================
     P* Processo..: getCodRisco
     P* Objectivo.: Obter o Código de Risco ou todos
     P*=======================================================
     P $GetCapRisco    B                   EXPORT
     D $GetCapRisco    PI                  LIKEDS(LST#Capital) DIM(25)
     D   CodProduto                   5S 0 CONST
     D   CodRamo                      2S 0 CONST
     D   NumApolice                   7S 0 CONST
     D   CodRisco                     5S 0 CONST Options(*NOPASS)
     D   IcRefresh                     N   CONST Options(*NOPASS)
     D   DatInicio                    8S 0 CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WIcActTot       S              1A
     D WSessao         S              5S 0
     D WIdx            S              2S 0
     D WRecAct         S              3S 0
     D WCodRisco       S              5S 0
     D WIcRefresh      S               N
     D WResult         DS                  LIKEDS(LST#Capital) DIM(25)
     D*=======================================================
      /FREE
        WRecAct    = 1;
        WDatInicio = $DateToInt(%DATE());
        WCodRisco  = *ZEROS;
        WIcRefresh = *OFF;
        WIdx       = 1;
        WCodRisco  = *ZEROS;
        IF (%PARMS() > 3);
           WCodRisco = CodRisco;
           IF (%PARMS() > 4);
              WIcRefresh = IcRefresh;
              IF (%PARMS() > 5);
                 WDatInicio = DatInicio;
              ENDIF;
           ENDIF;
        ENDIF;

        //-----------------------------------------------------
        // Por razões de Performance, é valida se já foi dada
        // reposta ao pedido, com uma consulta anterior, e se
        // for o caso devolve, o resultado anterior.
        //-----------------------------------------------------
        IF ((CodRamo = WRamo) AND (WApolice <> NumApolice) AND
            (WDatInicio = DatInicio) AND NOT WIcRefresh) AND
            (WCodRisco = CodRisco);

        ENDIF;


        WResult(WIdx).CodRisco = *ZEROS;
        WResult(WIdx).CapRisco = *ZEROS;
        WResult(WIdx).IsEmpty  = *OFF;
        WSessao = $SetFiles('FPRDP011 A INNER JOIN FCATP101 B  +
                             ON (A.CDRAMO='''+%CHAR(CodRamo)+''') AND +
                             (B.CDPROD='''+%CHAR(CodProduto)+''') AND +
                             (A.CDRISCO = B.CDRISCO)');
        $AddDBDate($IntToDate(WDatInicio));
        $SetFields('FPRDP011':'A');
        $SetFields('FCATP101':'B':*OFF);
        IF (WCodRisco = *ZEROS);
           WIdx  += 1;
           $SetCriteria('(A.CDRAMO='''+%CHAR(CodRamo)+''') +
                     AND (A.NUAPOL='''+%CHAR(NumApolice)+''')');
        ELSE;
           $SetCriteria('(A.CDRAMO='''+%CHAR(CodRamo)+''') +
                     AND (A.NUAPOL='''+%CHAR(NumApolice)+''') +
                     AND (A.CDRISCO='''+%CHAR(WCodRisco)+''')');
        ENDIF;

        DOW ($GetRecord(WRecAct) > *ZEROS);
           WResult(WIdx).CodRisco = $GetFieldNum('A.CDRISCO');
           WResult(WIdx).CapRisco = $GetFieldNum('A.IMCPRISC');
           WResult(WIdx).IsEmpty  = *OFF;
           WIcActTot = $GetField('B.ICCAPTOT');
           IF (WIdx > 1);
              IF (WIcActTot = 'S');
                 WResult(1).CapRisco += $GetFieldNum('A.IMCPRISC');
              ENDIF;
           ENDIF;
           WIdx   += 1;
           WRecAct += 1;
        ENDDO;
        $RmvDBSession(WSessao);
        WResult(WIdx).IsEmpty  = *ON;


        RETURN WResult;
      /END-FREE
     P $GetCapRisco    E

     P*=======================================================
     P* Processo..: getLimites
     P* Objectivo.: Obter os valores dos Limites
     P*=======================================================
     P $GetLimites     B                   EXPORT
     D $GetLimites     PI                  LIKEDS(LST#Limite)
     D   CodProduto                   5S 0 CONST
     D   CodRamo                      2S 0 CONST
     D   NumApolice                   7S 0 CONST
     D   CodCobert                    5S 0 CONST
     D   CodLimite                    5S 0 CONST Options(*NOPASS)
     D   IcRefresh                     N   CONST Options(*NOPASS)
     D   DatInicio                    8S 0 CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WSessao         S              5S 0
     D WCodLimite      S              5S 0
     D WIcSelect       S               N
     D WIcRefresh      S               N
     D WResult         DS                  LIKEDS(LST#Limite)
     D*=======================================================
      /FREE
        WCodLimite = *ZEROS;
        WDatInicio = $DateToInt(%DATE());
        WIcRefresh = *OFF;
        IF (%PARMS() > 4);
           WCodLimite = CodLimite;
           IF (%PARMS() > 5);
              WIcRefresh = IcRefresh;
              IF (%PARMS() > 6);
                 WDatInicio = DatInicio;
              ENDIF;
           ENDIF;
        ENDIF;

        //-----------------------------------------------
        // Obter a Paremeterização em Catálogo
        //-----------------------------------------------
        WIcSelect = *OFF;
        WSessao = $SetFiles('FCATP103');
        $AddDBDate($IntToDate(WDatInicio));
        IF (WCodLimite = *ZEROS);
           $SetCriteria('(CDPROD='''+%CHAR(CodProduto)+''') +
                     AND (CDCOBERT='''+%CHAR(CodCobert)+''') +
                     AND (CDSUPER=CDLIMITE)');
        ELSE;
           $SetCriteria('(CDPROD='''+%CHAR(CodProduto)+''') +
                     AND (CDCOBERT='''+%CHAR(CodCobert)+''') +
                     AND (CDLIMITE='''+%CHAR(CodLimite)+''')');
        ENDIF;
        IF ($GetRecord() > *ZEROS);
           WIcSelect = *ON;
        ELSEIF (WCodLimite = *ZEROS);
           $SetCriteria('(CDPROD='''+%CHAR(CodProduto)+''') +
                     AND (CDCOBERT='''+%CHAR(CodCobert)+''')');
           IF ($GetRecord() > *ZEROS);
              WIcSelect = *ON;
           ENDIF;
        ENDIF;

        IF (WIcSelect);
           WResult.CdRisco  = $GetFieldNum('CDRISCO');
           WResult.CdCobert = $GetFieldNum('CDCOBERT');
           WCodLimite       = $GetFieldNum('CDLIMITE');
           WResult.CdIncInd = $GetField('CDINCIND');
           WResult.VlPerInd = $GetFieldNum('VLPERIND');
           WResult.VlMaxInd = $GetFieldNum('VLMAXIND');
           WResult.CdIncFrq = $GetField('CDINCFRQ');
           WResult.VlPerFrq = $GetFieldNum('VLPERFRQ');
           WResult.VlMinFrq = $GetFieldNum('VLMINFRQ');
           WResult.IsEmpty  = *OFF;
        ENDIF;
        $RmvDBSession(WSessao);

        //-----------------------------------------------
        // Obter a Excepção na Base de Dados
        //-----------------------------------------------
        WSessao = $SetFiles('FPRDP014');
        $AddDBDate($IntToDate(WDatInicio));
        $SetCriteria('(CDRAMO='''+%CHAR(CodRamo)+''') +
                     AND (NUAPOL='''+%CHAR(NumApolice)+''') +
                     AND (CDLIMITE='''+%CHAR(WCodLimite)+''')');
        IF ($GetRecord() > *ZEROS);
           WResult.CdIncInd = $GetField('CDINCIND');
           WResult.VlPerInd = $GetFieldNum('VLPERIND');
           WResult.VlMaxInd = $GetFieldNum('VLMAXIND');
           WResult.CdIncFrq = $GetField('CDINCFRQ');
           WResult.VlPerFrq = $GetFieldNum('VLPERFRQ');
           WResult.VlMinFrq = $GetFieldNum('VLMINFRQ');
           WResult.IsEmpty  = *OFF;
        ENDIF;
        $RmvDBSession(WSessao);
        RETURN WResult;
      /END-FREE
     P $GetLimites     E

     P*=======================================================
     P* Processo..: getValorAlfa
     P* Objectivo.: Obter os valores
     P*=======================================================
     P $GetValorAlfa   B                   EXPORT
     D $GetValorAlfa   PI            15A
     D   CodProduto                   5S 0 CONST
     D   CodRamo                      2S 0 CONST
     D   NumApolice                   7S 0 CONST
     D   CodTipo                      1A   CONST
     D   CodCobert                    5S 0 CONST
     D   CodLimite                    5S 0 CONST Options(*NOPASS)
     D   IcRefresh                     N   CONST Options(*NOPASS)
     D   DatInicio                    8S 0 CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WCodLimite      S              5S 0
     D WLstLimite      S              5S 0
     D WDatInicio      S              8S 0
     D WIcRefresh      S               N
     D WCapital        S             11S 2
     D WIdx            S              3S 0
     D WPerc           S              5S 2
     D WIncid          S              2A
     D WValor          S             11S 2
     D WResultado      S             15A
     D*=======================================================
      /FREE
        IF (%PARMS() > 5);
           WCodLimite = CodLimite;
           IF (%PARMS() > 6);
              WIcRefresh = IcRefresh;
              IF (%PARMS() > 7);
                WDatInicio = DatInicio;
              ENDIF;
           ENDIF;
        ENDIF;

         //--------------------------------------
         // Obter os Limites
         //--------------------------------------
         IF ((WCodCobert <> CodCobert) OR (WLstLimite <> CodLimite)
            OR WIcRefresh);
            WCodCobert = CodCobert;
            WLstLimite = CodLimite;
            LST#Limite = $getLimites(CodProduto:CodRamo:NumApolice:
                                     CodCobert:CodLimite:
                                     WIcRefresh:WDatInicio);
         ENDIF;

         //--------------------------------------
         // Obter os Capitais de Risco
         //--------------------------------------
         IF ((CodRamo <> WRamo) OR (WApolice <> NumApolice) OR
            (WDatInicio <> DatInicio) OR WIcRefresh);
            WRamo      = CodRamo;
            WApolice   = NumApolice;
            WDatInicio = DatInicio;

            //--------------------------------------
            // Obter o Capital de Risco
            //--------------------------------------
            LST#Risco   = $GetCapRisco(CodProduto:CodRamo:NumApolice:
                                       *ZEROS:WIcRefresh:WDatInicio);
         ENDIF;

         //--------------------------------------
         // Calcular o valor para o Capital
         //--------------------------------------
         WCapital = *ZEROS;
         WIdx = 1;
         DOW (NOT LST#Risco(WIdx).IsEmpty);
            IF (LST#Risco(Widx).Codrisco = LST#Limite.CdRisco);
                WCapital = LST#Risco(Widx).CapRisco/100;
                LEAVE;
            ENDIF;
            Widx += 1;
         ENDDO;

         //-------------------------------------------------------
         // Calcular os Valores para os Limites
         //-------------------------------------------------------
         IF (CodTipo = 'L');
            WPerc  = LST#Limite.VlPerInd;
            WIncid = LST#Limite.CdIncInd;
            WValor = LST#Limite.VlMaxInd;

            IF (WIncid = '01') OR (WIncid = '02');
               IF (WPerc = *ZEROS);
                  WCapital = WValor;
               ELSE;
                  WCapital = WCapital * WPerc;
               ENDIF;

               IF (WCapital > WValor) AND (WValor > *ZEROS);
                  WCapital = WValor;
               ENDIF;
            ELSEIF (WIncid = '03'); // Capital Próprio
               IF (WValor = *ZEROS);
                  RETURN ATR#Red+' Definir valor';
               ENDIF;
               WCapital = WValor;
            ELSEIF (WIncid = '04');
               WCapital = *ZEROS;
            ELSEIF (WIncid = '05');
               RETURN ATR#Yellow+' Conf.Cond.Esp.';
            ELSE;
               IF (WPerc = *ZEROS);
                  WCapital = WValor;
               ENDIF;
            ENDIF;
         ENDIF;

         //-------------------------------------------------------
         // Calcular os Valores para as Franquias
         //-------------------------------------------------------
         IF (CodTipo = 'F');
            WPerc  = LST#Limite.VlPerFrq;
            WIncid = LST#Limite.CdIncFrq;
            WValor = LST#Limite.VlMinFrq;
            IF (WIncid = '04');
               WCapital = *ZEROS;
            ELSE;
               IF (WPerc = *ZEROS);
                  WCapital = WValor;
               ELSE;
                  WCapital = WCapital * WPerc;
               ENDIF;

               IF (WCapital > WValor) AND (WValor > *ZEROS);
                  WCapital = WValor;
               ENDIF;
            ENDIF;
         ENDIF;

         //-------------------------------------------------------
         // Calcular os Valores para os Capitais
         //-------------------------------------------------------
         IF (CodTipo = 'C');
            WPerc  = LST#Limite.VlPerFrq;
            WIncid = LST#Limite.CdIncFrq;
            WValor = LST#Limite.VlMinFrq;
            IF (WIncid = '04');
               WCapital = *ZEROS;
            ELSE;
               IF (WPerc = *ZEROS);
                  WCapital = WValor;
               ELSE;
                  WCapital = WCapital * WPerc;
               ENDIF;

               IF (WCapital > WValor) AND (WValor > *ZEROS);
                  WCapital = WValor;
               ENDIF;
            ENDIF;
         ENDIF;

         IF (WCapital > *ZEROS);
            WResultado = ' '+%EDITW(WCapital:'   .   .   ,  ');
         ELSE;
            WResultado = '       --------';
         ENDIF;
         RETURN WResultado;
      /END-FREE
     P $GetValorAlfa   E
     P*=======================================================
     P* Processo..: $GetCapitalAlf
     P* Objectivo.: Obter os valores
     P*=======================================================
     P $GetCapitalAlf  B                   EXPORT
     D $GetCapitalAlf  PI            15A
     D   CodProduto                   5S 0 CONST
     D   CodRamo                      2S 0 CONST
     D   NumApolice                   7S 0 CONST
     D   CodTipo                      1A   CONST
     D   CodCobert                    5S 0 CONST
     D   CodLimite                    5S 0 CONST Options(*NOPASS)
     D   IcRefresh                     N   CONST Options(*NOPASS)
     D   DatInicio                    8S 0 CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WResultado      S             11S 2
     D*=======================================================
      /FREE
         WResultado = $GetCapitalNum(CodProduto:CodRamo:NumApolice:CodTipo:
                             CodCobert:CodLimite:IcRefresh:DatInicio);
         IF (WResultado = -100);
            RETURN ATR#Red+' Definir valor';
         ELSEIF (WResultado = -200);
            RETURN ATR#Yellow+' Conf.Cond.Esp.';
         ELSEIF (WResultado > *ZEROS);
            RETURN ' '+%EDITW(WResultado:'   .   .   ,  ');
         ELSE;
            RETURN '       --------';
         ENDIF;
      /END-FREE
     P $GetCapitalAlf  E
     P*=======================================================
     P* Processo..: $GetCapitalNum
     P* Objectivo.: Obter os valores
     P*=======================================================
     P $GetCapitalNum  B                   EXPORT
     D $GetCapitalNum  PI            15S 2
     D   CodProduto                   5S 0 CONST
     D   CodRamo                      2S 0 CONST
     D   NumApolice                   7S 0 CONST
     D   CodTipo                      1A   CONST
     D   CodCobert                    5S 0 CONST
     D   CodLimite                    5S 0 CONST Options(*NOPASS)
     D   IcRefresh                     N   CONST Options(*NOPASS)
     D   DatInicio                    8S 0 CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WCodLimite      S              5S 0
     D WLstLimite      S              5S 0
     D WDatInicio      S              8S 0
     D WIcRefresh      S               N
     D WCapital        S             11S 2
     D WIdx            S              3S 0
     D WPerc           S              5S 2
     D WIncid          S              2A
     D WValor          S             11S 2
     D WResultado      S             15A
     D*=======================================================
      /FREE
        IF (%PARMS() > 5);
           WCodLimite = CodLimite;
           IF (%PARMS() > 6);
              WIcRefresh = IcRefresh;
              IF (%PARMS() > 7);
                WDatInicio = DatInicio;
              ENDIF;
           ENDIF;
        ENDIF;

         //--------------------------------------
         // Obter os Limites
         //--------------------------------------
         IF ((WCodCobert <> CodCobert) OR (WLstLimite <> CodLimite)
            OR WIcRefresh);
            WCodCobert = CodCobert;
            WLstLimite = CodLimite;
            LST#Limite = $getLimites(CodProduto:CodRamo:NumApolice:
                                     CodCobert:CodLimite:
                                     WIcRefresh:WDatInicio);
         ENDIF;

         //--------------------------------------
         // Obter os Capitais de Risco
         //--------------------------------------
         IF ((CodRamo <> WRamo) OR (WApolice <> NumApolice) OR
            (WDatInicio <> DatInicio) OR WIcRefresh);
            WRamo      = CodRamo;
            WApolice   = NumApolice;
            WDatInicio = DatInicio;

            //--------------------------------------
            // Obter o Capital de Risco
            //--------------------------------------
            LST#Risco   = $GetCapRisco(CodProduto:CodRamo:NumApolice:
                                       *ZEROS:WIcRefresh:WDatInicio);
         ENDIF;

         //--------------------------------------
         // Calcular o valor para o Capital
         //--------------------------------------
         WCapital = *ZEROS;
         WIdx = 1;
         DOW (NOT LST#Risco(WIdx).IsEmpty);
            IF (LST#Risco(Widx).Codrisco = LST#Limite.CdRisco);
                WCapital = LST#Risco(Widx).CapRisco/100;
                LEAVE;
            ENDIF;
            Widx += 1;
         ENDDO;

         //-------------------------------------------------------
         // Calcular os Valores para os Limites
         //-------------------------------------------------------
         IF (CodTipo = 'L');
            WPerc  = LST#Limite.VlPerInd;
            WIncid = LST#Limite.CdIncInd;
            WValor = LST#Limite.VlMaxInd;

            IF (WIncid = '01') OR (WIncid = '02');
               IF (WPerc = *ZEROS);
                  WCapital = WValor;
               ELSE;
                  WCapital = WCapital * WPerc;
               ENDIF;

               IF (WCapital > WValor) AND (WValor > *ZEROS);
                  WCapital = WValor;
               ENDIF;
            ELSEIF (WIncid = '03'); // Capital Próprio
               IF (WValor = *ZEROS);
                  RETURN -100;
               ENDIF;
               WCapital = WValor;
            ELSEIF (WIncid = '04');
               WCapital = *ZEROS;
            ELSEIF (WIncid = '05');
               RETURN -200;
            ELSE;
               IF (WPerc = *ZEROS);
                  WCapital = WValor;
               ENDIF;
            ENDIF;
         ENDIF;

         //-------------------------------------------------------
         // Calcular os Valores para as Franquias
         //-------------------------------------------------------
         IF (CodTipo = 'F');
            WPerc  = LST#Limite.VlPerFrq;
            WIncid = LST#Limite.CdIncFrq;
            WValor = LST#Limite.VlMinFrq;
            IF (WIncid = '04');
               WCapital = *ZEROS;
            ELSE;
               IF (WPerc = *ZEROS);
                  WCapital = WValor;
               ELSE;
                  WCapital = WCapital * WPerc;
               ENDIF;

               IF (WCapital > WValor) AND (WValor > *ZEROS);
                  WCapital = WValor;
               ENDIF;
            ENDIF;
         ENDIF;

         //-------------------------------------------------------
         // Calcular os Valores para os Capitais
         //-------------------------------------------------------
         IF (CodTipo = 'C');
            WPerc  = LST#Limite.VlPerFrq;
            WIncid = LST#Limite.CdIncFrq;
            WValor = LST#Limite.VlMinFrq;
            IF (WIncid = '04');
               WCapital = *ZEROS;
            ELSE;
               IF (WPerc = *ZEROS);
                  WCapital = WValor;
               ELSE;
                  WCapital = WCapital * WPerc;
               ENDIF;

               IF (WCapital > WValor) AND (WValor > *ZEROS);
                  WCapital = WValor;
               ENDIF;
            ENDIF;
         ENDIF;

         RETURN WCapital;
      /END-FREE
     P $GetCapitalNum  E
