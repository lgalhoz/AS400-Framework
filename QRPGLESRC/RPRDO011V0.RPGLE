     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RRDO011
     H* Descrição.: Informação Standard de Multiriscos Habitação
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Escavar lista de Catálogo
     D DigCatalogo     PR              N
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_NuApol                  7S 0 CONST
     D    PRM_CdTexto                 5S 0 CONST
     D    PRM_DatIni                  8S 0 CONST
     D
     D*==> Escavar lista de valores da Apólice
     D DigValues       PR              N
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_NuApol                  7S 0 CONST
     D    PRM_NuSequ                  5S 0 CONST
     D    PRM_DatIni                  8S 0 CONST
     D    PRM_DsTexto               100A   CONST
     D
     D*==> Parametros recebidos
     D WDsRisco        S             40A   INZ(*BLANKS)
     D WDsRiscoBk      S             40A   INZ(*BLANKS)
     D WIcTitulo       S               N   INZ(*OFF)
     D WIcMainHdr      S               N   INZ(*OFF)
     D WIcExiste       S               N   INZ(*OFF)
     D WLinha          S            500A   INZ(*BLANKS)
     D WSession        S              5S 0 INZ(*ZEROS)
     D WRecAct         S              3S 0 INZ(*ZEROS)
     D WCdProd         S              5S 0
     D WCdRisco        S              5S 0 INZ(*ZEROS)
     D WCdTexto        S              5S 0 INZ(*ZEROS)
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyzç+
     D                                     ãáâàéèêíìîóòõôúùû'
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZÇ+
     D                                     ÃÁÂÀÉÈÊÍÌÎÓÒÕÔÚÙÛ'
     D
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
      /FREE
         WRecAct = 1;
         WIcMainHdr = *OFF;
         WDsRiscoBk = *BLANKS;

         WSession = $SetFiles('FCATP105');
         $AddDBDate($IntToDate(WDatInicio));
         $SetCriteria('(CDPROD = '''+%CHAR(WCdProd)+''')');
         DOW ($GetRecord(WRecAct) > *ZEROS);
            //----------------------------------------------
            // Obter dependentes ao nível do Catálogo
            //----------------------------------------------
            WIcTitulo = *ON;
            WCdRisco  = $GetfieldNum('CDRISCO');
            WCdTexto  = $GetfieldNum('CDTEXTO');
            WDsRisco  = $getDescRisco(WCdProd:WCdRisco);
            WIcExiste = DigCatalogo(WCdProd:WRamo:WApolice:
                                  WCdTexto:WDatInicio);
            WRecAct += 1;
            $SetDBSession(WSession);
         ENDDO;
         $RmvDBSession(WSession);

         RETURN;
      /END-FREE
     P*=======================================================
     P* Processo..: DigCatalogo
     P* Objectivo.: Precorrer a árvore do Catálogo
     P*=======================================================
     P DigCatalogo     B                   EXPORT
     D DigCatalogo     PI              N
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D   PRM_CdTexto                  5S 0 CONST
     D   PRM_DatIni                   8S 0 CONST
     D
     D*==> Registos de Catalogo
     D Lst#CAT         DS                  QUALIFIED DIM(200)
     D   CDPROD                       5S 0 INZ(*ZEROS)
     D   CDTEXTO                      5S 0 INZ(*ZEROS)
     D   CDSUPER                      5S 0 INZ(*ZEROS)
     D   NUORDEM                      5S 0 INZ(*ZEROS)
     D   CDGRUPO                      5S 0 INZ(*ZEROS)
     D   DSTEXTO                    100A   INZ(*BLANKS)
     D   ICOBRIGA                     1A   INZ(*BLANKS)
     D   ALIAS                       10A   INZ(*BLANKS)
     D   CONVERSAO                   10A   INZ(*BLANKS)
     D   ICRESERV                     1A   INZ(*BLANKS)
     D   CDFUNCT                      5S 0 INZ(*ZEROS)
     D   ICDETAIL                     1A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D WIdxCat         S              5S 0 INZ(*ZEROS)
     D WCdSuper        S              5S 0 INZ(*ZEROS)
     D WNuSequ         S              5S 0 INZ(*ZEROS)
     D WTexto          S            150A   INZ(*BLANKS)
     D WIcExiste       S               N   INZ(*OFF)
     D WSessProd       S              5S 0 INZ(*ZEROS)
     D WSessCat        S              5S 0 INZ(*ZEROS)
     D WRegAct         S              3S 0 INZ(*ZEROS)
     D WResultado      S               N
     D*=================================================================
      /FREE

        //----------------------------------------------
        // Trabalhar o Registos da Lista
        //----------------------------------------------
        WIcExiste  = *OFF;
        WResultado = WIcExiste;
        WCdSuper   = PRM_CdTexto;
        WSessProd  = $SetFiles('FPRDP010');
        $AddDBDate($IntToDate(PRM_DatIni));
        $SetCriteria('(CDRAMO = '''+%CHAR(PRM_CdRamo)+''') AND +
                      (NUAPOL = '''+%CHAR(PRM_NuApol)+''') AND +
                      (CDTEXTO= '''+%CHAR(PRM_CdTexto)+''')');
        IF ($GetRecord() > *ZEROS);
           WNuSequ = $GetFieldNum('NUSEQU');

           WSessCat = $SetFiles('FCATP106');
           $AddDBDate($IntToDate(PRM_DatIni));
           $SetCriteria('(CDPROD = '''+%CHAR(PRM_CdProd)+''') AND +
                        (CDTEXTO= '''+%CHAR(PRM_CdTexto)+''')');
           IF ($GetRecord() > *ZEROS);
               WTexto  = $GetField('DSTEXTO');
               WIcExiste=DigValues(PRM_CdProd:PRM_CdRamo:PRM_NuApol:
                               WNuSequ:PRM_DatIni:WTexto);
               IF (WIcExiste);
                  WResultado = WIcExiste;
               ENDIF;
           ENDIF;
           $RmvDBSession(WSessCat);
        ELSE;
           //----------------------------------------------
           // Obter dependentes ao nível do Catálogo
           //----------------------------------------------
           WIdxCat  = 1;
           WSessCat = $SetFiles('FCATP106');
           $AddDBDate($IntToDate(PRM_DatIni));
           $SetCriteria('(CDPROD = '''+%CHAR(PRM_CdProd)+''') AND +
                         (CDTEXTO= '''+%CHAR(PRM_CdTexto)+''') AND +
                         (CDTEXTO <> CDSUPER)');
           $SetSort('NUORDEM');
           DOW ($GetRecord(WIdxCat) > *ZEROS);
              Lst#CAT(WIdxCat).CDPROD    = $GetFieldNum('CDPROD');
              Lst#CAT(WIdxCat).CDTEXTO   = $GetFieldNum('CDTEXTO');
              Lst#CAT(WIdxCat).CDSUPER   = $GetFieldNum('CDSUPER');
              Lst#CAT(WIdxCat).NUORDEM   = $GetFieldNum('NUORDEM');
              Lst#CAT(WIdxCat).CDGRUPO   = $GetFieldNum('CDGRUPO');
              Lst#CAT(WIdxCat).DSTEXTO   = $GetField('DSTEXTO');
              Lst#CAT(WIdxCat).ICOBRIGA  = $GetField('ICOBRIGA');
              Lst#CAT(WIdxCat).ALIAS     = $GetField('ALIAS');
              Lst#CAT(WIdxCat).CONVERSAO = $GetField('CONVERSAO');
              Lst#CAT(WIdxCat).ICRESERV  = $GetField('ICRESERV');
              Lst#CAT(WIdxCat).CDFUNCT   = $GetFieldNum('CDFUNCT');
              Lst#CAT(WIdxCat).ICDETAIL  = $GetField('ICDETAIL');
              Lst#CAT(WIdxCat).IsEmpty   = *OFF;
              WIdxCat += 1;
           ENDDO;
           $RmvDBSession(WSessCat);

           IF (WIdxCat > 1);
              WIdxCat = 1;
              DOW (NOT Lst#CAT(WIdxCat).IsEmpty);
                 WCdSuper  = Lst#CAT(WIdxCat).CDTEXTO;
                 WIcExiste = DigCatalogo(PRM_CdProd:PRM_CdRamo:PRM_NuApol:
                                         WCdSuper:PRM_DatIni);
                 IF (WIcExiste);
                    WResultado = WIcExiste;
                 ENDIF;
                 WIdxCat += 1;
              ENDDO;
           ENDIF;
        ENDIF;
        $RmvDBSession(WSessProd);

        RETURN WResultado;
      /END-FREE
     P DigCatalogo     E

     P*=======================================================
     P* Processo..: DigValues
     P* Objectivo.: Precorrer a árvore do valores da Apólice
     P*=======================================================
     P DigValues       B                   EXPORT
     D DigValues       PI              N
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_NuApol                  7S 0 CONST
     D    PRM_NuSequ                  5S 0 CONST
     D    PRM_DatIni                  8S 0 CONST
     D    PRM_DsTexto               100A   CONST
     D
     D*==> Registos de Catalogo
     D Lst#PRD         DS                  QUALIFIED DIM(200)
     D   CDRAMO                       2S 0 INZ(*ZEROS)
     D   NUAPOL                       7S 0 INZ(*ZEROS)
     D   NUSEQU                       5S 0 INZ(*ZEROS)
     D   NUSQSUP                      5S 0 INZ(*ZEROS)
     D   CDTEXTO                      5S 0 INZ(*ZEROS)
     D   CPSEGURO                    11S 2 INZ(*ZEROS)
     D   IsEMpty                       N   INZ(*ON)
     D
     D WIdxPrd         S              5S 0
     D WNuSqSup        S              5S 0 INZ(*ZEROS)
     D WNuSequ         S              5S 0 INZ(*ZEROS)
     D WTexto          S            150A   INZ(*BLANKS)
     D WCapital        S             11S 2 INZ(*ZEROS)
     D WSession        S              5S 0 INZ(*ZEROS)
     D WIcExiste       S               N
     D WResultado      S               N
     D*=================================================================
      /FREE

        //----------------------------------------------
        // Obter dependentes ao nível do Catálogo
        //----------------------------------------------
        WIcExiste  = *OFF;
        WResultado = WIcExiste;
        WTexto     = PRM_DSTEXTO;
        WIdxPrd    = 1;
        WSession   = $Setfiles('FPRDP010');
        $AddDBDate($IntToDate(PRM_DatIni));
        $SetCriteria('(CDRAMO ='''+%CHAR(PRM_Cdramo)+''') AND +
                      (NUAPOL ='''+%CHAR(PRM_Nuapol)+''') AND +
                      (NUSQSUP='''+%CHAR(PRM_NuSequ)+''') AND +
                      (NUSEQU <> NUSQSUP)');
        DOW ($GetRecord(WIdxPrd) > *ZEROS);
           Lst#PRD(WIdxPrd).CDRAMO   = $GetFieldNum('CDRAMO');
           Lst#PRD(WIdxPrd).NUAPOL   = $GetFieldNum('NUAPOL');
           Lst#PRD(WIdxPrd).NUSEQU   = $GetFieldNum('NUSEQU');
           Lst#PRD(WIdxPrd).NUSQSUP  = $GetFieldNum('NUSQSUP');
           Lst#PRD(WIdxPrd).CDTEXTO  = $GetFieldNum('CDTEXTO');
           Lst#PRD(WIdxPrd).CPSEGURO = $GetFieldNum('CPSEGURO');
           Lst#PRD(WIdxPrd).IsEmpty  = *OFF;
           WIcExiste  = *ON;
           WIdxPrd += 1;
        ENDDO;
        $RmvDBSession(WSession);

        IF (WIdxPrd > 1);
           WIdxPrd = 1;
           DOW (NOT Lst#PRD(WIdxPrd).IsEmpty);
              WNuSqSup   = Lst#PRD(WIdxPrd).NUSEQU;
              WSession = $SetFiles('FCATP106');
              $AddDBDate($IntToDate(PRM_DatIni));
              $SetCriteria('(CDPROD = '''+%CHAR(PRM_CdProd)+''') AND +
                            (CDTEXTO='''+%CHAR(Lst#PRD(WIdxPrd).CDTEXTO)+''')');
              IF ($GetRecord() > *ZEROS);
                 WTexto = $GetField('DSTEXTO');
              ELSE;
                 WTexto = *BLANKS;
              ENDIF;
              $RmvDBSession(WSession);
              WIcExiste= DigValues(PRM_CdProd:PRM_CdRamo:PRM_NuApol:WNuSqSup:
                                   PRM_DatIni:WTexto);
              IF (WIcExiste);
                 WResultado = WIcExiste;
              ENDIF;
              WIdxPrd += 1;
           ENDDO;
        ELSE;
           //----------------------------------------------
           // Trabalhar o Registos da Lista
           //----------------------------------------------
           WSession = $SetFiles('FPRDP012');
           $AddDBDate($IntToDate(PRM_DatIni));
           $SetCriteria('(CDRAMO='''+%CHAR(PRM_Cdramo)+''') AND +
                         (NUAPOL='''+%CHAR(PRM_NUAPOL)+''') AND +
                         (NUSEQU='''+%CHAR(PRM_NUSEQU)+''')');
           IF ($GetRecord() > *ZEROS);
              WCapital = $GetFieldNum('CPSEGURO');
              WTexto   = $GetField('DSDETAIL');
              $RmvDBSession(WSession);

              WSession = $SetFiles('FPRDP013');
              $AddDBDate($IntToDate(PRM_DatIni));
              $SetCriteria('(CDRAMO='''+%CHAR(PRM_Cdramo)+''') AND +
                            (NUAPOL='''+%CHAR(PRM_NUAPOL)+''') AND +
                            (NUSEQU='''+%CHAR(PRM_NUSEQU)+''')');
              IF ($GetRecord() > *ZEROS);
                 WTexto   = $GetField('MORADA');
                 WCapital = $GetfieldNum('CPSEGURO');
              ENDIF;

              IF (NOT WIcMainHdr);
                 $AddTextLine();
                 $AddLineType('H1');
                 WLinha = 'BENS SEGUROS E OBJECTOS SEGUROS';
                 $AddTextLine(WLinha);
                 $AddLineType('T1');
                 WIcMainHdr = *ON;
              ENDIF;

              IF (WIcTitulo);
                 IF (WDsRiscoBk <> WDsRisco);
                    WDsRiscoBk = WDsRisco;
                    $AddLineType('H2');
                    $addException(*ON);
                    WLinha = *BLANKS;
                    %SUBST(WLinha:3) = WDsRisco;
                    %SUBST(WLinha:85) = 'CAPITAIS';
                    WLinha = %XLATE(StrLo:StrUp:WLinha);
                    $AddTextLine(WLinha);
                    $addException(*OFF);
                    $AddLineType('B1');
                 ENDIF;
                 WIcTitulo = *OFF;
              ENDIF;
              WLinha = WTexto;
              $AddTextLine(WLinha:1:*BLANKS:
                                       %EDITW(WCapital:'           '):'VL');
              WIcExiste = *ON;
           ENDIF;
           $RmvDBSession(WSession);
        ENDIF;

        RETURN WResultado;
      /END-FREE
     P DigValues       E
