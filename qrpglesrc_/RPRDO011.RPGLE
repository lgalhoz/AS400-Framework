     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RRDO011
     H* DescriÁ„o.: InformaÁ„o Standard de Multiriscos HabitaÁ„o
     H*==================================================================
     FFCATP105  IF   E           K DISK    PREFIX(CAT_)
     FFCATP106  IF   E           K DISK    PREFIX(CAT_)
     FFCATP10601IF   E           K DISK    RENAME(RCATP106:RCATP10601)
     F                                     PREFIX(CAT_)
     FFPRDP010  IF   E           K DISK    PREFIX(PRD_)
     FFPRDP01001IF   E           K DISK    RENAME(RPRDP010:RPRDP01001)
     F                                     PREFIX(PRD_)
     FFPRDP01002IF   E           K DISK    RENAME(RPRDP010:RPRDP01002)
     F                                     PREFIX(PRD_)
     FFPRDP012  IF   E           K DISK    PREFIX(PRD_)
     FFPRDP013  IF   E           K DISK    PREFIX(PRD_)
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010

     D*==> Escavar lista de Cat·logo
     D DigCatalogo     PR              N
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_NuApol                  7S 0 CONST
     D    PRM_CdTexto                 5S 0 CONST
     D    PRM_DatIni                  8S 0 CONST
     D
     D*==> Escavar lista de valores da ApÛlice
     D DigValues       PR              N
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_NuApol                  7S 0 CONST
     D    PRM_NuSequ                  5S 0 CONST
     D    PRM_DatIni                  8S 0 CONST
     D    PRM_DsTexto               100A   CONST
     D
     D*==> Parametros recebidos
     D WDsRisco        S             40A
     D WDsRiscoBk      S             40A
     D WIcTitulo       S               N
     D WIcMainHdr      S               N
     D WIcExiste       S               N
     D WLinha          S            500A
     D WCdProd         S              5S 0
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyzÁ+
     D                                     „·‚‡ÈËÍÌÏÓÛÚıÙ˙˘˚'
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ«+
     D                                     √¡¬¿…» ÕÃŒ”“’‘⁄Ÿ€'
     D
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
      /FREE

         WIcMainHdr = *OFF;
         WDsRiscoBk = *BLANKS;
         SETLL (WCdProd) RCATP105;
         READE (WCdProd) RCATP105;
         DOW (NOT %EOF(FCATP105));
            //----------------------------------------------
            // Obter dependentes ao nÌvel do Cat·logo
            //----------------------------------------------
            WIcTitulo = *ON;
            WDsRisco  = $getDescRisco(WCdProd:CAT_CdRisco);
            //IF (WDsRisco <> WDsRiscoBk);
            //   WDsRiscoBk = WDsRisco;
            //ENDIF;
            WIcExiste=DigCatalogo(WCdProd:WRamo:WApolice:
                                  CAT_CdTexto:WDatInicio);
            READE (WCdProd) RCATP105;
         ENDDO;

         *INLR = *ON;
         RETURN;

      /END-FREE
     P*=======================================================
     P* Processo..: DigCatalogo
     P* Objectivo.: Precorrer a ·rvore do Cat·logo
     P*=======================================================
     P DigCatalogo     B                   EXPORT
     D DigCatalogo     PI              N
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_NuApol                  7S 0 CONST
     D    PRM_CdTexto                 5S 0 CONST
     D    PRM_DatIni                  8S 0 CONST
     D
     D*==> Registos de Catalogo
     D Lst#CAT         DS                  QUALIFIED DIM(50)
     D   CDPROD                            LIKE(CAT_CDPROD)
     D   CDTEXTO                           LIKE(CAT_CDTEXTO)
     D   CDSUPER                           LIKE(CAT_CDSUPER)
     D   NUORDEM                           LIKE(CAT_NUORDEM)
     D   CDGRUPO                           LIKE(CAT_CDGRUPO)
     D   DSTEXTO                           LIKE(CAT_DSTEXTO)
     D   ICOBRIGA                          LIKE(CAT_ICOBRIGA)
     D   ALIAS                             LIKE(CAT_ALIAS)
     D   CONVERSAO                         LIKE(CAT_CONVERSAO)
     D   ICRESERV                          LIKE(CAT_ICRESERV)
     D   CDFUNCT                           LIKE(CAT_CDFUNCT)
     D   ICDETAIL                          LIKE(CAT_ICDETAIL)
     D   DTINICIO                          LIKE(CAT_CAT10601)
     D   DTTERMO                           LIKE(CAT_CAT10602)
     D   IsEmpty                       N   INZ(*ON)
     D WIdxCat         S              5S 0
     D WCdSuper        S                   LIKE(CAT_CDSUPER)
     D WNuSequ         S                   LIKE(PRD_NUSEQU)
     D WTexto          S                   LIKE(PRD_DSDETAIL)
     D WIcExiste       S               N   INZ(*OFF)
     D WResultado      S               N
     D*=================================================================
      /FREE

        //----------------------------------------------
        // Trabalhar o Registos da Lista
        //----------------------------------------------
        WIcExiste  = *OFF;
        WResultado = WIcExiste;
        WCdSuper   = PRM_CdTexto;
        CHAIN (PRM_CdRamo:PRM_NuApol:PRM_CdTexto) RPRDP01001;
        IF (%FOUND(FPRDP01001));
           CHAIN (PRM_CdProd:PRM_CdTexto) RCATP106;
           WTexto  = CAT_DSTEXTO;
           WIcExiste=DigValues(PRM_CdProd:PRM_CdRamo:PRM_NuApol:
                               PRD_NUSEQU:PRM_DatIni:CAT_DsTexto);
           IF (WIcExiste);
              WResultado = WIcExiste;
           ENDIF;
        ELSE;
           //----------------------------------------------
           // Obter dependentes ao nÌvel do Cat·logo
           //----------------------------------------------
           WIdxCat = *Zeros;
           SETLL (PRM_CdProd:PRM_CdTexto) RCATP10601;
           READE (PRM_CdProd:PRM_CdTexto) RCATP10601;
           DOW (NOT %EOF(FCATP10601));
              IF (CAT_CdTexto <> CAT_CdSuper);
                 WIdxCat += 1;
                 Lst#CAT(WIdxCat).CDPROD    = CAT_CDPROD;
                 Lst#CAT(WIdxCat).CDTEXTO   = CAT_CDTEXTO;
                 Lst#CAT(WIdxCat).CDSUPER   = CAT_CDSUPER;
                 Lst#CAT(WIdxCat).NUORDEM   = CAT_NUORDEM;
                 Lst#CAT(WIdxCat).CDGRUPO   = CAT_CDGRUPO;
                 Lst#CAT(WIdxCat).DSTEXTO   = CAT_DSTEXTO;
                 Lst#CAT(WIdxCat).ICOBRIGA  = CAT_ICOBRIGA;
                 Lst#CAT(WIdxCat).ALIAS     = CAT_ALIAS;
                 Lst#CAT(WIdxCat).CONVERSAO = CAT_CONVERSAO;
                 Lst#CAT(WIdxCat).ICRESERV  = CAT_ICRESERV;
                 Lst#CAT(WIdxCat).CDFUNCT   = CAT_CDFUNCT;
                 Lst#CAT(WIdxCat).ICDETAIL  = CAT_ICDETAIL;
                 Lst#CAT(WIdxCat).DTINICIO  = CAT_CAT10601;
                 Lst#CAT(WIdxCat).DTTERMO   = CAT_CAT10602;
                 Lst#CAT(WIdxCat).IsEmpty   = *OFF;
              ENDIF;
              READE (PRM_CdProd:PRM_CdTexto) RCATP10601;
           ENDDO;

           IF (WIdxCat > *ZEROS);
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

        RETURN WResultado;
      /END-FREE
     P DigCatalogo     E

     P*=======================================================
     P* Processo..: DigValues
     P* Objectivo.: Precorrer a ·rvore do valores da ApÛlice
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
     D Lst#PRD         DS                  QUALIFIED DIM(50)
     D   CDPROD                            LIKE(PRD_CDRAMO)
     D   NUAPOL                            LIKE(PRD_NUAPOL)
     D   NUSEQU                            LIKE(PRD_NUSEQU)
     D   NUSQSUP                           LIKE(PRD_NUSQSUP)
     D   CDTEXTO                           LIKE(PRD_CDTEXTO)
     D   CPSEGURO                          LIKE(PRD_CPSEGURO)
     D   PRD01001                          LIKE(PRD_PRD01001)
     D   PRD01002                          LIKE(PRD_PRD01002)
     D   IsEMpty                       N   INZ(*ON)
     D WIdxPrd         S              5S 0
     D WNuSqSup        S                   LIKE(PRD_NUSQSUP)
     D WNuSequ         S                   LIKE(PRD_NUSEQU)
     D WTexto          S                   LIKE(PRD_DSDETAIL)
     D WCapital        S                   LIKE(PRD_CPSEGURO)
     D WIcExiste       S               N
     D WResultado      S               N
     D*=================================================================
      /FREE

        //----------------------------------------------
        // Obter dependentes ao nÌvel do Cat·logo
        //----------------------------------------------
        WIcExiste  = *OFF;
        WResultado = WIcExiste;
        WTexto     = PRM_DSTEXTO;
        WIdxPrd    = *Zeros;
        SETLL (PRM_CdRamo:PRM_NuApol:PRM_NuSequ) RPRDP01002;
        READE (PRM_CdRamo:PRM_NuApol:PRM_NuSequ) RPRDP01002;
        DOW (NOT %EOF(FPRDP01002));
           IF (PRD_NUSEQU <> PRD_NUSQSUP);
           WIdxPrd += 1;
              Lst#PRD(WIdxPrd).CDPROD    = PRD_CDRAMO;
              Lst#PRD(WIdxPrd).NUAPOL    = PRD_NUAPOL;
              Lst#PRD(WIdxPrd).NUSEQU    = PRD_NUSEQU;
              Lst#PRD(WIdxPrd).NUSQSUP   = PRD_NUSQSUP;
              Lst#PRD(WIdxPrd).CDTEXTO   = PRD_CDTEXTO;
              Lst#PRD(WIdxPrd).CPSEGURO  = PRD_CPSEGURO;
              Lst#PRD(WIdxPrd).PRD01001  = PRD_PRD01001;
              Lst#PRD(WIdxPrd).PRD01002  = PRD_PRD01002;
              Lst#PRD(WIdxPrd).IsEmpty   = *OFF;
           ENDIF;
           READE (PRM_CdRamo:PRM_NuApol:PRM_NuSequ) RPRDP01002;
        ENDDO;

        IF (WIdxPrd > *ZEROS);
           WIdxPrd = 1;
           DOW (NOT Lst#PRD(WIdxPrd).IsEmpty);
              WNuSqSup   = Lst#PRD(WIdxPrd).NUSEQU;
              CHAIN (WRamo:Lst#PRD(WIdxPrd).CDTEXTO) RCATP106;
              WTexto   = CAT_DSTEXTO;
              WIcExiste=DigValues(PRM_CdProd:PRM_CdRamo:PRM_NuApol:WNuSqSup:
                                  PRM_DatIni:CAT_DSTEXTO);
              IF (WIcExiste);
                 WResultado = WIcExiste;
              ENDIF;
              WIdxPrd += 1;
           ENDDO;
        ELSE;
           //----------------------------------------------
           // Trabalhar o Registos da Lista
           //----------------------------------------------
           CHAIN (PRM_CdRamo:PRM_NuApol:PRM_NuSequ) RPRDP010;
           IF (%FOUND(FPRDP010));
              WCapital = PRD_CPSEGURO;
              CHAIN (PRM_CdRamo:PRM_NuApol:PRM_NuSequ) RPRDP012;
              IF (%FOUND(FPRDP012));
                 WTexto   = PRD_DSDETAIL;
                 WCapital = PRD_CPSEGURO;
                 CHAIN (PRM_CdRamo:PRM_NuApol:PRM_NuSequ) RPRDP013;
                 IF (%FOUND(FPRDP013));
                    WTexto   = PRD_MORADA;
                    WCapital = PRD_CPSEGURO;
                 ENDIF;
              ENDIF;
              IF (NOT WIcMainHdr);
                 $AddTextLine();
                 $AddLineType('H1');
                 WLinha = 'Nota Descritiva:';
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
                    %SUBST(WLinha:82) = 'CAPITAIS';
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
        ENDIF;

        RETURN WResultado;
      /END-FREE
     P DigValues       E
