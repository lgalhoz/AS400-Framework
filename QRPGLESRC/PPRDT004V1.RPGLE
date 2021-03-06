     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *=======================================================
      * Autor......: Luis Galhoz
      * Programa...: PPRDT004
      * Objectivo..: Actualizar ficheiro de �ndices de Ap�lices
      *              com as Ap�lices Tempor�rias de Autom�vel e
      *              Outros Ramos.
      *=======================================================
     FFPRDT001  UF A E           K DISK    PREFIX(IDX_)
     FBASEGROT  IF   E           K DISK    EXTFILE('BASEGROT')
     F                                     PREFIX(GRL_)
     FBASEGRAT  IF   E           K DISK    EXTFILE('BASEGRAT')
     F                                     PREFIX(GRL_)
     FGBASEDIA  IF   E           K DISK    EXTFILE('GBASEDIA')
     F                                     PREFIX(GRL_)
     FGBASEDIO  IF   E           K DISK    EXTFILE('GBASEDIO')
     F                                     PREFIX(GRL_)
     FGFSEG     IF   E           K DISK    EXTFILE('GFSEG')
     F                                     PREFIX(SEG_)
     F*==========================================================
     D WIcCriar        S               N
     D WIcChange       S               N
     D
     D*==> Informa��o do Ap�lice
     D WCdRamo         S                   LIKE(IDX_CDRAMO)
     D WNuApol         S                   LIKE(IDX_NUAPOL)
     D
     D*==> Informa��o do Segurado
     D WNuSeg          S                   LIKE(IDX_NUSEG)
     D WNoSeg          S                   LIKE(IDX_NOSEG)
     D WApelido        S                   LIKE(IDX_APELIDO)
     D WNuNIF          S                   LIKE(IDX_NUNIF)
     D
     D*==> Informa��o do Risco
     D WMatric         S                   LIKE(IDX_MATRIC)
     D WStatus         S                   LIKE(IDX_STATUS)
     D WLcRisk         S                   LIKE(IDX_LCRISK)
     D
     D*==> Informa��o da Media��o
     D WCdMED1         S                   LIKE(IDX_CDMED1)
     D WCdJMM1         S                   LIKE(IDX_CDJMM1)
     D WCdJMM2         S                   LIKE(IDX_CDJMM2)
     D WIcCarr         S                   LIKE(IDX_ICCARR)
     C*========================================================
      /FREE
         //------------------------------------------------------
         // Actualizar o adicionar novos Registos
         //------------------------------------------------------
         SETLL (*LOVAL) BASEGROT;
         READ BASEGROT;
         DOW (NOT %EOF(BASEGROT));
            EXSR UpdIndice;
            READ BASEGROT;
         ENDDO;

         SETLL (*LOVAL) BASEGRAT;
         READ BASEGRAT;
         DOW (NOT %EOF(BASEGRAT));
            EXSR UpdIndice;
            READ BASEGRAT;
         ENDDO;

         *INLR = *ON;
         RETURN;

         //=======================================================================
         // Subrotina...: UpdIndice
         // Objectivo...: Actualizar o Registo ou Adicionar se n�o Existir
         //=======================================================================
         BEGSR UpdIndice;
            WIcChange = *OFF;
            WIcCriar  = *OFF;

            //----------------------------------------------
            // Obter vers�o mais actualizada do registo
            //----------------------------------------------
            IF (WCdRamo <> 50) AND (WCdRamo <> 51);
               CHAIN (WCdRamo:WNuApol) GBASEDIO;
               IF (%FOUND(GBASEDIO));
                  WIcCarr = 'S';
               ELSE;
                  WIcCarr = 'N';
               ENDIF;
               WLcRisk = GRL_LRPS;
               WMatric = *BLANKS;
            ELSE;
               CHAIN (WCdRamo:WNuApol) GBASEDIA;
               IF (%FOUND(GBASEDIA));
                  WIcCarr = 'S';
               ELSE;
                  WIcCarr = 'N';
               ENDIF;
               WLcRisk = *BLANKS;
               WMatric = GRL_MATR1;
            ENDIF;

            //----------------------------------------------
            // ForCompr restantes campos do ficheiro
            //----------------------------------------------
            WNuSeg  = GRL_NSEG;
            WCdRamo = GRL_RAMO;
            WNuApol = GRL_APOL;
            WStatus = GRL_SIT;
            WCdMED1 = GRL_MED1;
            WCdJMM1 = GRL_JMM1;
            WCdJMM2 = GRL_JMM2;

            //----------------------------------------------
            // Obter registo do Indice e comparar
            //----------------------------------------------
            CHAIN(N) (WCdRamo:WNuApol) RPRDP001;
            IF (NOT %FOUND(FPRDT001));
               WIcCriar = *ON;
            ELSE;
               IF (IDX_STATUS <> WStatus);
                  WIcChange = *ON;
               ENDIF;
               IF (%TRIM(IDX_MATRIC) <> %TRIM(WMatric));
                  WIcChange = *ON;
               ENDIF;
               IF (%TRIM(IDX_LCRISK) <> %TRIM(WLcRisk));
                  WIcChange = *ON;
               ENDIF;
               IF (IDX_CDMED1  <> WCdMed1);
                  WIcChange = *ON;
               ENDIF;
               IF (IDX_CDJMM1 <> WCdJMM1);
                  WIcChange = *ON;
               ENDIF;
               IF (IDX_CDJMM2 <> WCdJMM2);
                  WIcChange = *ON;
               ENDIF;
               IF (IDX_LCRISK <> WLcRisk);
                  WIcChange = *ON;
               ENDIF;
               IF (IDX_ICCARR <> WIcCarr);
                  WIcChange = *ON;
               ENDIF;
               IF (IDX_NUSEG <> WNuSeg);
                  WIcChange = *ON;
               ENDIF;
            ENDIF;

            //----------------------------------------------
            // Actualizar/Criar a informa��o do Registo
            //----------------------------------------------
            IF (WIcChange) OR (WIcCriar);
               IF (WIcChange);
                  CHAIN (WCdRamo:WNuApol) RPRDP001;
               ENDIF;

               CHAIN (WNuSeg) GFSEG;
               IF (%FOUND(GFSEG));
                  WNoSeg    = SEG_NOM;
                  WApelido  = SEG_APELID;
                  WNuNIF    = SEG_NIF;
               ENDIF;

               IDX_CDRAMO   = WCdRamo;
               IDX_NUAPOL   = WNuApol;
               IDX_NuSeg    = WNuSeg;
               IDX_NoSeg    = WNoSeg;
               IDX_Apelido  = WApelido;
               IDX_NUNIF    = WNuNIF;
               IDX_MATRIC   = WMatric;
               IDX_STATUS   = WStatus;
               IDX_LCRISK   = WLcRisk;
               IDX_CDMED1   = WCdMed1;
               IDX_CDJMM1   = WCdJMM1;
               IDX_CDJMM2   = WCdJMM2;
               IDX_ICCARR   = WIcCarr;
               IDX_TPACTA   = 'A';
               IDX_CDPROD   = WCdRamo;
               IDX_PRD00101 = %DATE('2000-01-01':*ISO);
               IDX_PRD00102 = %DATE('9999-12-31':*ISO);

               IF (WIcChange);
                  UPDATE RPRDP001 %FIELDS(IDX_STATUS:
                                     IDX_NUSEG:IDX_NOSEG:IDX_APELIDO:IDX_NUNIF:
                                     IDX_MATRIC:IDX_LCRISK:
                                     IDX_CDMED1:IDX_CDJMM1:IDX_CDJMM2:
                                     IDX_ICCARR);
               ELSEIF (WIcCriar);
                  WRITE  RPRDP001;
               ENDIF;
            ENDIF;
         ENDSR;
      /END-FREE
