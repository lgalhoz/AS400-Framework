     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *=======================================================
      * Autor .....: Luis Galhoz
      * Data ......: 2013/07/17
      * Programa ..: RCVSO052
      * Objectivo .: Actualizar os Riscos e Notas Descritivas
      *=======================================================
     FFCATP10101IF   E           K DISK    PREFIX(CAT_)                         Riscos
     F                                     RENAME(RCATP101:RCATP10101)
     FFCATP105  IF   E           K DISK    PREFIX(CAT_)                         Riscos com Notas
     FFCATP106  IF   E           K DISK    PREFIX(CAT_)                         Notas Descritivas
     FFPRDP010  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Notas Descritivas
     FFPRDP01001IF   E           K DISK    PREFIX(PRD_)                         Notas Descritivas
     F                                     RENAME(RPRDP010:RPRDP01001)
     FFPRDP01101IF   E           K DISK    PREFIX(PRD_)                         Riscos
     F                                     RENAME(RPRDP011:RPRDP01101)
     FFPRDP011  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Riscos
     FFPRDP012  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Texto das Notas
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura de dados do Registo
     D RBASE         E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Lista de Riscos da Apólice
     D LST#Risco       DS                  QUALIFIED DIM(250)
     D   NuSequ                       5S 0 INZ(*ZEROS)
     D   Capital                     11S 2 INZ(*ZEROS)
     D   Taxa                         5S 4 INZ(*ZEROS)
     D   Premio                      11S 2 INZ(*ZEROS)
     D   CdRisco                      5S 0 INZ(*ZEROS)
     D   Alias                        1A   INZ(*OFF)
     D   IcExiste                      N   INZ(*OFF)
     D   IsEmpty                       N
     D WIdx            S              3S 0
     D
     D*==> Definição dos Campos de Trabalho
     D WRamo           S              2S 0
     D WCodProd        S              5S 0
     D WNuSequ         S              5S 0
     D WApolice        S              7S 0
     D WData           S              8S 0
     D WDatSelect      S               D
     D WRegisto        S               *
     D WNuSequFound    S              5S 0
     D WNuSequRisco    S              5S 0
     D WNuSequNotSp    S              5S 0
     D WNuSequNotDp    S              5S 0
     D WCdTexto        S              5S 0
     D WCdRisco        S              5S 0
     D WIcSelected     S               N
     D WIcAction       S              1A
     D WIcExiste       S               N
     D WIcDeleted      S               N
     D
     D WAlias          S              1A
     D WSavCapital     S             11S 2 INZ(*ZEROS)
     D WSavPremio      S             11S 2 INZ(*ZEROS)
     D WSavTaxa        S              5S 4 INZ(*ZEROS)
     D
     D WRisco          S                   LIKE(OR_CR1)
     D WCapital        S                   LIKE(OR_CAP1)
     D WPremio         S                   LIKE(OR_PR1)
     D WTaxa           S                   LIKE(OR_TX1)
     D
     D*==> Deficnição de constantes
     D ACT#Criar       C                   'I'
     D ACT#Alterar     C                   'C'
     D ACT#Eliminar    C                   'D'
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WRamo
     C                   PARM                    WData
     C                   PARM                    WRegisto
     C
      /FREE
        //------------------------------------------------------
        // Instanciar Campos e Listas de Trabalho
        //------------------------------------------------------
        ptrOr = WRegisto;
        IF (OR_Condom = 'C');
           WCodProd = 11;
        ELSE;
           WCodProd = 5;
        ENDIF;
        WApolice   = OR_APOL;
        WDatSelect = $IntToDate(WData);
        WNuSequ    = *ZEROS;
        EXSR FillRiscos;

        //------------------------------------------------------
        // Actualizar os Riscos da Apólice
        //------------------------------------------------------
        IF (OR_CR1 <> *ZEROS) AND (OR_CR1 <> OR_CR2) AND (OR_CR1 <> OR_CR3);
           WRisco   = OR_CR1;
           WCapital = OR_CAP1;
           WPremio  = OR_PR1;
           WTaxa    = OR_TX1;
           EXSR UpdRisco;
        ENDIF;
        IF (OR_CR2 <> *ZEROS) AND (OR_CR2 <> OR_CR1) AND (OR_CR2 <> OR_CR3);
           WRisco   = OR_CR2;
           WCapital = OR_CAP2;
           WPremio  = OR_PR2;
           WTaxa    = OR_TX2;
           EXSR UpdRisco;
        ENDIF;
        IF (OR_CR3 <> *ZEROS) AND (OR_CR3 <> OR_CR1) AND (OR_CR3 <> OR_CR2);
           WRisco   = OR_CR3;
           WCapital = OR_CAP3;
           WPremio  = OR_PR3;
           WTaxa    = OR_TX3;
           EXSR UpdRisco;
        ENDIF;

        //------------------------------------------------------
        // Eliminar Riscos que foram retirados
        //------------------------------------------------------
        WIdx = 1;
        DOW (NOT LST#Risco(WIdx).IsEmpty);
           IF (NOT LST#Risco(WIdx).IcExiste);
              DELETE (WRamo:WApolice:LST#Risco(WIdx).NuSequ) RPRDP011;
              WCdRisco  = LST#Risco(WIdx).CdRisco;
              WIcAction = ACT#Eliminar;
              EXSR ActNotasDesc;
           ENDIF;
           WIdx += 1;
        ENDDO;
        RETURN;

        //===============================================================================
        // Subrotina.: UpdRisco
        // Objectivo.: Actualizar Tabelas de Códigos de Riscos e Itens de Notas
        //             descritivas.
        //===============================================================================
        BEGSR UpdRisco;
           //------------------------------------------------------
           // Actualizar Códigos de Risco
           //------------------------------------------------------
           WIdx = 1;
           DOW (NOT LST#Risco(WIdx).IsEmpty);
              WIcExiste = *OFF;
              WAlias    = LST#Risco(WIdx).Alias;
              WCdRisco  = LST#Risco(WIdx).CdRisco;
              IF (WRisco = %DEC(WAlias:1:0));
                 WIcExiste  = *ON;
                 LST#Risco(WIdx).IcExiste = *ON;
                 WSavCapital = LST#Risco(WIdx).Capital;
                 WSavPremio  = LST#Risco(WIdx).Premio;
                 WSavTaxa    = LST#Risco(WIdx).Taxa;
                 IF (WSavCapital <> WCapital) OR (WSavTaxa <> WTaxa)
                    OR (WSavPremio <> WPremio);
                    CHAIN (WRamo:WApolice:LST#Risco(WIdx).NuSequ) RPRDP011;
                    PRD_IMCPRISC = WCapital;
                    PRD_TXRISCO  = WTaxa;
                    PRD_IMPRRISC = WPremio;
                    UPDATE RPRDP011 %FIELDS(PRD_IMCPRISC:PRD_TXRISCO:
                                            PRD_IMPRRISC);

                    WIcAction = ACT#Alterar;
                    EXSR ActNotasDesc;
                 ENDIF;
                 LEAVE;
              ENDIF;
              WIdx += 1;
           ENDDO;

           //------------------------------------------------------
           // Inserir Códigos de Risco
           //------------------------------------------------------
           IF (NOT WIcExiste);
              PRD_CDRAMO   = WRamo;
              PRD_NUAPOL   = WApolice;
              PRD_NUSEQU   = *ZEROS;
              PRD_CDRISCO  = %DEC(WRisco:1:0);
              PRD_IMCPRISC = WCapital;
              PRD_TXRISCO  = WTaxa;
              PRD_IMPRRISC = WPremio;
              PRD_PRD01101 = WDatSelect;
              WRITE RPRDP011;

              WIcAction = ACT#Criar;
              EXSR ActNotasDesc;
           ENDIF;
        ENDSR;

       //=============================================================
       // Subrotina.: ActNotasDesc
       // Objectivo.: Actualizar Notas Descritivas.
       //=============================================================
       BEGSR ActNotasDesc;
          CHAIN (WCodProd:WRisco) RCATP105;
          IF (%FOUND(FCATP105));
             WCdTexto = CAT_CDTEXTO;
             IF (WIcAction = ACT#Alterar) OR (WIcAction = ACT#Eliminar);
                SETLL (WRamo:WApolice:WCdTexto) RPRDP01001;
                READE (WRamo:WApolice:WCdTexto) RPRDP01001;
                DOW (NOT %EOF(FPRDP01001));
                   IF (PRD_PRD01001<=WDatSelect) AND (PRD_PRD01002>=WDatSelect);
                      WNuSequNotSp = PRD_NUSEQU;
                      EXSR UpdDepend;
                      IF (WIcAction = ACT#Eliminar);
                         DELETE (WRamo:WApolice:WNuSequNotSp) RPRDP010;
                      ENDIF;
                      LEAVE;
                   ENDIF;
                   READE (WRamo:WApolice:WCdTexto) RPRDP01001;
                ENDDO;
             ELSEIF (WIcAction = ACT#Criar);
                Exsr AddNotasDesc;
             ENDIF;
          ENDIF;
       ENDSR;

       //=============================================================
       // Subrotina.: UpdDepend
       // Objectivo.: Actualizar Dependentes.
       //=============================================================
       BEGSR UpdDepend;
          SETLL (WRamo:WApolice:WNuSequNotSp) RPRDP012;
          READE (WRamo:WApolice:WNuSequNotSp) RPRDP012;
          DOW (NOT %EOF(FPRDP012));
             IF (PRD_PRD01201 <= WDatSelect) AND (PRD_PRD01202 >= WDatSelect);
                WNuSequNotDp = PRD_NUSEQHST;
                IF (WIcAction = ACT#Eliminar);
                   DELETE (WRamo:WApolice:WNuSequNotSp:WNuSequNotDp) RPRDP012;
                ELSE;
                   PRD_CPSEGURO = WCapital;
                   UPDATE RPRDP012 %FIELDS(PRD_CPSEGURO);
                ENDIF;
                LEAVE;
             ENDIF;
             READE (WRamo:WApolice:WNuSequNotSp) RPRDP012;
          ENDDO;
       ENDSR;

       //=============================================================
       // Subrotina.: AddNotasDesc
       // Objectivo.: Actualizar Notas Descritivas.
       //=============================================================
       BEGSR AddNotasDesc;
          PRD_CDRAMO  = WRamo;
          PRD_NUAPOL  = WApolice;
          PRD_NUSEQU  = *ZEROS;
          PRD_NUSQSUP = *ZEROS;
          PRD_CDTEXTO = WCdTexto;
          WRITE RPRDP010;

          //-----------------------------------------------
          // Obter o Código Superior
          //-----------------------------------------------
          SETLL (WRamo:WApolice) RPRDP010;
          READE (WRamo:WApolice) RPRDP010;
          DOW (NOT %EOF(FPRDP010));
            WNuSequNotSp = PRD_NUSEQU;
            READE (WRamo:WApolice) RPRDP010;
          ENDDO;

          //-----------------------------------------------
          // Obter o Texto por defeito para a Nota
          //-----------------------------------------------
          CHAIN (WCodProd:WCdTexto) RCATP106;
          IF (%FOUND(FCATP106));
             PRD_DSDETAIL = CAT_DSTEXTO;
          ELSE;
             PRD_DSDETAIL = '.';
          ENDIF;
          PRD_CDRAMO   = WRamo;
          PRD_NUAPOL   = WApolice;
          PRD_NUSEQU   = WNuSequNotSp;
          PRD_NUSEQHST = *ZEROS;
          PRD_ICDETAIL = 'N';
          PRD_CPSEGURO = WCapital;
          WRITE RPRDP012;
       ENDSR;

        //===============================================================================
        // Subrotina.: FillRiscos
        // Objectivo.: Actualizar lista de trabalho com os riscos activos
        //===============================================================================
        BEGSR FillRiscos;
           WIdx = 1;
           LST#Risco(WIdx).IsEmpty = *ON;
           SETLL (WRamo:WApolice) RPRDP01101;
           READE(N) (WRamo:WApolice) RPRDP01101;
           DOW (NOT %EOF(FPRDP01101));
              IF (PRD_PRD01101 <= WDatSelect) AND (PRD_PRD01102 >= WDatSelect);
                 LST#Risco(WIdx).NuSequ   = PRD_NUSEQU;
                 LST#Risco(WIdx).Capital  = PRD_IMCPRISC;
                 LST#Risco(WIdx).Taxa     = PRD_TXRISCO;
                 LST#Risco(WIdx).Premio   = PRD_IMPRRISC;
                 LST#Risco(WIdx).CdRisco  = PRD_CDRISCO;
                 LST#Risco(WIdx).Alias    = %CHAR(PRD_CDRISCO);
                 LST#Risco(WIdx).IcExiste = *OFF;
                 LST#Risco(WIdx).IsEmpty  = *OFF;
                 WIdx += 1;
                 LST#Risco(WIdx).IsEmpty = *ON;
              ENDIF;
              READE(N) (WRamo:WApolice) RPRDP01101;
           ENDDO;
        ENDSR;
      /END-FREE
