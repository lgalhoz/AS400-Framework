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
     D*==> Actualizar Coberturas da Apólice
     D UpdRisco        PR
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D   PRM_Alias                    1S 0 CONST
     D   PRM_CdRisco                  1S 0 CONST
     D   PRM_TxRisco                  5S 4 CONST
     D   PRM_CpRisco                 11S 0 CONST
     D   PRM_PrRisco                  9S 2 CONST
     D
     D*==> Definição dos Campos de Trabalho
     D WRamo           S              2S 0
     D WCodProduto     S              5S 0
     D WNuSequ         S              5S 0
     D WApolice        S              7S 0
     D WData           S              8S 0
     D WDatSelect      S               D
     D WRegisto        S               *
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WRamo
     C                   PARM                    WData
     C                   PARM                    WRegisto
     C
      /FREE
        //------------------------------------------------------
        // Actualizar o adicionar novos Registos
        //------------------------------------------------------
        ptrOr = WRegisto;
        IF (OR_Condom = 'C');
           WCodProduto = 11;
        ELSE;
           WCodProduto = 5;
        ENDIF;
        WDatSelect = $IntToDate(WData);
        WNuSequ    = *ZEROS;

        //------------------------------------------------------
        // Actualizar o as coberturas da Apólice
        //------------------------------------------------------
        IF (OR_CR1 <> *ZEROS) AND (OR_CR1 <> OR_CR2) AND (OR_CR1 <> OR_CR3);
           UpdRisco(WCodProduto:OR_RAMO:OR_APOL:1:OR_CR1:OR_TX1:OR_CAP1:OR_PR1);
        ENDIF;
        IF (OR_CR2 <> *ZEROS) AND (OR_CR2 <> OR_CR1) AND (OR_CR2 <> OR_CR3);
           UpdRisco(WCodProduto:OR_RAMO:OR_APOL:2:OR_CR2:OR_TX2:OR_CAP2:OR_PR2);
        ENDIF;
        IF (OR_CR3 <> *ZEROS) AND (OR_CR3 <> OR_CR1) AND (OR_CR3 <> OR_CR2);
           UpdRisco(WCodProduto:OR_RAMO:OR_APOL:3:OR_CR3:OR_TX3:OR_CAP3:OR_PR3);
        ENDIF;
        RETURN;
      /END-FREE

     P*===============================================================================
     P* Processo..: UpdRisco
     P*-------------------------------------------------------------------------------
     P* Objectivo.: Validar se a Cobertura está contratada para
     P*             a Apólices (trabalha com as duas versões das
     P*             Tabelas). Actualizar nova Tabela, partindo
     P*             do que encontrar na antiga.
     P*===============================================================================
     P UpdRisco        B                   EXPORT
     D                 PI
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D   PRM_Alias                    1S 0 CONST
     D   PRM_CdRisco                  1S 0 CONST
     D   PRM_TxRisco                  5S 4 CONST
     D   PRM_CpRisco                 11S 0 CONST
     D   PRM_PrRisco                  9S 2 CONST
     D
     D*==> Campos de Trabalho
     D WNuSequFound    S              5S 0
     D WNuSequRisco    S              5S 0
     D WNuSequNotSp    S              5S 0
     D WNuSequNotDp    S              5S 0
     D WCdTexto        S              5S 0
     D WCdRisco        S              5S 0
     D WAlias          S              1A
     D WIcSelected     S               N
     D WIcAction       S              1A
     D WIcExiste       S               N
     D WIcDeleted      S               N
     D*=======================================================
      /FREE
       //------------------------------------------------------
       // Localizar o Código do Risco
       //------------------------------------------------------
       WAlias = %CHAR(PRM_Alias);
       CHAIN (PRM_CdProd:WAlias) RCATP10101;
       IF (%FOUND());
          WCdrisco   = CAT_CDRISCO;
          WIcExiste  = *OFF;
          WIcDeleted = *OFF;
          SETLL (PRM_CdRamo:PRM_NuApol:WCdRisco) RPRDP01101;
          READE (PRM_CdRamo:PRM_NuApol:WCdRisco) RPRDP01101;
          DOW (NOT %EOF(FPRDP01101));
             IF (PRD_PRD01101 <= WDatSelect) AND (PRD_PRD01102 >= WDatSelect);
                WNuSequRisco = PRD_NUSEQU;
                WIcExiste    = *ON;
                IF (PRD_IMCPRISC <> PRM_CPRISCO) OR
                    (PRD_TXRISCO <> PRM_TxRisco) OR
                     (PRD_IMPRRISC <> PRM_PrRisco);
                   CHAIN (PRM_CdRamo:PRM_NuApol:WNuSequRisco) RPRDP011;
                   PRD_IMCPRISC = PRM_CPRISCO;
                   PRD_TXRISCO  = PRM_TXRISCO;
                   PRD_IMPRRISC = PRM_PRRISCO;
                   UPDATE RPRDP011 %FIELDS(PRD_IMCPRISC:PRD_TXRISCO:
                                           PRD_IMPRRISC);
                ENDIF;

                //-------------------------------------------------------
                // Validar Detalhe
                //-------------------------------------------------------
                CHAIN (PRM_CdProd:WCdRisco) RCATP105;
                IF (%FOUND(FCATP105));
                   WNuSequNotSp = PRD_NUSEQU;
                   WCdTexto = CAT_CDTEXTO;
                   WIcAction = 'U'; // Actualizar
                   EXSR ActNotasDesc;
                ENDIF;
                WIcExiste  = *ON;
                LEAVE;
             ENDIF;
             READE (PRM_CdRamo:PRM_NuApol:WCdRisco) RPRDP01101;
          ENDDO;

          IF (NOT WIcExiste);
             PRD_CDRAMO   = OR_RAMO;
             PRD_NUAPOL   = OR_APOL;
             PRD_NUSEQU   = *ZEROS;
             PRD_CDRISCO  = WCdRisco;
             PRD_IMCPRISC = PRM_CpRisco;
             PRD_TXRISCO  = PRM_TxRisco;
             PRD_IMPRRISC = PRM_PrRisco;
             PRD_PRD01101 = WDatSelect;
             WRITE RPRDP011;
             WIcAction = 'I'; // Criar
             EXSR ActNotasDesc;
          ELSEIF (WIcExiste AND NOT WIcSelected AND WIcDeleted);
             DELETE (OR_RAMO:OR_APOL:WNuSequRisco) RPRDP011;
             WIcAction = 'D'; // Eliminar
             EXSR ActNotasDesc;
          ENDIF;
       ENDIF;
       RETURN;

       //=============================================================
       // Subrotina.: ActNotasDesc
       // Objectivo.: Actualizar Notas Descritivas.
       //=============================================================
       BEGSR ActNotasDesc;
          IF (WIcAction <> 'I');
             IF (WIcAction = 'U');
                WIcAction = 'I';
             ENDIF;

             SETLL (PRM_CdProd:PRM_NuApol:WCdTexto) RPRDP01001;
             READE (PRM_CdProd:PRM_NuApol:WCdTexto) RPRDP01001;
             DOW (NOT %EOF(FPRDP01001));
                IF (PRD_PRD01001<=WDatSelect) AND (PRD_PRD01002>=WDatSelect);
                   WNuSequNotSp = PRD_NUSEQU;
                   EXSR UpdDepend;
                   IF (WIcAction = 'D');
                      DELETE (PRM_CdRamo:PRM_NuApol:WNuSequNotSp) RPRDP010;
                   ELSE;
                      WIcAction = 'U';
                   ENDIF;
                   LEAVE;
                ENDIF;
                READE (PRM_CdProd:PRM_NuApol:WCdTexto) RPRDP01001;
             ENDDO;
          ENDIF;

          IF (WIcAction = 'I');
             Exsr AddNotasDesc;
          ENDIF;
       ENDSR;

       //=============================================================
       // Subrotina.: UpdDepend
       // Objectivo.: Actualizar Dependentes.
       //=============================================================
       BEGSR UpdDepend;
          SETLL (PRM_CdRamo:PRM_NuApol:WNuSequNotSp) RPRDP012;
          READE (PRM_CdRamo:PRM_NuApol:WNuSequNotSp) RPRDP012;
          DOW (NOT %EOF(FPRDP012));
             IF (PRD_PRD01201 <= WDatSelect) AND (PRD_PRD01202 >= WDatSelect);
                WNuSequNotDp = PRD_NUSEQHST;
                IF (WIcAction = 'D');
                   DELETE (PRM_CdRamo:PRM_NuApol:WNuSequNotSp:WNuSequNotDp)
                      RPRDP012;
                ELSE;
                   IF (PRD_CPSEGURO <> PRM_CpRisco);
                      CHAIN (PRM_CdRamo:PRM_NuApol:WNuSequNotSp:WNuSequNotDp)
                         RPRDP012;
                      PRD_CPSEGURO = PRM_CpRisco;
                      UPDATE RPRDP012 %FIELDS(PRD_CPSEGURO);
                   ENDIF;
                ENDIF;
                LEAVE;
             ENDIF;
             READE (PRM_CdRamo:PRM_NuApol:WNuSequNotSp) RPRDP012;
          ENDDO;
       ENDSR;

       //=============================================================
       // Subrotina.: AddNotasDesc
       // Objectivo.: Actualizar Notas Descritivas.
       //=============================================================
       BEGSR AddNotasDesc;
          CHAIN (PRM_CdProd:WCdRisco) RCATP105;
          IF (%FOUND(FCATP105));
             PRD_CDRAMO  = PRM_CdRamo;
             PRD_NUAPOL  = PRM_NuApol;
             PRD_NUSEQU  = *ZEROS;
             PRD_NUSQSUP = *ZEROS;
             PRD_CDTEXTO = CAT_CDTEXTO;
             WRITE RPRDP010;

             //-----------------------------------------------
             // Obter o Código Superior
             //-----------------------------------------------
             SETLL (PRM_CdRamo:PRM_NuApol) RPRDP010;
             READE (PRM_CdRamo:PRM_NuApol) RPRDP010;
             DOW (NOT %EOF(FPRDP010));
               WNuSequNotSp = PRD_NUSEQU;
               READE (PRM_CdRamo:PRM_NuApol) RPRDP010;
             ENDDO;

             //-----------------------------------------------
             // Obter o Texto por defeito para a Nota
             //-----------------------------------------------
             CHAIN (PRM_CdProd:CAT_CDTEXTO) RCATP106;
             IF (%FOUND(FCATP106));
                PRD_DSDETAIL = CAT_DSTEXTO;
             ELSE;
                PRD_DSDETAIL = '.';
             ENDIF;
             PRD_CDRAMO   = PRM_CdRamo;
             PRD_NUAPOL   = PRM_NuApol;
             PRD_NUSEQU   = WNuSequNotSp;
             PRD_NUSEQHST = *ZEROS;
             PRD_ICDETAIL = 'N';
             PRD_CPSEGURO = PRM_CpRisco;
             WRITE RPRDP012;
          ENDIF;
       ENDSR;
      /END-FREE
     P UpdRisco        E
