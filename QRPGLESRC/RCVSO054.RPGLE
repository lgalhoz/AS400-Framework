     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *=======================================================
      * Autor .....: Luis Galhoz
      * Data ......: 2013/07/17
      * Programa ..: RCVSO053
      * Objectivo .: Actualizar Clausulas e Condições
      *=======================================================
     FFCATP10802IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP108:RCATP10802)
     FFPRDP016  UF A E           K DISK    PREFIX(PRD_) COMMIT
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
     D UpdClausula     PR
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D   PRM_Clausula                 1A   CONST
     D   PRM_Alias                   10A   CONST
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
        // Actualizar a caracterização do risco da Apólice
        //------------------------------------------------------
        PRD_CDRAMO    = OR_RAMO;   // Ramo
        PRD_NUAPOL    = OR_APOL;   // Número da Apólice
        PRD_NUSEQU    = *ZEROS;    // Sequencia
        PRD_CDPOST    = OR_CPMOR1; // Código Postal
        PRD_CDCREDHIP = OR_CRHIP;  // Credor Hipotecário
        PRD_TXINDEX   = OR_TXIND;  // Taxa de Indexação
        PRD_OBS1      = OR_OBS1;   // Observações - Linha 01
        PRD_OBS2      = OR_OBS2;   // Observações - Linha 02

        IF (OR_FILO3 <> *BLANKS);
           //--------------------------
           // Indice Base
           //--------------------------
           MONITOR;
              PRD_CDIDXBASE = %DEC(%SUBST(OR_FILO3:1:3):3:0);
           ON-ERROR;
              PRD_CDIDXBASE = *ZEROS;
           ENDMON;

           //--------------------------
           // Indice Vencimento
           //--------------------------
           MONITOR;
              PRD_CDIDXVENC = %DEC(%SUBST(OR_FILO3:4:3):3:0);
           ON-ERROR;
              PRD_CDIDXVENC = *ZEROS;
           ENDMON;
        ELSE;
           PRD_CDIDXBASE = *ZEROS;    // Indice Base
           PRD_CDIDXVENC = *ZEROS;    // Indice Vencimento
        ENDIF;

        //-------------------------------------
        // Estado Conservação
        //-------------------------------------
        IF (OR_ESTCON = '1');
           PRD_CDESTCONS = 'B';
        ELSEIF (OR_ESTCON = '2');
           PRD_CDESTCONS = 'D';
        ELSEIF (OR_ESTCON = '3');
           PRD_CDESTCONS = 'M';
        ELSE;
           PRD_CDESTCONS = 'B'; // Valor por defeito
        ENDIF;

        //-------------------------------------
        // Tipo de Construção
        //-------------------------------------
        IF (OR_TIPCON <> *BLANKS);
           PRD_CDTIPCONS = '00'+OR_TIPCON;
        ELSE;
           PRD_CDTIPCONS = '001'; // Materiais incombustiveis por defeito
        ENDIF;

        //-------------------------------------
        // Tipo Cobertura
        //-------------------------------------
        PRD_CDTIPCOB  = '001'; // Telha

        //-------------------------------------
        // Edificio Adjacente
        //-------------------------------------
        IF (OR_CF21 = 'S');
           PRD_CDEDIADJ = '00000';
        ELSE;
           PRD_CDEDIADJ = '00000';
        ENDIF;
        WRITE RPRDP016;
        RETURN;
      /END-FREE
