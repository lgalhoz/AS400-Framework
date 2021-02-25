     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *=======================================================
      * Autor .....: Luis Galhoz
      * Data ......: 2013/07/17
      * Programa ..: RCVSO051
      * Objectivo .: Actualizar as Coberturas das Apólices
      *=======================================================
     FFCATP10201IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP102:RCATP10201)
     FFPRDP008  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP00801IF   E           K DISK    PREFIX(PRD_)
     F                                     RENAME(RPRDP008:RPRDP00801)
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
     D UpdCobertura    PR
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D   PRM_CdCobert                 1A   CONST
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
        // Actualizar o as coberturas da Apólice
        //------------------------------------------------------
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF01:'01');
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF02:'02');
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF03:'03');
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF04:'04');
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF05:'05');
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF08:'08');
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF09:'09');
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF10:'10');
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF11:'11');
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF12:'12');
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF13:'13');

        //-----------------------------------------------------------
        // Martelada para os Fenómenos Sísmicos
        //-----------------------------------------------------------
        UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF07:'07');
        IF (OR_CF07 = 'S') AND (OR_CF0800 <> 'S') AND (OR_CF0830 <> 'S');
           OR_CF0800 = 'S';
           UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF0800:'0800');
        ELSEIF (OR_CF07 <> 'S') AND ((OR_CF0800 = 'S') OR (OR_CF0830 = 'S'));
           OR_CF07 = 'S';
           UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF07:'07');
           UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF0800:'0800');
           UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF0830:'0830');
        ENDIF;

        //-----------------------------------------------------------
        // Martelada para os Riscos Electricos
        //-----------------------------------------------------------
        IF (OR_CF06 = 'S');
           UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:OR_CF06:'06');
           UpdCobertura(WCodProduto:OR_RAMO:OR_APOL:'S':'0602');
        ENDIF;
        RETURN;
      /END-FREE

     P*===============================================================================
     P* Processo..: UpdCobertura
     P*-------------------------------------------------------------------------------
     P* Objectivo.: Validar se a Cobertura está contratada para
     P*             a Apólices (trabalha com as duas versões das
     P*             Tabelas). Actualizar nova Tabela, partindo
     P*             do que encontrar na antiga.
     P*===============================================================================
     P UpdCobertura    B                   EXPORT
     D                 PI
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D   PRM_CdCobert                 1A   CONST
     D   PRM_Alias                   10A   CONST
     D
     D*==> Campos de Trabalho
     D WNuSequFound    S              5S 0
     D WCdCobert       S              5S 0
     D WIcSelected     S               N
     D WIcExiste       S               N
     D WIcDeleted      S               N
     D*=======================================================
      /FREE
       //------------------------------------------------------
       // Validar se o Código está Seleccionado
       //------------------------------------------------------
       WIcSelected = *OFF;
       IF (PRM_CdCobert = 'S');
          WIcSelected = *ON;
       ENDIF;

       //------------------------------------------------------
       // Localizar o Código da Cobertura
       //------------------------------------------------------
       CHAIN (PRM_CdProd:PRM_Alias:'F') RCATP10201;
       IF (%FOUND());
          WCdCobert  = CAT_CDCOBERT;
          WIcExiste  = *OFF;
          WIcDeleted = *OFF;
          SETLL (PRM_CdRamo:PRM_NuApol:WCdCobert) RPRDP00801;
          READE (PRM_CdRamo:PRM_NuApol:WCdCobert) RPRDP00801;
          DOW (NOT %EOF(FPRDP00801));
             IF (PRD_PRD00801 <= WDatSelect) AND (PRD_PRD00802 >= WDatSelect);
                WNuSequFound = PRD_NUSEQU;
                WIcExiste    = *ON;
                IF (PRD_PRD00802 = %DATE('2999-12-31':*ISO));
                   WIcDeleted = *ON;
                ENDIF;
                LEAVE;
             ENDIF;
             READE (PRM_CdRamo:PRM_NuApol:WCdCobert) RPRDP00801;
          ENDDO;

          IF (NOT WIcExiste AND WIcSelected);
             PRD_CDRAMO   = OR_RAMO;
             PRD_NUAPOL   = OR_APOL;
             PRD_CDCOBERT = WCdCobert;
             PRD_NUSEQU   = *ZEROS;
             PRD_PRD00801 = WDatSelect;
             WRITE RPRDP008;
          ELSEIF (WIcExiste AND NOT WIcSelected AND NOT WIcDeleted);
             DELETE (OR_RAMO:OR_APOL:WNuSequFound) RPRDP008;
          ENDIF;
       ENDIF;
       RETURN;

      /END-FREE
     P UpdCobertura    E
