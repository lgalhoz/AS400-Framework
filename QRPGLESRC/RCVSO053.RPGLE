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
     FFPRDP009  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP00901IF   E           K DISK    PREFIX(PRD_)
     F                                     RENAME(RPRDP009:RPRDP00901)
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
        // Actualizar o as coberturas da Apólice
        //------------------------------------------------------
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CE001:'001');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CE002:'002');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CE003:'003');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CE004:'004');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CA030:'030');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CA031:'031');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CA100:'100');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CA101:'101');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CA102:'102');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CD103:'103');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CD130:'130');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CD131:'131');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CD132:'132');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CD133:'133');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CD134:'134');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CD135:'135');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CD136:'136');
        UpdClausula(WCodProduto:OR_RAMO:OR_APOL:OR_CD137:'137');
        RETURN;
      /END-FREE

     P*===============================================================================
     P* Processo..: UpdClausula
     P*-------------------------------------------------------------------------------
     P* Objectivo.: Validar se a Cobertura está contratada para
     P*             a Apólices (trabalha com as duas versões das
     P*             Tabelas). Actualizar nova Tabela, partindo
     P*             do que encontrar na antiga.
     P*===============================================================================
     P UpdClausula     B                   EXPORT
     D                 PI
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D   PRM_Clausula                 1A   CONST
     D   PRM_Alias                   10A   CONST
     D
     D*==> Campos de Trabalho
     D WNuSequFound    S              5S 0
     D WCdClausula     S              3A
     D WIcSelected     S               N
     D WIcExiste       S               N
     D WIcDeleted      S               N
     D*=======================================================
      /FREE
       //------------------------------------------------------
       // Validar se o Código está Seleccionado
       //------------------------------------------------------
       WIcSelected = *OFF;
       IF (PRM_Clausula = 'S');
          WIcSelected = *ON;
       ENDIF;

       //------------------------------------------------------
       // Localizar o Código da Cobertura
       //------------------------------------------------------
       CHAIN (PRM_CdProd:PRM_Alias) RCATP10802;
       IF (%FOUND());
          WCdClausula= CAT_CDITEM;
          WIcExiste  = *OFF;
          WIcDeleted = *OFF;
          SETLL (PRM_CdRamo:PRM_NuApol:CAT_CDITEM) RPRDP00901;
          READE (PRM_CdRamo:PRM_NuApol:CAT_CDITEM) RPRDP00901;
          DOW (NOT %EOF(FPRDP00901));
             IF (PRD_PRD00901 <= WDatSelect) AND (PRD_PRD00902 >= WDatSelect);
                WNuSequFound = PRD_NUSEQU;
                WIcExiste    = *ON;
                IF (PRD_PRD00902 = %DATE('2999-12-31':*ISO));
                   WIcDeleted = *ON;
                ENDIF;
                LEAVE;
             ENDIF;
             READE (PRM_CdRamo:PRM_NuApol:CAT_CDITEM) RPRDP00901;
          ENDDO;

          IF (NOT WIcExiste AND WIcSelected);
             PRD_CDRAMO   = OR_RAMO;
             PRD_NUAPOL   = OR_APOL;
             PRD_CDITEM   = WCdClausula;
             PRD_NUSEQU   = *ZEROS;
             PRD_PRD00901 = WDatSelect;
             WRITE RPRDP009;
          ELSEIF (WIcExiste AND NOT WIcSelected AND NOT WIcDeleted);
             DELETE (OR_RAMO:OR_APOL:WNuSequFound) RPRDP009;
          ENDIF;
       ENDIF;
       RETURN;

      /END-FREE
     P UpdClausula     E
