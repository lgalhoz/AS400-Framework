     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO108
     H* Descrição.: Devolve Credor Hipotecário - Condominio
     H*==================================================================
     FFPRDP017  IF   E           K DISK    PREFIX(PRD_)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SENTO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Campos de Trabalho
     D WIsHeader       S               N
     D WCodProduto     S              5S 0
     D WDatInicio      S              8S 0
     D WPtrOR          S               *
     D WModo           S               N
     D WResultado      S            100A
     D WNomCredor      S            100A
     D WBullet         S              3A
     D WCodErro        S              7A
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrOR
     C                   PARM                    WModo
     C                   PARM                    WResultado
     C                   PARM                    WBullet
     C                   PARM                    WCodErro
      /FREE
       ptrOr      = WptrOR;
       WResultado = '<NOP>'; // Não escrever
       WIsHEader  = *OFF;
       EXSR WriteCredor;

       RETURN;
       //==============================================================
       // Subrotina.: WriteCredor
       // Objectivo.: Obter os Credores Hipotecários da Apólice
       //==============================================================
       BEGSR WriteCredor;
           SETLL (OR_RAMO:OR_APOL) RPRDP017;
           READE (OR_RAMO:OR_APOL) RPRDP017;
           DOW (NOT %EOF(FPRDP017));
              IF (PRD_CDCREDOR > *ZEROS);
                 EXSR WriteFirst;
                 WNomCredor = ATR#Blink+'<Falta definir o Credor>';
                 WCodErro   = 'PRD0204';
                 MONITOR;
                    IF (OR_CRHIP > *ZEROS);
                       WNomCredor = $getDescCred(PRD_CDCREDOR);
                       WCodErro   = *BLANKS;
                    ENDIF;
                 ON-ERROR;
                 ENDMON;
                 $AddLineType('B7');
                 $AddTextLine('Fracção '+%TRIM(PRD_CDFRAC)+' ('+
                                          %TRIM(PRD_DSFRAC)+') com o Credor "' +
                                          %TRIM(WNomCredor)+'".');
              ENDIF;
              READE (OR_RAMO:OR_APOL) RPRDP017;
           ENDDO;
       ENDSR;

       //==============================================================
       // Subrotina.: WriteFirst
       // Objectivo.: Escrever o Primeiro Registo
       //==============================================================
       BEGSR WriteFirst;
          IF (NOT WIsHeader);
             $AddLineType('B6');
             $AddTextLine('Credor Hipotecário. As Fracções com +
                           Credor Hipotecário:':1:WBullet);
             WIsHEader  = *ON;
          ENDIF;
       ENDSR;
      /END-FREE
