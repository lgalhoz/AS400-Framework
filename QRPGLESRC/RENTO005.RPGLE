     H*==========================================================
     H* Programa...: RENTO005
     H* Descrição..: ForCompr restante parte do nome com
     H*              asteriscos
     H*==========================================================
     D
     D*==> Campos de Trabalho
     D PNome           S             40A
     D WNome           S             40A
     D*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PNome
      /FREE
        WNome   = '****************************************';
        MONITOR;
          WNome = %TRIM(PNome) + %TRIM(WNome);
          PNome = WNome;
        ON-ERROR;
          WNome = *BLANKS;
        ENDMON;
        RETURN;
      /END-FREE
