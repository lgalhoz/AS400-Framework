     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=====================================================================
     H* Programa...: PAUDO009
     H* Objectivo..: Converter Extracto de Movimentos da Lusitânia.
     H* Observação.:
     H*   CRTPF FILE(QTEMP/FAUDP020) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*=====================================================================
     FCSVCONT   IP   E           K DISK    PREFIX(INT_) RENAME(CSVCONT:CSV)
     FFAUDP020  UF A E           K DISK    PREFIX(CTL_)
     F*=====================================================================
     D ExtractText     PR           100A
     D   Palavra                    200A   CONST
     D   Posicao                      3S 0 CONST
     D
     D*==> Campos de Trabalho
     D WAgente         S              5A
     D WData           S              8A
     D WReferenc       S             50A
     D WCodigo         S              4A
     D WDsCritivo      S             50A
     D WDebito         S             11A
     D WCredito        S             11A
     D WTomador        S             40A
     D WNumrec         S              9S 0
     D WCdRamo         S              2S 0
     D WNuApol         S              7S 0
     D WIcConv         S              1A
     D*=====================================================================
      /FREE
         IF (%SUBST(INT_CSVCONT:1:7) <> 'AGENTE;');
            CTL_Agente    = ExtractText(INT_CSVCONT:1);
            CTL_Data      = ExtractText(INT_CSVCONT:2);
            CTL_Referenc  = ExtractText(INT_CSVCONT:3);
            CTL_Codigo    = ExtractText(INT_CSVCONT:4);
            CTL_DsCritivo = ExtractText(INT_CSVCONT:5);
            CTL_Debito    = ExtractText(INT_CSVCONT:6);
            CTL_Credito   = ExtractText(INT_CSVCONT:7);
            CTL_Tomador   = ExtractText(INT_CSVCONT:8);
            CTL_Numrec    = *ZEROS;
            CTL_CdRamo    = *ZEROS;
            CTL_NuApol    = *ZEROS;
            CTL_IcConv    = 'N';
            WRITE RAUDP020;
         ENDIF;
      /END-FREE

     P*==================================================================
     P* Processo..: ExtractText
     P* Descrição.: Extrair um segmento de Texto delimitado
     P*==================================================================
     P ExtractText     B                   EXPORT
     D ExtractText     PI           100A
     D   Palavra                    200A   CONST
     D   Posicao                      3S 0 CONST
     D
     D*==> Campos de Trabalho
     D WPosAct         S              3S 0
     D WPosIni         S              3S 0
     D WPosFim         S              3S 0
     D WQtde           S              3S 0
     D WFldActual      S              3S 0
     D WResultado      S            100A
     D*==================================================================
      /FREE
        //--------------------------------------------
        // Inicializar campos de Trabalho
        //--------------------------------------------
        WResultado = *BLANKS;
        WPosIni    = 1;
        WFldActual = *ZEROS;

        //--------------------------------------------
        // Obter o segmento de texto pedido
        //--------------------------------------------
        DOW (WFldActual < Posicao);
           WPosFim    = %SCAN(';':Palavra:WPosIni);
           IF (WPosFim = *ZEROS);
              WPosFim = 200;
              WResultado = %SUBST(Palavra:WPosIni:(WPosFim - WPosIni));
              LEAVE;
           ENDIF;
           WResultado = %SUBST(Palavra:WPosIni:(WPosFim - WPosIni));
           WPosIni    = WPosFim + 1;
           WFldActual += 1;
        ENDDO;

        RETURN WResultado;
      /END-FREE
     P ExtractText     E
