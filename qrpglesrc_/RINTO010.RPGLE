     H*====================================================================
     H* Subrotina..: PINTP010
     H* Objectivo..: Obter os dados de uma Apólice
     H*====================================================================
     FGBASEGRA  IF   E           K DISK    PREFIX(O_)
     FGBASEGRO  IF   E           K DISK    PREFIX(A_)
     FGBASEGRT  IF   E           K DISK    PREFIX(T_)
     F*====================================================================
     D WCodRamo        S              2S 0
     D WNumApolice     S              7S 0
     D WRetParms       S           1000A
     D*====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodRamo
     C                   PARM                    WNumApolice
     C                   PARM                    WRetParms
      /FREE
         *INLR = *ON;
         RETURN;
      /END-FREE
