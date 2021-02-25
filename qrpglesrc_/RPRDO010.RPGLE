     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RRDO010
     H* Descrição.: Informação Standard de Multiriscos Habitação
     H*
     H*==================================================================
     D/Copy QRPGLESRC,SGRLO010
     D/Copy QRPGLESRC,SPRDO002

     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D WLinha          S            500A
     D WptrHist        S               *
     D WptrOR          S               *
     D WCdProd         S              5S 0
     D WLRPS           S                   LIKE(HST_LRPS)
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrHist
     C                   PARM                    WPtrOr
      /FREE

       $AddLineType('T1');
       ptrHist = WptrHist;
       ptrOr     = WptrOR;
       MONITOR;
          WLRPS = HST_LRPS;
       ON-ERROR;
          WLRPS = OR_LRPS;
       ENDMON;

       $AddTextLine('LOCAL DE RISCO: ' + WLRPS);

       *INLR = *ON;
       RETURN;
      /END-FREE
