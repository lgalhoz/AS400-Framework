     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*====================================================================
     H* Subrotina..: PINTP012
     H* Objectivo..: Obter os Mediadores das Cartas Credores Hipotecarios
     H*====================================================================
     F*==================================================================
     FGC895     IF   E           K DISK
     D*==========================================================
     D*==> Lista de Textos a escrever
     D #LinhasTxt      DS                  QUALIFIED DIM(20)
     D   CodTexto                     5S 0
     D   Fonte                       20A
     D   IcBold                        N
     D   FontSize                     2S 0
     D   Texto                       80A
     D   IsEMpty                       N
     D pLinhasTxt      S               *   INZ(%ADDR(#LinhasTxt))
     D WIdx            S              5S 0
     D WMaxEntries     S              5S 0 INZ(20)
     D
     D*==> Parametros recebidos
     D WCdProd         S              5S 0
     D WRamo           S              2S 0
     D WApolice        S              7S 0
     D WDatInicio      S              8S 0
     D WpLinhasTxt     S               *
     D
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WpLinhasTxt
      /FREE

         EXSR InzRecords;
         #LinhasTxt(1).Texto   = 'Linha de Texto 01';
         #LinhasTxt(1).IsEMpty = *OFF;
         #LinhasTxt(2).Texto   = 'Linha de Texto 02';
         #LinhasTxt(2).IsEMpty = *OFF;
         #LinhasTxt(3).Texto   = 'Linha de Texto 03';
         #LinhasTxt(3).IsEMpty = *OFF;
         WpLinhasTxt = %ADDR(#LinhasTxt);
         *INLR = *ON;
         RETURN;

       //==================================================================
       // Subrotina..: InzRecords
       // Objectivo..: Limpar todos os registos da lista
       //==================================================================
       BEGSR InzRecords;
          WIdx = *ZEROS;
          DOW (WIdx < WMaxEntries);
             WIdx += 1;
             #LinhasTxt(WIdx).Texto = *BLANKS;
             #LinhasTxt(WIdx).IsEMpty = *ON;
          ENDDO;
       ENDSR;
      /END-FREE
