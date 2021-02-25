     H ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Processo..:
     H* Descrição.: Validar os dias úteis, em função dos Feriados fixos
     H*             móveis e Municipais
     H*==================================================================
     FFGRLP030  IF   E           K DISK
     D
     D*==> Validar dia util, devolve *OFF se for Feriado ou fim de Semana
     D $chkDUtil       PR              N
     D   Data                         8S 0 Const
     D   Local                        5S 0 Const Options(*NOPASS)
     D
     D WResultado      S               N
      /FREE
          WResultado = $chkDUtil(20130311);
         *INLR = *ON;
      /END-FREE
     P*===================================================================
     P* Processo..: $chkDUtil
     P* Descrição.: Obter o dia util (considerando Feriado Fixos e Móveis)
     P*             devolve *ON se dia util ou *OFF se Feriado ou fim de
     P*             semana.
     P*===================================================================
     P $chkDUtil       B                   EXPORT
     D                 PI              N
     D    Data                        8S 0 Const
     D    Local                       5S 0 Const Options(*NOPASS)
     D
     D WData           S               D
     D WVarA           S              7S 0
     D WVarB           S              7S 0
     D WVarC           S              7S 0
     D WVarD           S              7S 0
     D WVarE           S              7S 0
     D WVarF           S              7S 0
     D WVarG           S              7S 0
     D WVarH           S              7S 0
     D WVarI           S              7S 0
     D WVarJ           S              7S 0
     D WVarK           S              7S 0
     D WVarL           S              7S 0
     D WVarM           S              7S 0
     D WAno            S              4S 0
     D WMes            S              2S 0
     D WDia            S              2S 0
     D
     D WDatPascoa      S               D
     D WDatSexta       S               D
     D WCarnaval       S               D
     D
     D WResultado      S               N   INZ(*OFF)
     D
     D*==> Constantes
     D DIF#Carnaval    C                   -47
     D DIF#Sexta       C                   -2
     D DIF#CorpDeus    C                   +60
     C*==============================================================
      /FREE
        WData = %DATE(Data:*ISO);
        WAno = %SUBDT(WData:*YEARS);
        EXSR CalcMoveis;

        RETURN WResultado;

        //========================================================================
        // Subrotina..: CalcPascoa
        // Objectivo..: Calcular o Feriado da Pascoa  em função do ano
        //========================================================================
        BEGSR CalcPacoa;
           WVarA = %REM(WAno:19);
           WVarB = (Wano / 100);
           WVarC = %REM(Wano:100);
           WVarD = WVarB / 4;
           WVarE = %REM(WVarB:4);
           WVarF = ((WVarb + 8) / 25);
           WVarG = ((WVarB - WVarF + 1) / 3);
           WVarH = %REM((19 * WVarA +WVarB - WVarD - WVarG + 15):30);
           WVarI = (WVarC / 4);
           WVarK = %REM(WVarc:4);
           WVarL = %REM((32 + 2*WVarE + 2*WVarI - WVarH - WVarK):7);
           WVarM = ((WVarA + 11*WVarH + 22*WVarL) / 451);
           WMes  = ((WVarH + WVarL - 7*WVarM + 114)/31);
           WDia  = %REM((WVarH + WVarL - 7*WVarM +114):31)  + 1;
        ENDSR;
      /END-FREE
     P $chkDUtil       E
