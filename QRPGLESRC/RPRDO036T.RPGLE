     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*===================================================================
     H* Biblioteca.: RPRDO036
     H* Objectivo..: Ddevolver o Capital da viatura à data do Sinistro
     H*===================================================================
     D
     D CalcValor       PR                  EXTPGM('RPRDO036')
     D  Categoria                     1A   CONST
     D  DatNovo                       8S 0 CONST
     D  DatActual                     8S 0 CONST
     D  ValNovo                      13S 2 CONST
     D  ValActual                    13S 2 CONST
     D  CodErro                       7A   CONST

     D*==> Campos de Trabalho
     D PCategoria      S              1A
     D PDatNovo        S              8S 0
     D PValNovo        S             13S 2
     D WCodErro        S              7A
     D PDatActual      S              8S 0
     D PValActual      S             13S 2
     D*===================================================================
      /FREE
       //-----------------------------------------------------
       // ForCompr campos de trabalho
       //-----------------------------------------------------
       PCategoria = '1';
       PDatNovo   = 20150601;
       WCodErro   = *BLANKS;
       PDatActual = 20160501;
       PValNovo   = 6000;

       //-----------------------------------------------------
       // Devolver o valor à data
       //-----------------------------------------------------
       PValActual = *ZEROS;
       CalcValor(PCategoria:PDatNovo:PDatActual:PValNovo:PValActual:WCodErro);
       DSPLY  %CHAR(PValActual);


       //-----------------------------------------------------
       // Devolver o valor em novo
       //-----------------------------------------------------
       PValNovo   = *ZEROS;
       CalcValor(PCategoria:PDatNovo:PDatActual:PValNovo:PValActual:WCodErro);
       DSPLY  %CHAR(PValNovo);

       *INLR = *ON;
      /END-FREE
