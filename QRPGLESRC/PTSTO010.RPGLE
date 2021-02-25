     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PTSTO010
     H* Descrição.: Actualizar lista de ficheiros temporários
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Variáveis de Trabalho
     D WProduto        S              1A
     D WCarrega        S              1A
     D WMes            S              2S 0
     D WAno            S              4S 0
     D WCdGrupo        S             10A
     D
     D WFilName        S             10A
     D WTemplate       S             40A
     D*==========================================================
      /FREE

        WTemplate = 'GUIM[1|2][MM][C]';
        WFilName  = 'GUIM103A';
        IF ($ValStrStruct(WFilName:WTemplate));
          DSPLY 'Nome Válido';
        ELSE;
          DSPLY 'Nome Inválido';
        ENDIF;
        WFilName  = 'GUIM204B';
        IF ($ValStrStruct(WFilName:WTemplate));
          DSPLY 'Nome Válido';
        ELSE;
          DSPLY 'Nome Inválido';
        ENDIF;

        WFilName  = 'GUIM2041';
        WTemplate = 'GUIM[1|2][MM][C]';
        WProduto  =      $ExtrStringVar(WFilName:WTemplate:'P');
        WCarrega  =      $ExtrStringVar(WFilName:WTemplate:'C');
        WMes      = %DEC($ExtrStringVar(WFilName:WTemplate:'M'):2:0);
        WAno      = %DEC($ExtrStringVar(WFilName:WTemplate:'A'):4:0);
        WCdGrupo  =      $ExtrStringVar(WFilName:WTemplate:'G');

        *INLR = *ON;
      /END-FREE
