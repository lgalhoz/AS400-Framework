     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=================================================================================
     H* Programa..: RPRDO024
     H* Descrição.: Disponiblizar Cartas Verdes
     H*
     H* Observação:
     H*   Para executar este programa num Browser Web fazer:
     H*   1.Login    => http://joComp/DemoComp?frame=login&user=<user>&passw=<passw>
     H*   2.Executar => http://joComp/DemoComp?frame=cisa&ramo=&apolice=&cisa=
     H*
     H*   Para executar este programa no AS400 fazer:
     H*   1.Executar => CALL RPRDO024 PARM(' ' ' ')
     H*=================================================================================
     FGBASEGRA  IF   E           K DISK    PREFIX(AU_)
     FGBASEGRO  IF   E           K DISK    PREFIX(AU_)
     FGFSEG     IF   E           K DISK    PREFIX(EN_)
     F*=================================================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO014
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002

     D*==> Campos de Trabalho
     D   WURI          S          65535A
     D   HTTPCODE      S          65535A
     D
     D*==> formato de registo do GBASEGRO
     D   R#GRO       E DS                  EXTNAME('GBASEGRO') PREFIX(OR_)
     D     OR_RECORD           1    700
     D
     D*==> Restantes campos de Trabalho
     D RetTexto        PR                  EXTPGM('RPRDO008')
     D   WGexap                        *   CONST
     D   WGBaseDio                     *   CONST
     D   #RetTexto                         CONST LIKE(#RETTEXTO) DIM(1000)
     D   CdGrupo                      2A   CONST
     D   Data                         8S 0 CONST
     D
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D
     D WPtrHistorico   S               *
     D WPtrDiario      S               *
     D WNatureza       S              2A
     D WDatSelect      S              8S 0
     D
     D WTexto          S            110A
     D WRegua          S            110A
     D WCdRamo         S              2S 0
     D WNuApolice      S              7S 0
     D WMaxEspacos     S              2S 0
     D
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
       //-----------------------------------------------
       // Validar se pode utilizar a função
       //-----------------------------------------------
       $inzHTTPSrc('XML');  // Possivel(XML, HTTP, TXT)
       IF NOT $ValAccess(S#PGM);
          HTTPCode = $getHTTPmsg('HTP0500');
          RETURN;
       ENDIF;

       MONITOR;
          WCdRamo    = %DEC($getHTTPVar('ramo'):2:0);
          WNuApolice = %DEC($getHTTPVar('apolice'):7:0);
       ON-ERROR;
          WCdRamo    = 5;
          WNuApolice = 9910401;
       ENDMON;
       WRegua = '|....+....1....+....2....+....3....+....4....+....5+
                 ....+....6....+....7....+....8....+....9....+...10+
                 ....+...11';

       EXSR getRecords;
       HTTPCODE = $getHTTPSrc();
       RETURN;

       //==============================================================
       // Subrotina.: getRecords
       // Objectivo.: Obter os ergistos a apresentar ao utilizador
       //==============================================================
       BEGSR getRecords;
          CHAIN (WCdRamo:WNuApolice) GBASEGRO;
          IF (NOT %FOUND(GBASEGRO));
             HTTPCode = $getHTTPmsg('HTP0500');
             RETURN;
          ENDIF;

          WPtrDiario = %ADDR(OR_RECORD);
          WNatureza  = '00';
          WdatSelect = 20140516;
          RetTexto(WPtrHistorico:WPtrDiario:#RetTexto:WNatureza:WDatSelect);

          WPosIni     = 1;
          WMaxEspacos = *ZEROS;
          $addHTTPVal('':WRegua);
          DOW (WPosIni < 1000);
             WTexto = #RetTexto(WPosIni).Texto;
             IF (WTexto = *BLANKS);
                WMaxEspacos += 1;
                IF (WMaxEspacos = 1);
                   $addHTTPVal('I':WRegua);
                ELSE;
                   %SUBST(WTexto:94:1) = '|';
                   $addHTTPVal('I':WTexto);
                ENDIF;
             ELSE;
                WMaxEspacos = *ZEROS;
                WTexto = '|'+WTexto;
                IF (%SUBST(WTexto:94:1) = *BLANKS);
                   %SUBST(WTexto:94:1) = '|';
                ENDIF;
                $addHTTPVal('I':WTexto);
             ENDIF;
             IF (WMaxEspacos = 10);
                LEAVE;
             ENDIF;
             WPosIni += 1;
          ENDDO;
       ENDSR;
      /END-FREE
