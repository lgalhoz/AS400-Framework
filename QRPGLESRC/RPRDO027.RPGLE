     H DFTACTGRP(*NO) BNDDIR('JMDIR') ACTGRP(*NEW)
     H*=================================================================================
     H* Programa..: RPRDO027
     H* Descrição.: Disponiblizar em XML
     H*
     H* Observação:
     H*   Para executar este programa num Browser Web fazer:
     H*   1.Login    => http://jomata/josemata?frame=login&user=<user>&passw=<passw>
     H*   2.Executar => http://jomata/josemata?frame=actaxml&ramo=5&apolice=9910401&grupo=00
     H*
     H*   Para executar este programa no AS400 fazer:
     H*   1.Executar => CALL RPRDO027 PARM(' ' ' ')
     H*=================================================================================
     FGBASEGRO  IF   E           K DISK    PREFIX(BAS_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
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
     D   R#GRO       E DS                  EXTNAME('GBASEGRO') PREFIX(BAS_)
     D     BAS_RECORD          1    700
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
     D WIdx            S              3S 0
     D
     D WPtrHistorico   S               *
     D WPtrDiario      S               *
     D WNatureza       S              2A
     D WDatSelect      S              8S 0
     D
     D WHTTPCODE       S          65535A
     D WTexto          S            120A
     D WCdRamo         S              2S 0
     D WNuApolice      S              7S 0
     D WMaxEspacos     S              2S 0
     D IsAS400Mode     S               N
     D
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
       //-----------------------------------------------
       // Validar se pode utilizar a função
       //-----------------------------------------------
       IsAS400Mode = *OFF;
       $inzHTTPSrc('XML');  // Possivel(XML, HTTP, TXT)
       IF NOT $ValAccess(S#PGM);
          HTTPCode = $getHTTPmsg('HTP0500');
          RETURN;
       ENDIF;

       MONITOR;
          WCdRamo    = %DEC($getHTTPVar('ramo'):2:0);
          WNuApolice = %DEC($getHTTPVar('apolice'):7:0);
          WNatureza  = $getHTTPVar('grupo');
       ON-ERROR;
          WCdRamo    = 5;
          WNuApolice = 9910527;
          WNatureza  = '00';
          IsAS400Mode = *ON;
       ENDMON;

       CHAIN (WCdRamo:WNuApolice) GBASEGRO;
       IF (NOT %FOUND(GBASEGRO));
          HTTPCode = $getHTTPmsg('HTP0500');
          RETURN;
       ENDIF;

       EXSR getSegurado;

       IF (WNatureza <> '99');
          EXSR getRecords;
       ENDIF;

       IF (NOT IsAS400Mode);
          HTTPCODE = $getHTTPSrc();
       ELSE;
          WHTTPCODE = $getHTTPSrc();
       ENDIF;
       RETURN;

       //==============================================================
       // Subrotina.: getSegurado
       // Objectivo.: Obter os registos a apresentar ao utilizador
       //==============================================================
       BEGSR getSegurado;
          $addHTTPVal('HEAD');
          CHAIN (BAS_NSEG) GFSEG;
          IF (%FOUND(GFSEG));
              $addHTTPVal('ramo':%CHAR(BAS_RAMO));
              $addHTTPVal('apol':%CHAR(BAS_APOL));
              $addHTTPVal('nsegur':%CHAR(BAS_NSEG));
              $addHTTPVal('nome':SEG_NOM);
              $addHTTPVal('morada':SEG_MORAD);
              $addHTTPVal('cpost':%CHAR(SEG_CPOST));
              $addHTTPVal('dpost':SEG_DESCP);
              $addHTTPVal('jmm1':%CHAR(BAS_JMM1));
              $addHTTPVal('jmm2':%CHAR(BAS_JMM2));
          ENDIF;
          $addHTTPVal();
       ENDSR;

       //==============================================================
       // Subrotina.: getRecords
       // Objectivo.: Obter os registos a apresentar ao utilizador
       //==============================================================
       BEGSR getRecords;
          WPtrDiario = %ADDR(BAS_RECORD);
          WDatSelect = $DateToInt(%DATE());
          RetTexto(WPtrHistorico:WPtrDiario:#RetTexto:WNatureza:WDatSelect);

          $addHTTPVal('DATA');
          WPosIni     = 1;
          WMaxEspacos = *ZEROS;
          DOW (WPosIni < 2000) AND (NOT #RetTexto(WPosIni).IsEmpty);
             WTexto = #RetTexto(WPosIni).Texto;
             WIdx = 1;
             DOW (WIdx < %LEN(WTexto));
                IF (%SUBST(WTexto:WIdx:1) = ' ');
                   %SUBST(WTexto:WIdx:1) = '^';
                ENDIF;
                WIdx += 1;
             ENDDO;

             $addHTTPVal('I':WTexto);
             WPosIni += 1;
          ENDDO;
          $addHTTPVal();
       ENDSR;
      /END-FREE
