     H DFTACTGRP(*NO) BNDDIR('JMDIR') ACTGRP(*NEW)
     H*=================================================================================
     H* Programa..: RSINO005
     H* Descrição.: Disponiblizar Informação para Etiquetas de Sinistros automóvel e
     H*             Outros Ramos.
     H*
     H* Observação:
     H*   Para executar este programa num Browser Web fazer:
     H*   1.Login    => http://jomata/josemata?frame=login&user=<user>&passw=<passw>
     H*   2.Executar => http://jomata/josemata?frame=sinlabel&nuproc=6960&ano=12
     H*
     H*   Para executar este programa no AS400 fazer:
     H*   1.Executar => CALL RSINO005 PARM(' ' ' ')
     H*=================================================================================
     FGBASEGRA  IF   E           K DISK    PREFIX(AUT_)
     FASINPRO   IF   E           K DISK    PREFIX(SIN_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     F*=================================================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010

     D*==> Campos de Trabalho
     D   WURI          S          65535A
     D   HTTPCODE      S          65535A
     D
     D*==> Definição de Campos de Trabalho
     D WHTTPCODE       S          65535A
     D WTexto          S            120A
     D
     D WNumProcess     S              7S 0
     D WAnoProcess     S              2S 0
     D WRamo           S              2S 0
     D WApolice        S              7S 0
     D IsAS400Mode     S               N
     D
     D*==> Definição de constantes
     D TIP#Process     C                   'P'
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
          WNumProcess = %DEC($getHTTPVar('nuproc'):5:0);
          WAnoProcess = %DEC($getHTTPVar('ano'):2:0);
          IsAS400Mode = *OFF;
       ON-ERROR;
          WNumProcess = 6960;
          WAnoProcess = 12;
          IsAS400Mode = *ON;
       ENDMON;

       EXSR getRecords;
       IF (NOT IsAS400Mode);
          HTTPCODE = $getHTTPSrc();
       ELSE;
          WHTTPCODE = $getHTTPSrc();
       ENDIF;
       RETURN;

       //==============================================================
       // Subrotina.: getRecords
       // Objectivo.: Obter os registos a apresentar ao utilizador
       //==============================================================
       BEGSR getRecords;
          CHAIN (TIP#Process:WAnoProcess:WNumProcess) ASINPRO;
          IF (NOT %FOUND(ASINPRO));
             HTTPCode = $getHTTPmsg('HTP0500');
             RETURN;
          ENDIF;

          CHAIN (SIN_RM:SIN_AP) GBASEGRA;
          IF (NOT %FOUND(GBASEGRA));
             HTTPCode = $getHTTPmsg('HTP0500');
             RETURN;
          ENDIF;

          CHAIN (AUT_NSEG) GFSEG;
          IF (NOT %FOUND(GFSEG));
             HTTPCode = $getHTTPmsg('HTP0500');
             RETURN;
          ENDIF;

          $addHTTPVal('label');
          $addHTTPVal('processo');
          $addHTTPVal('processo':%EDITW(SIN_AAPROC:'0  ')+'9'+
                                 %CHAR(SIN_NSEQ));
          $addHTTPVal('sequencia':%CHAR(SIN_NSEQ));
          $addHTTPVal('ano':%CHAR(SIN_AAPROC));
          $addHTTPVal('abertura':
                      %EDITW($CvtData(SIN_DTINIC:'YMD':'DMYY'):'0  -  -    '));
          $addHTTPVal('sinitro':
                      %EDITW($CvtData(SIN_DTSIN:'YMD':'DMYY'):'0  -  -    '));
          $addHTTPVal('franquia':%CHAR(SIN_FRQ2));
          $addHTTPVal();

          $addHTTPVal('apolice');
          $addHTTPVal('ramo':%CHAR(AUT_RAMO));
          $addHTTPVal('apolice':%CHAR(AUT_APOL));
          $addHTTPVal('vencimento':%CHAR(AUT_DTINI)); // Calcular vencimento YYYY/MM
          $addHTTPVal('matricula':AUT_MATR1);
          $addHTTPVal('rc':%CHAR(AUT_CAPRC));         // Capital de RC (Obter de Sinistros)
          $addHTTPVal('dp':%CHAR(AUT_CAPDPI));        // Capital de DP (Obter de Sinistros)
          $addHTTPVal('codocupa':%CHAR(AUT_CR3));     // Código de Ocupantes (Obter de Sinistros)
          $addHTTPVal('ocupantes':%CHAR(AUT_CAPOCU)); // Capital de ocupantes (Obter de Sinistros)

          $addHTTPVal('mediador':'mediador');  // Obter valor para o mediador
          $addHTTPVal('nsinistr':'sinistro');  // Obter o número de sinistro (Incluir IDS)
          $addHTTPVal();

          $addHTTPVal('segurado');
          $addHTTPVal('codigo':%CHAR(SEG_NSEG));
          $addHTTPVal('nome':SEG_NOM);
          $addHTTPVal();

          $addHTTPVal();
       ENDSR;
      /END-FREE
