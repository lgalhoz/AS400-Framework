     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*====================================================================
     H* Programa..: RSINO008
     H* Autor.....: Luís Galhoz
     H* Data......: 2015-06-26
     H* Descrição.: Disponibilizar a informação no formato XML.
     H*             A mais valia do formato XML, deve-se ao facto do output
     H*             poder ser tratado à posteriori em diferentes formatos, por
     H*             exemplo em PDF ou em XML.
     H*
     H* Execução..: http://20.0.0.1/josemata?frame=login&user=apache
     H*             http://20.0.0.1/josemata?frame=sinistros&mediador=900&inicio=20150101&termo=20150630
     H*             http://20.0.0.1/josemata?frame=sinistros&segurado=84142&inicio=20150101&termo=20150630
     H*             http://20.0.0.1/josemata?frame=sinistros&grupo=84&inicio=20150101&termo=20150630
     H*
     H* Compilar..: CRTPF FILE(QTEMP/FSINT008) SRCFILE(#JMPRDSRC/QDDSSRC)
     H*====================================================================
     F*FSINT008  IF   E           K DISK    PREFIX(S_) USROPN
     F*                                     EXTFILE('QTEMP/FSINT008')

      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010

     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Campos de Trabalho
     D   WURI          S          65535A
     D   HTTPCODE      S          65535A
     D
     D*==> Parametros a receber
     D WComando        S            300A   INZ(*BLANKS)
     D WMediador       S              3A
     D WSegurado       S              8A
     D WGrupo          S              8A
     D WDatInicio      S              8A
     D WDatTermo       S              8A
     D
     D*==> Campos de Trabalho
     D  WCriteria      S            100A
     D  WOrderBy       S             50A
     D  WFields        S            200A
     D  WSqlStmt       S            400A
     D  WSqlSess       S              5S 0
     D  WQtdRegis      S              9S 0
     D  WRemoteAddr    S             15A
     D  WCodRamo       S              2A
     D  WNumApol       S              7A
     D  WNumProcess    S              9A
     D  WAnoProces     S              4A
     D  WRecord        S              5S 0
     D  WIdx           S              3S 0
     D  WTipReport     S              1A
     D
     D*==> Constantes
     D XML#AllMedia    C                   'A'
     D XML#Mediador    C                   'M'
     D XML#Segurado    C                   'S'
     D XML#Grupo       C                   'G'
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
        MONITOR;
           WRemoteAddr = %str($getenv('REMOTE_ADDR'));
        ON-ERROR;
           WRemoteAddr  = '20.0.0.53';
        ENDMON;

        //-----------------------------------------------
        // Validar se pode utilizar a função
        //-----------------------------------------------
        $inzHTTPSrc('XML');  // Possivel(XML, HTTP, TXT)
        IF NOT $ValAccess(S#PGM);
           HTTPCode = $getHTTPmsg('HTP0500');
           RETURN;
        ENDIF;

        //-----------------------------------------------------
        // Inicializar campos de Trabalho
        //-----------------------------------------------------
        $addHTTPVal('IP':WRemoteAddr);
        $addHTTPVal('user':S#USR);
        $addHTTPVal('stscod':'200');

        //-----------------------------------------------
        // Validar os Parametros recebidos
        //-----------------------------------------------
        MONITOR;
           WMediador  = $getHTTPVar('mediador');
           WSegurado  = $getHTTPVar('segurado');
           WGrupo     = $getHTTPVar('grupo');
           WDatInicio = $getHTTPVar('inicio');
           WDatTermo  = $getHTTPVar('termo');
        ON-ERROR;
           WMediador  = '900';
           WSegurado  = *BLANKS;
           WGrupo     = *BLANKS;
           WDatInicio = '20150101';
           WDatTermo  = '20150630';
        ENDMON;

        //-----------------------------------------------------
        // Carregar ficheiros de Processos com dados
        //-----------------------------------------------------
        EXSR FillProcess;

        //-----------------------------------------------------
        // Obter os registos de acordo com o critério
        //-----------------------------------------------------
        EXSR FillRecords;
        IF (WQtdRegis = *ZEROS);
        //   HTTPCode = $getHTTPmsg('HTP0207');
        //   RETURN;
        ENDIF;

        HTTPCODE = $getHTTPSrc();
        RETURN;

        //=====================================================
        // Subrotina.: FillRecords
        // Descrição.:
        //=====================================================
        BEGSR FillRecords;
           WQtdRegis = *ZEROS;

           //-------------------------------------------------
           // Preparar as datas de pesquisa
           //-------------------------------------------------
           IF (WDatInicio = *BLANKS);
              LEAVESR;
           ENDIF;
           WDatInicio = %SUBST(WDatInicio:3:6);
           IF (WDatTermo = *BLANKS);
              WDatTermo = %CHAR(%DATE():*ISO);
           ENDIF;
           WDatTermo = %SUBST(WDatTermo:3:6);

           //-------------------------------------------------
           // Caracterizar tipo de Reporting XML a devolver
           //-------------------------------------------------
           IF (WGrupo <> *BLANKS);
              WTipReport = XML#Grupo;
              WCriteria = *BLANKS;
              WOrderBy = 'NSEG, RM, AP';
           ELSEIF (WSegurado <> *BLANKS);
              WTipReport = XML#Segurado;
              WCriteria = 'NSEG = ' + WSegurado;
              WOrderBy = 'NSEG, RM, AP';
           ELSEIF (WMediador <> *BLANKS);
              WTipReport = XML#Mediador;
              WCriteria = 'NMED = ' + WMediador;
              WOrderBy = 'NMED, RM, AP';
           ELSE;
              WTipReport = XML#AllMedia;
              WCriteria = *BLANKS;
              WOrderBy = 'NMED, RM, AP';
           ENDIF;
           EXSR ReadFiles;
        ENDSR;

        //=====================================================
        // Subrotina.: ReadFiles
        // Descrição.: Ler ficheiros de dados
        //=====================================================
        BEGSR ReadFiles;
           //----------------------------------------------------
           // Construção dinamica da Query aos Processos
           //----------------------------------------------------
           WFields = 'AAPROC,NSEQ,SPROC,DTINIC,DTRINI,DTFIM,CAUSA,+
                      DTSIN,DTPART,NSEG,RM,AP,ACORDO,NMED,TMED,+
                      MATR1';

           FOR WIdx=1 TO 3;
              $addHTTPVal('apolice');
              WSqlSess = $SetFiles('QTEMP/FSINT008');
              $SetFields(WFields);
              WSqlStmt = ' IPROC = ''P'' AND ' +
                         ' DTINIC >= ' + %TRIM(WDatInicio) + ' AND ' +
                         ' DTINIC <= ' + %TRIM(WDatTermo);
              IF (WCriteria <> *BLANKS);
                 WSqlStmt = %TRIM(WSqlStmt) + ' AND ' +
                            %TRIM(WCriteria);
              ENDIF;
              $SetCriteria(WSqlStmt);
              IF (WOrderBy <> *BLANKS);
                 $SetSort(WOrderBy);
              ENDIF;

              WRecord = 1;
              DOW ($GetRecord(WRecord) > *ZEROS);
                 $addHTTPVal('Processo');
                 $addHTTPVal('ano':$GetField('AAPROC'));
                 $addHTTPVal('sequencia':$GetField('NSEQ'));
                 $addHTTPVal('inicio':$GetField('DTINIC'));
                 $addHTTPVal('reabertur':$GetField('DTRINI'));
                 $addHTTPVal('encerramento':$GetField('DTFIM'));
                 $addHTTPVal('causa':$GetField('CAUSA'));
                 $addHTTPVal('datasinistro':$GetField('DTSIN'));
                 $addHTTPVal('local':$GetField('LOCSIN'));
                 $addHTTPVal('DataParti':$GetField('DTPART'));
                 $addHTTPVal('Participa':$GetField('NOMPART'));
                 $addHTTPVal('segurado':$GetField('NSEG'));
                 $addHTTPVal('mediador':$GetField('NMED'));
                 $addHTTPVal('ramo':$GetField('RM'));
                 $addHTTPVal('apolice':$GetField('AP'));
                 $addHTTPVal('matricula':$GetField('MATR1'));
                 $addHTTPVal('acordo':$GetField('ACORDO'));
                 $addHTTPVal();
                 WRecord += 1;
                 WQtdRegis += 1;
              ENDDO;
              $addHTTPVal();
              $RmvDBSession(WSqlSess);
           ENDFOR;
        ENDSR;

        //=====================================================
        // Subrotina.: FillProcess
        // Descrição.:
        //=====================================================
        BEGSR FillProcess;
           //---------------------------------------------
           // Criar o Ficheiro
           //---------------------------------------------
           MONITOR;
              WComando = 'DLTF FILE(QTEMP/FSINT008)';
              ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
           ENDMON;
           MONITOR;
              WComando = 'CRTPF FILE(QTEMP/FSINT008) SRCFILE(#JMPRDSRC/+
                          QDDSSRC) OPTION(*NOSRC *NOLIST) SIZE(*NOMAX)';
              ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
           ENDMON;

            //---------------------------------------------
            // Carregar Processos de Sinistros
            //---------------------------------------------
           MONITOR;
              WComando = 'CPYF FROMFILE(QS36F/G.SINPRO) TOFILE(QTEMP/FSINT008) +
                          MBROPT(*ADD) FMTOPT(*MAP *DROP)';
              ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
           ENDMON;
           MONITOR;
              WComando = 'CPYF FROMFILE(QS36F/O.SINPRO) TOFILE(QTEMP/FSINT008) +
                          MBROPT(*ADD) FMTOPT(*MAP *DROP)';
              ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
           ENDMON;
           MONITOR;
              WComando = 'CPYF FROMFILE(QS36F/IDSPRO) TOFILE(QTEMP/FSINT008) +
                          MBROPT(*ADD) FMTOPT(*MAP *DROP)';
              ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
           ENDMON;
        ENDSR;
      /END-FREE
