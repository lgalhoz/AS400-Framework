     H ALWNULL(*USRCTL)  DFTACTGRP(*NO) ACTGRP(*NEW) AUT(*ALL)
     H BNDDIR('QC2LE':'JMDIR')
     H*===========================================================
     H* Programa..: PGRLO016
     H* Descrição.: Escutar a pasta da CMPSeguro e validar se
     H*             existem ficheiros a processar.
     H*===========================================================
     FFINTP010  UF A E           K DISK    PREFIX(INT_)
     FFINTP01001IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP010:RINTP01001)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO016
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Tratamento de Interfaces
     D ReadInterface   PR                  EXTPGM(WNomPrograma)
     D   NomFile                     21A   CONST
     D   MsgErro                      7A   CONST
     D
     D*==> Ler uma linha de texto
     D ReadLine        PR            10I 0
     D   fd                          10I 0 value
     D   text                          *   value
     D   maxlen                      10I 0 value
     D
     D*==> Inserir o registo
     D $ChangeRecord   PR              N
     D    FileName                   21A   Const
     D    ListField                        Const LIKEDS(LST#FILE)
     D                                     DIM(100)
     D    IO#Operacao                 1A   Const
     D
     D*==> Estrutura dos comandos AS400
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D WComando        S            300A
     D
     D*==> Layout do Interface
     D LST#FILE        DS                  DIM(100) QUALIFIED
     D    Field                      10A
     D    Tipo                        1A
     D    PosIni                      3S 0
     D    Dimensao                    3S 0
     D    Value                      80A
     D    IsEmpty                      N
     D WIdx            S              3S 0
     D
     D*==> Campos de Trabalho
     D WFolder         S               *
     D WMsg_           S             52A
     D
     D WNuSequ         S              5S 0
     D WExtFile        S             21A
     D WCdFile         S              9S 0
     D WCdRegis        S              9S 0
     D WAAMMDD         S              6A
     D WDatFile        S             10A
     D WNomPasta       S             40A
     D WNomFile        S             40A
     D WFileInDir      S             40A
     D WLenFile        S              3S 0
     D WCdPrograma     S              5S 0
     D WNomPrograma    S             10A
     D WCdServer       S              5S 0
     D WCdTempl        S              5S 0
     D WNomPastaBk     S             40A
     D WSessTPL        S              5S 0
     D WRecDIR         S              5S 0
     D WRecTPL         S              5S 0
     D WFieldDef       S              5S 0
     D WRecDef         S              5S 0
     D WCdInter        S              9S 0
     D WMsgErro        S              7A
     D
     D WFd             S             10I 0
     D WLine           S            400A
     D WLine_          S            400A
     D WIcSair         S               N
     D WLen            S             10I 0
     H*===========================================================
      /FREE
       //---------------------------------------------------
       // Sessão de ficheiros a tratar
       //---------------------------------------------------
       WSessTPL = $SetFiles('FGRLP210 A INNER JOIN FGRLP200 B +
                             ON (A.CDTEMPL = B.CDTEMPL)');
       $SetFields('B.NOPASTA,A.NOMFILE,A.CDPROGR,+
                   A.CODDIRE,A.CDTEMPL,A.CDINTER,+
                   B.CDSERVER');
       $SetSort('NOPASTA');

       //---------------------------------------------------
       // Sessão de ficheiros a tratar
       //---------------------------------------------------
       WFieldDef = $SetFiles('FGRLP211');
       $SetFields('LCFIELD,NUSEQU,NOFIELD,POINI,DIMENSAO,TIPO');
       $SetSort('NUSEQU');

       EXSR ExecRecebidos; // Tratar Registos Recebidos da CMPSeguro

       $RmvDBSession(WSessTPL);
       $RmvDBSession(WFieldDef);

       MONITOR;
          WComando = 'COMMIT';
          ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
       ON-ERROR;
       // WMsgErro = 'XXXXXXX';
       ENDMON;

       *INLR = *ON;
       RETURN;

       //=============================================================
       // Subrotina..: ExecRecebidos
       // Descrição..: Escutar pasta da CMPSeguro e tratar ficheiros
       //              recebidos.
       //=============================================================
       BEGSR ExecRecebidos;
          $SetDBSession(WSessTPL);
          WNomPasta = *BLANKS;
          WNomPastaBk = *BLANKS;
          $SetCriteria('(A.CODDIRE = ''E'')');
          WRecDIR = 1;
          DOW ($GetRecord(WRecDIR) > *ZEROS);
             //----------------------------------------
             // Actualizar conteudo da Pasta
             //----------------------------------------
             WNomFile    = $GetField('A.NOMFILE');
             WNomPasta   = $GetField('B.NOPASTA');
             WCdTempl    = $GetFieldNum('A.CDTEMPL');
             WCdPrograma = $GetFieldNum('A.CDPROGR');
             WCdInter    = $GetFieldNum('A.CDINTER');
             WCdServer   = $GetFieldNum('B.CDSERVER');
             IF (WCdServer = 4);
                WFolder = $OpenDir(%TRIM(WNomPasta));
                IF (WFolder <> *NULL);
                   P_dirent = $ReadDir(WFolder);
                   DOW (P_dirent <> *NULL);
                      WFileInDir = %SUBST(dirent.name:1:dirent.namelen);
                      WLenFile = %LEN(%TRIM(WNomFile));
                      IF (%LEN(%TRIM(WFileInDir)) >= WLenFile);
                         IF (%TRIM(WNomFile) =
                             %SUBST(%TRIM(WFileInDir):1:WLenFile));
                             EXSR ExecFile;
                         ENDIF;
                      ENDIF;
                      P_dirent = $ReadDir(WFolder);
                   ENDDO;
                ENDIF;
             ENDIF;
             WRecDIR += 1;
          ENDDO;
       ENDSR;

       //===============================================================
       // Subrotina.: ExecFile
       // Descição..:
       //===============================================================
       BEGSR ExecFile;
          WMsgErro = *BLANKS;

          //-------------------------------------------------
          // Validar se o Ficheiro já foi processado
          //-------------------------------------------------
          EXSR ValFile;
          IF (WMsgErro <> *BLANKS);
             LEAVESR;
          ENDIF;

          //-------------------------------------------------
          // Carregar Layout do Ficheiro
          //-------------------------------------------------
          WIcSair = *OFF;
          $SetDBSession(WFieldDef);
          $SetCriteria('(CDINTER = ''' + %CHAR(WCdInter) + ''')');
          WRecTPL = 1;
          WNuSequ = 1;
          LST#FILE(WNuSequ).IsEmpty  = *ON;
          DOW ($GetRecord(WRecTPL) > *ZEROS);
             IF ($Getfield('LCFIELD') = 'R');
                LST#FILE(WNuSequ).Field    = $GetField('NOFIELD');
                LST#FILE(WNuSequ).Tipo     = $Getfield('TIPO');
                LST#FILE(WNuSequ).PosIni   = $GetFieldNum('POINI');
                LST#FILE(WNuSequ).Dimensao = $GetFieldNum('DIMENSAO');
                LST#FILE(WNuSequ).Value    = *BLANKS;
                LST#FILE(WNuSequ).IsEmpty  = *OFF;
                WNuSequ += 1;
                LST#FILE(WNuSequ).IsEmpty  = *ON;
             ENDIF;
             WRecTPL += 1;
          ENDDO;
          $SetDBSession(WSessTPL);

          //-------------------------------------------------
          // Criar o ficheiro Físico
          //-------------------------------------------------
          EXSR CrtPfile;
          IF (WMsgErro <> *BLANKS);
             LEAVESR;
          ENDIF;

          //-------------------------------------------------
          // Procede à Leitura do Interface
          //-------------------------------------------------
          WLine_ = '######';
          WFd = $Open(%TRIM(WNomPasta)+%TRIM(WFileInDir):O#RDONLY+
                      O#TEXTDATA+O#CCSID:S#IRGRP:37);
          IF (Wfd >= *ZEROS);
             DOW (ReadLine(WFd:%addr(WLine):%Size(WLine)) >= *ZEROS);
                IF (WLine <> *BLANKS);
                   WIdx = 1;
                   DOW (NOT LST#FILE(WIdx).IsEmpty);
                      LST#FILE(WIdx).Value = %SUBST(WLine:
                                             LST#FILE(WIdx).PosIni:
                                             LST#FILE(WIdx).Dimensao);
                      WIdx += 1;
                   ENDDO;
                   $ChangeRecord(WExtFile:LST#File:'I');
                ENDIF;
             ENDDO;
             $Close(WFd);
          ENDIF;

          //-------------------------------------------------
          // Proceder ao tratamento dos Registos
          //-------------------------------------------------
          COMMIT;
          MONITOR;
          IF (WCdPrograma > *ZEROS);
             WNomPrograma = $GetFunction(WCdPrograma);
             ReadInterface(WExtFile:*BLANKS);
          ENDIF;
          ON-ERROR;
          ENDMON;
       ENDSR;

       //===============================================================
       // Subrotina.: ValiFile
       // Descição..: Validar se já foi processado
       //===============================================================
       BEGSR ValFile;
          CHAIN (WFileInDir) RINTP01001;
          IF (%FOUND());
             WMsgErro = 'INT0001'; // O ficheiro já foi processado
             LEAVESR;
          ELSE;
             WCdRegis    = $GetNextKey('FENTP010':'CDREGIS');
             WCdFile     = $GetNextKey('FINTP010':'CDFILE');
             INT_CDFILE  = WCdFile;
             INT_NOMFILE = WFileInDir;
             MONITOR;
                IF (WCdInter = 17);
                   WDatFile   = %SUBST(WFileInDir:14:4) + '-' +
                                %SUBST(WFileInDir:18:2) + '-' +
                                %SUBST(WFileInDir:20:2);
                   INT_DTFILE = %DATE(WDatFile:*ISO); // ForCompr com a data do ficheiro
                ELSEIF (WCdInter = 18);
                   WDatFile   = %SUBST(WFileInDir:15:4) + '-' +
                                %SUBST(WFileInDir:19:2) + '-' +
                                %SUBST(WFileInDir:21:2);
                   INT_DTFILE = %DATE(WDatFile:*ISO); // ForCompr com a data do ficheiro
                ENDIF;
             ON-ERROR;
                INT_DTFILE  = %DATE();
             ENDMON;
             INT_DTPROC  = %DATE();
             WRITE RINTP010;
          ENDIF;
       ENDSR;

       //===============================================================
       // Subrotina.: CrtPFile
       // Descição..:
       //===============================================================
       BEGSR CrtPfile;
          MONITOR;
             IF (WCdInter = 17);
                WAAMMDD  = %SUBST(WFileInDir:16:6);
                WExtFile = 'QS36F/FICA' + %TRIM(WAAMMDD);
                WComando = 'CRTPF FILE(' + %TRIM(WExtFile) + ') +
                                  SRCFILE(SOURCE/QDDSSRC) +
                                  SRCMBR(FICAT) SIZE(*NOMAX)';
             ELSEIF (WCdInter = 18);
                WAAMMDD  = %SUBST(WFileInDir:17:6);
                WExtFile = 'QS36F/FICI' + %TRIM(WAAMMDD);
                WComando = 'CRTPF FILE(' + %TRIM(WExtFile) + ') +
                                  SRCFILE(SOURCE/QDDSSRC) +
                                  SRCMBR(FICIDS) SIZE(*NOMAX)';
             ENDIF;
             ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
          // WMsgErro = 'XXXXXXX';
          ENDMON;

          MONITOR;
             WComando = 'CLRPFM FILE(' + %TRIM(WExtFile) + ')';
             ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
          // WMsgErro = 'XXXXXXX';
          ENDMON;

          MONITOR;
             WComando = 'STRJRNPF FILE(' + %TRIM(WExtFile) + ') +
                         JRN(#JMPRDDBS/JMDBS) OMTJRNE(*OPNCLO)';
             ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
          // WMsgErro = 'XXXXXXX';
          ENDMON;
       ENDSR;
      /END-FREE

     P*=======================================================
     P* Processo..: $ChangeRecord
     P* Descrição.: Actualizar o registo
     C*=======================================================
     P $ChangeRecord   B                   EXPORT
     D                 PI              N
     D    FileName                   21A   Const
     D    Lista                            Const LIKEDS(LST#FILE)
     D                                     DIM(100)
     D    IO#Operacao                 1A   Const
     D
     D*==> Campos de Trabalho
     D  WFileName      S             21A
     D  WSqlSTMT       S           1000A
     D  WFields        S            100A
     D  WValues        S            300A
     D  WIOResult      S               N
     D  WIdx           S              3S 0
     D
     D*==> Constantes
     D  IO#Insert      C                   'I'
     D  IO#Delete      C                   'D'
     D  IO#Update      C                   'U'
     D*===========================================================
      /FREE
       WIOResult = *ON;
       WFileName = %TRIM(FileName);

       //------------------------------------------------
       // Inicalizar lista de Trabalho
       //------------------------------------------------
       IF (IO#Operacao = IO#Insert);
          WFields  = *BLANKS;
          WValues  = *BLANKS;
          WSqlStmt = 'INSERT INTO ' + %TRIM(WFileName);
          WIdx = 1;
          DOW (NOT Lista(WIdx).IsEmpty);
             WFields = %TRIM(WFields) + %TRIM(Lista(WIdx).Field) + ',';
             IF (%TRIM(Lista(WIdx).Value) = *BLANKS);
                IF (Lista(WIdx).Tipo = 'A');
                   WValues = %TRIM(WValues)+''' '',';
                ELSE;
                   WValues = %TRIM(WValues)+'''0'',';
                ENDIF;
             ELSE;
                WValues = %TRIM(WValues)+''''+%TRIM(Lista(WIdx).Value) + ''',';
             ENDIF;
             WIdx += 1;
          ENDDO;
          WFields = %SUBST(WFields:1:%LEN(%TRIM(WFields))-1);
          WValues = %SUBST(WValues:1:%LEN(%TRIM(WValues))-1);
          WSqlStmt = %TRIM(WSqlStmt) + ' (' + %TRIM(WFields) + ') +
                     VALUES(' + %TRIM(WValues) + ')';
       ELSEIF (IO#Operacao = IO#Update);
       ELSEIF (IO#Operacao = IO#Delete);
       ENDIF;

       EXEC SQL PREPARE STMINSERT FROM :WSqlStmt;
       EXEC SQL EXECUTE STMINSERT;

       RETURN WIOResult;
      /END-FREE
     P $ChangeRecord   E

     P*=======================================================
     P* Processo..: ReadLine
     P* Descrição.: Ler uma linha do ficheiro de Texto
     P*             É um algoritmo artesanal.
     C*=======================================================
     P ReadLine        B
     D ReadLine        PI            10I 0
     D   fd                          10I 0 value
     D   text                          *   value
     D   maxlen                      10I 0 value
     D
     D rdbuf           S           1024A   static
     D rdpos           S             10I 0 static
     D rdlen           S             10I 0 static
     D
     D p_retstr        S               *
     D WRetStr         S          32766A   based(p_retstr)
     D Wlen            S             10I 0
     D*=================================================================
      /FREE
         WLen = 0;
         p_retstr = text;
         %subst(WRetStr:1:MaxLen) = *blanks;
         dow (1 = 1);

            //--------------------------------------------
            //  Carregar o texto
            //--------------------------------------------
            if (rdpos >= rdlen);
               rdpos = 0;
               rdlen=$read(fd:%addr(rdbuf):%size(rdbuf));

               if (rdlen < 1);
                  return -1;
               endif;
            endif;

            //--------------------------------------------
            //  Validar se é o fim da linha
            //--------------------------------------------
            rdpos = rdpos + 1;
            if (%subst(rdbuf:rdpos:1) = x'25');
               return  Wlen;
            endif;

            //--------------------------------------------
            // Se não for adiciona o texto à palavra.
            //--------------------------------------------
            if (%subst(rdbuf:rdpos:1) <> x'0d' and Wlen <> maxlen);
               Wlen += 1;
               %subst(WRetstr:Wlen:1) = %subst(rdbuf:rdpos:1);
            endif;
         enddo;
         return  Wlen;
      /END-FREE
     P                 E
