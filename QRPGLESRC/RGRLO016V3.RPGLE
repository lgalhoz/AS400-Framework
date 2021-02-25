     H NOMAIN
     H*=================================================================
     H* Biblioteca.: Troca de ficheiros entre sistemas via FTP
     H* Descrição..: Conjunto de de funções para realizar a troca de
     H*              ficheiros entre um AS400 e uma máquina Microsoft,
     H*              Linux, Unix e AS400, através de FTP.
     H*
     H* Observação.: Estrutura dos Códigos de Retornos FTP
     H*              ABCD => Código de Três digitos
     H*              |||
     H*              ||+---> Resultado da Operação [1=Preparação, 2=Conclusão
     H*              ||       1 = Primeira Resposta (positiva)
     H*              ||       2 = Conclusão com sucesso
     H*              ||       3 = Resposta intermédia com sucesso
     H*              ||       4 = Erro mas em recuperação
     H*              ||       5 = Erro sem recuperação
     H*              ||       6 = Protecção
     H*              |+----> Código Intermédio
     H*              +-----> Código Final
     H*
     H* Observação: Código > 600 (Erros de Comunicação)
     H*=================================================================
     FFGRLP200  IF   E           K DISK
     FOUTPUT    IF   F  100        DISK    USROPN EXTFILE('QTEMP/OUTPUT')
     FINPUT     O  A F  120        DISK    USROPN EXTFILE('QTEMP/INPUT')
     FFILEMBR   IF A F   74        DISK    USROPN
     F*=================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Processo para gerar um ficheiro no AS400
     D CrtFile         PR             7A
     D   FileName                    10A   Const
     D   FileLen                      3S 0 Const
     D   LibFileSrc                  21A   Const Options(*NOPASS)
     D   MbrSource                   10A   Const Options(*NOPASS)
     D   LibName                     10A   Const Options(*NOPASS)
     D   IsToDelete                    N   Const Options(*NOPASS)
     D
     D*==> Lista de Processos usados internamente
     D GetAS400Name    PR           100A
     D    Biblioteca                 10A   Const
     D    Ficheiro                   10A   Const
     D
     D*==> Realizar a Transferência
     D MakeTransf      PR             7A
     D    Server                     15A   Const
     D    UserAndPass               120A   Const
     D    Comando                   120A   Const
     D    Formato                     6A   Const  Options(*NOPASS)
     D    IsS36File                   1A   Const  Options(*NOPASS)
     D
     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Lista de campos de trabalho
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WMsgErro        S              7A
     D
     D*==> Lista de Formatos de Registo
     D FTPRecord       DS
     D    FTPField             1    120
     D
     D FILRecord       DS
     D    $LIB                14     23
     D    $FILE               24     33
     D    $MBR                65     74
     D
     D ResRecord       DS
     D  RES_ResText            1    100
     D  RES_StsCode00          1      4
     D  RES_StsCode01          1      1
     D  RES_StsCode02          2      2
     D  RES_StsCode03          3      3
     D  RES_StsCode04          4      4
     D  RES_DesCode            5    100
     D*==================================================================

     P*==================================================================
     P* Processo..: $SendFTP
     P* Descrição.: Transferir qualquer ficheiro AS/400, via FTP.
     P*             Este programa é para ser usado como ferramenta
     P*             entre o antigo AS/400 eo novo AS/400.
     P*==================================================================
     P $SendFTP        B                   EXPORT
     D $SendFTP        PI             7A
     D    Template                    5S 0 Const
     D    NomLib                     10A   Const
     D    NomFile                    10A   Const
     D    DestFile                   20A   Const
     D
     D*==> Campos de Trabalho
     D WTemplate       S              5S 0
     D WNomFile        S             10A
     D WNomLib         S             10A
     D WLibFile        S            100A
     D WDestFile       S             20A
     D WTempalte       S              5S 0
     D WUser           S             10A
     D WPassw          S             15A
     D WServer         S             15A
     D WIsS36File      S              1A
     D WNoPasta        S             20A
     D WComando        S            120A
     D WTpFile         S              4A
     D WDlChar         S              1A
     D WLtChar         S              1A
     D WUpChar         S              1A
     D WSrcRepos       S             21A
     D WSrcMbr         S             10A
     D WDftLib         S             10A
     D WUserAndPass    S            120A
     D WFormato        S              6A
     D*=================================================================
      /FREE
        //-------------------------------------------------------------
        // Assumir valores por defeito para os parametros facultativos
        //-------------------------------------------------------------
        WMsgErro  = *BLANKS; // Mensagem de Erro
        WNomLib   = NomLib;
        WNomFile  = NomFile;
        WDestFile = DestFile;
        WTemplate = Template;

        CHAIN (WTemplate) RGRLP200;
        IF (%FOUND());
           WUser      = CDUSER;    // Utilizador
           WPassw     = CDPASSW;   // Palavra Chave
           WServer    = IPSERVER;  // IP do Servidor
           WFormato   = FORMATO;   // ASCII ou BIN
           WIsS36File = ISS36FILE; // Ficheiro AS400
           WNoPasta   = NOPASTA;   // Pasta do Servidor
           WTpFile    = TPFILE;    // (CVS, TXT, XML)
           WDlChar    = DlChar;    // Caracter delimitador
           WLtChar    = LtChar;    // Aceita caracteres Portugueses
           WUpChar    = UpChar;    // Enviar caracteres em maiusculas
           WSrcRepos  = SRCREPOS;  // Source do repositório
           WSrcMbr    = SRCMBR;    // Membro
           WNomLib    = DFTLIB;    // Biblioteca defeito
           IF (WNomLib = *BLANKS);
              WNomLib = 'QTEMP';
           ENDIF;
        ELSE;
           RETURN 'FTP0008'; // A Template não está definida
        ENDIF;

        //------------------------------------------------
        // Criar Script FTP e enviar ficheiros
        //------------------------------------------------
        IF (NOT $checkObject(WNomFile:WNomLib:'*FILE'));
           RETURN 'FTP0009'; // O ficheiro a enviar não existe
        ENDIF;

        WLibFile = GetAS400Name(WNomLib:WNomFile);
        IF (WLibFile <> *BLANKS);
           WUserAndPass = %trim(WUser)+' '+%TRIM(WPassw);
           WComando     = 'PUT '+%TRIM(WLibFile)+' '+%TRIM(WNoPasta)+
                                 %TRIM(WDestFile);
           WMsgErro = MakeTransf(WServer:WUserAndPass:WComando:
                       WFormato:WIsS36File);
        ENDIF;
        RETURN WMsgErro;
      /END-FREE
     P $SendFTP        E

     P*==================================================================
     P* Processo..: $GetFTP
     P* Descrição.: Receber um ficheiro
     P*==================================================================
     P $GetFTP         B                   EXPORT
     D $GetFTP         PI             7A
     D    Template                    5S 0 Const
     D    DestFile                   20A   Const
     D    NomLib                     10A   Const
     D    NomFile                    10A   Const
     D
     D*==> Campos de Trabalho
     D WTemplate       S              5S 0
     D WNomFile        S             10A
     D WNomFileTmp     S             10A
     D WNomLib         S             10A
     D WNomLibTmp      S            100A
     D WLibFile        S            100A
     D WDestFile       S             20A
     D WTempalte       S              5S 0
     D WUser           S             10A
     D WPassw          S             15A
     D WServer         S             15A
     D WIsS36File      S              1A
     D WNoPasta        S             20A
     D WComando        S            120A
     D WTpFile         S              4A
     D WDlChar         S              1A
     D WLtChar         S              1A
     D WUpChar         S              1A
     D WSrcRepos       S             21A
     D WSrcMbr         S             10A
     D WDftLib         S             10A
     D WUserAndPass    S            120A
     D WLenRecord      S              3S 0
     D WFormato        S              6A
     D WMsgError       S              7A
     D*=================================================================
      /FREE
        //-------------------------------------------------------------
        // Assumir valores por defeito para os parametros facultativos
        //-------------------------------------------------------------
        WMsgError   = *BLANKS;
        WNomLib     = NomLib;
        WNomLibTmp  = NomLib;
        WNomFile    = NomFile;
        WNomFileTmp = NomFile;
        WDestFile   = DestFile;
        WTemplate   = Template;

        CHAIN (WTemplate) RGRLP200;
        IF (%FOUND());
           WUser      = CDUSER;    // Utilizador
           WPassw     = CDPASSW;   // Palavra Chave
           WServer    = IPSERVER;  // IP do Servidor
           WFormato   = FORMATO;   // ASCII ou BIN
           WNoPasta   = NOPASTA;   // Ficheiro AS400
           WTpFile    = TPFILE;    // (CVS, TXT, XML)
           WDlChar    = DlChar;    // Caracter delimitador
           WLtChar    = LtChar;    // Aceita caracteres Portugueses
           WUpChar    = UpChar;    // Enviar caracteres em maiusculas
           WIsS36File = ISS36FILE; // AS400
           WLenRecord = LENRECORD; // Pasta do Servidor
           WSrcRepos  = SRCREPOS;  // Source do repositório
           WSrcMbr    = SRCMBR;    // Membro
           WNomLib    = DFTLIB;    // Biblioteca defeito
           IF (WNomLib = *BLANKS);
              WNomLib = 'QTEMP';
           ENDIF;
           WNomLibTmp = WNomLib;
        ELSE;
           RETURN 'FTP0008'; // A Template não está definida
        ENDIF;

        //--------------------------------------------------
        // Criar o fichero no AS400 para receber o ficheiro
        //--------------------------------------------------
        IF (WTpFile = 'CVS');
           WMsgErro = CrtFile(WNomFile:*ZEROS:WSrcRepos:WSrcMbr:WNomLib);
           IF (WMsgErro <> *Blanks);
              RETURN WMsgErro;
           ENDIF;
           WNomFile = 'TMP'+%CHAR(%subdt(%timestamp():*Hours))
                           +%CHAR(%subdt(%timestamp():*minutes))
                           +%CHAR(%subdt(%timestamp():*seconds));

           WSrcRepos  = *BLANKS;
           WSrcMbr    = *BLANKS;
        ENDIF;

        WMsgErro = CrtFile(WNomFile:WLenRecord:WSrcRepos:WSrcMbr:WNomLib);
        IF (WMsgErro <> *Blanks);
           RETURN WMsgErro;
        ENDIF;
        WLibFile = GetAS400Name(WNomLib:WNomFile);

        //------------------------------------------------
        // Criar Script FTP e enviar ficheiros
        //------------------------------------------------
        IF (WLibFile <> *BLANKS);
           WUserAndPass = %trim(WUser)+' '+%TRIM(WPassw);
           WComando     = 'GET '+%TRIM(WNoPasta)+%TRIM(WDestFile)+
                               ' '+%TRIM(WLibFile)+' (REPLACE';
           WMsgErro = MakeTransf(WServer:WUserAndPass:WComando:
                                 WFormato:WIsS36File);
        ENDIF;

        //---------------------------------------------------------
        // Se for um ficheiro de Transferência então completar
        //---------------------------------------------------------
        IF ((WTpFile = 'CVS') AND (WMsgErro = *BLANKS));
           MONITOR;
              WStdCmd = 'CPYFRMIMPF FROMFILE(QTEMP/'+%TRIM(WNomFile)+
                        ') TOFILE('+%TRIM(WNomLib)+
                        '/'+%TRIM(WNomFileTmp)+') MBROPT(*REPLACE) +
                        FLDDLM('''+%TRIM(WDlChar)+''') RPLNULLVAL(*FLDDFT)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              RETURN 'FTP0010';
           ENDMON;

           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/'+%TRIM(WNomFile)+')';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              RETURN 'FTP0011';
           ENDMON;
        ENDIF;
        RETURN WMsgErro;
      /END-FREE
     P $GetFTP         E

     P*==================================================================
     P* Processo..: MakeTransf
     P* Descrição.: Preparar a transferência entre sistemas.
     P*==================================================================
     P MakeTransf      B                   EXPORT
     D MakeTransf      PI             7A
     D    Server                     15A   Const
     D    UserAndPass               120A   Const
     D    Comando                   120A   Const
     D    Formato                     6A   Const  Options(*NOPASS)
     D    IsS36File                   1A   Const  Options(*NOPASS)
     D    IsToList                    1A   Const  Options(*NOPASS)
     D
     D*==> Campos de Trabalhos
     D WMsgErro        S              7A
     D*==================================================================
      /FREE
        WMsgErro = *BLANKS;
        EXSR CrtFTPScript;
        EXSR SndFTP;
        Return WMsgErro;

        //==============================================================
        // Subrotina.: CrtFTPScritp
        // Descrição.: Construir ficheiro de Trabnsferência e enviar
        //             o ficheiro (S36) ou (AS400), para um sistema
        //             diferente (AS400, Wintel, Unix, ...)
        //==============================================================
        BEGSR CrtFTPScript;
           //--------------------------------------------------------
           // Cria Script de Transferência
           //--------------------------------------------------------
           WMsgErro = CrtFile('INPUT':120);
           IF (WMsgErro <> *BLANKS);
              LEAVESR;
           ENDIF;

           WMsgErro = CrtFile('OUTPUT':254);
           IF (WMsgErro <> *BLANKS);
              LEAVESR;
           ENDIF;

           //--------------------------------------------------------
           // Escrever script de Transferência
           //--------------------------------------------------------
           OPEN INPUT;
           FTPField = UserAndPass;
           WRITE INPUT FTPRecord;
           FTPField = Formato;
           WRITE INPUT FTPRecord;
           FTPField = 'NAM 1';
           WRITE INPUT FTPRecord;
           FTPField = Comando;
           WRITE INPUT FTPRecord;
           FTPField = 'QUIT';
           WRITE INPUT FTPRecord;
           CLOSE INPUT;
        ENDSR;

        //=================================================================
        // Subrotina.: SndFTP
        // Descrição.: Enviar o ficheiro via FPT para o servidor
        //             seleccionado com as cradenciais definidas
        //=================================================================
        BEGSR SndFTP;
           MONITOR;
              //----------------------------------------------
              // Transferir ficheiro
              //----------------------------------------------
              WStdCmd = 'CALL PGM(RGRLC016) PARM('''+%TRIM(Server)+''')';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);

              //----------------------------------------------
              // Testar resultado do envio
              //----------------------------------------------
              EXSR CheckResult;

           ON-ERROR;
              WMsgErro = 'FTP0001'; // Erro na execução do comando
              LEAVESR;
           ENDMON;

           //----------------------------------------------
           // Eliminar Registos Temporários
           //----------------------------------------------
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/INPUT)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WMsgErro = 'FTP0002'; // Ocorreram erros a eliminar
              LEAVESR;
           ENDMON;
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/OUTPUT)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WMsgErro = 'FTP0003'; // Ocorreram erros a eliminar
              LEAVESR;
           ENDMON;
        ENDSR;

        //==============================================================
        // Subrotina.: CheckResult
        // Descrição.: Testar o resultado da Transferência para
        //             Confirmar se foi ou não com sucesso.
        //             Carregar uma listagem.
        //==============================================================
        BEGSR CheckResult;
           WMsgErro = 'FTP0004'; // Ocorreram erros nas Transferência
           OPEN OUTPUT;
           SETLL *HIVAL OUTPUT;
           READP OUTPUT ResRecord;
           DOW NOT %EOF();
              IF (RES_StsCode01 = '2') AND (RES_StsCode00 <> '221');
                 WMsgErro = *BLANKS;
                 LEAVE;
              ELSEIF (RES_StsCode01 = '5');
                 LEAVE;
              ENDIF;
              READP OUTPUT ResRecord;
           ENDDO;
           CLOSE OUTPUT;
        ENDSR;
      /END-FREE
     P MakeTransf      E

     P*==================================================================
     P* Processo..: GetAS400Name
     P* Descrição.: Obter o nome do Objectio no AS400.
     P*==================================================================
     P GetAS400Name    B                   EXPORT
     D GetAS400Name    PI           100A
     D    NomLib                     10A   Const
     D    NomFile                    10A   Const
     D
     D*==> Campos de Trabalho
     D WNomMbro        S             10A
     D WResult         S            100A
     D*===================================================================
      /FREE
        //--------------------------------------------------------
        // Listar Membros dos Ficheiro a Exportar
        //--------------------------------------------------------
        MONITOR;
           WStdCmd = 'DLTF FILE(QTEMP/FILEMBR)';
           WStdLen = %LEN(WStdCmd);
           shell(WStdCmd:WStdLen);
        ON-ERROR;
        ENDMON;
        MONITOR;
           WStdCmd = 'DSPFD FILE('+%TRIM(NomLib)+'/'+%TRIM(NomFile) + ') +
                      TYPE(*MBRLIST) OUTPUT(*OUTFILE) +
                      OUTFILE(QTEMP/FILEMBR)';
           WStdLen = %LEN(WStdCmd);
           shell(WStdCmd:WStdLen);
        ON-ERROR;
           RETURN *BLANKS;
        ENDMON;

        WNomMbro = *BLANKS;
        OPEN FILEMBR;
        READ FILEMBR FILRecord;
        IF NOT %EOF();
           WNomMbro = $MBR;
           CLOSE FILEMBR;
        ENDIF;

        IF (WNomMbro <> *BLANKS);
           WResult  = '/QSYS.LIB/'+%TRIM(NomLIB)+'.LIB/'+%TRIM(NomFile)+
                      '.FILE/'+%TRIM(WNomMbro)+'.MBR';
        ELSE;
           WMsgErro = 'FTP0005'; // Não existem membros
        ENDIF;

        //--------------------------------------------------------
        // Eliminar o ficheiro temporário de trabalho
        //--------------------------------------------------------
        MONITOR;
           WStdCmd = 'DLTF FILE(QTEMP/FILEMBR)';
           WStdLen = %LEN(WStdCmd);
           shell(WStdCmd:WStdLen);
        ON-ERROR;
        ENDMON;

        RETURN WResult;
      /END-FREE
     P GetAS400Name    E

     P*==================================================================
     P* Processo..: CrtFile
     P* Descrição.: Criar ficheiro no AS400.
     P*==================================================================
     P CrtFile         B                   EXPORT
     D CrtFile         PI             7A
     D   FileName                    10A   Const
     D   FileLen                      3S 0 Const
     D   LibFileSrc                  21A   Const Options(*NOPASS)
     D   MbrSource                   10A   Const Options(*NOPASS)
     D   LibName                     10A   Const Options(*NOPASS)
     D   IsToDelete                    N   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WFileName       S             10A
     D WFileLen        S              3S 0
     D WLibFileSrc     S             21A
     D WMbrSource      S             10A
     D WLibName        S             10A
     D WIsToDelete     S               N
     D*==================================================================
      /FREE
         WFileName   = FileName;
         WFileLen    = FileLen;
         WLibFileSrc = *BLANKS;
         WLibName    = 'QTEMP';
         WIsToDelete = *ON;
         IF (%PARMS() > 2);
            WLibFileSrc = LibFileSrc;
            IF (%PARMS() > 3);
               WMbrSource = MbrSource;
               IF (%PARMS() > 4);
                  IF (LibName <> *BLANKS);
                     WLibName = LibName;
                  ENDIF;
                  IF (%PARMS() > 5);
                     WIsToDelete = IsToDelete;
                  ENDIF;
               ENDIF;
            ENDIF;
         ENDIF;

         //------------------------------------------------------
         // Eliminar o ficheiro se já existir
         //------------------------------------------------------
         IF (WIsToDelete);
            IF ($checkObject(WFileName:WLibName:'*FILE'));
            MONITOR;
               WStdCmd = 'DLTF FILE('+%TRIM(WLibName)+'/'+
                          %TRIM(WFileName)+')';
               WStdLen = %LEN(WStdCmd);
               shell(WStdCmd:WStdLen);
            ON-ERROR;
               RETURN 'FTP0006';
            ENDMON;
            ENDIF;
         ENDIF;

         //------------------------------------------------------
         // Criar o ficheiro de Dados com ou sem source
         //------------------------------------------------------
         IF (WFileLen > *ZEROS);
            WStdCmd = 'CRTPF FILE('+%TRIM(WLibName) +'/'+
                       %TRIM(WFileName)+') RCDLEN('+%CHAR(WFileLen)+
                      ') OPTION(*NOSRC) SIZE(*NOMAX)';
         ELSE;
            WStdCmd = 'CRTPF FILE('+%TRIM(WLibName)+'/'+
                       %TRIM(WFileName)+
                       ') SRCFILE('+%TRIM(WLibFileSrc)+
                       ') SRCMBR('+%TRIM(WMbrSource)+
                       ') SIZE(*NOMAX) OPTION(*NOSRC)';
         ENDIF;
         MONITOR;
            WStdLen = %LEN(WStdCmd);
            shell(WStdCmd:WStdLen);
         ON-ERROR;
            RETURN 'FTP0007';
         ENDMON;
         RETURN *BLANKS;
      /END-FREE
     P CrtFile         E
