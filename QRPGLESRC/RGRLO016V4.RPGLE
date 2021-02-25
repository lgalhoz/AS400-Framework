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
     FFGRLP200  IF   E           K DISK    USROPN
     FFGRLP20201IF   E           K DISK    USROPN PREFIX(GRL_)
     FFGRLP20301IF   E           K DISK    USROPN PREFIX(GRL_)
     FOUTPUT    IF   F  100        DISK    USROPN EXTFILE('QTEMP/OUTPUT')
     FINPUT     O  A F  120        DISK    USROPN EXTFILE('QTEMP/INPUT')
     F*=================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Realizar a Transferência
     D MakeTransf      PR             7A
     D    Server                     15A   Const
     D    UserAndPass               120A   Const
     D    Comando                   120A   Const
     D    Formato                     6A   Const  Options(*NOPASS)
     D    IsS36File                   1A   Const  Options(*NOPASS)
     D    IsToList                    1A   Const  Options(*NOPASS)
     D
     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Parametros do Servidor FTP Activo
     D LST#FTPServer   DS                  QUALIFIED
     D   Template                     5S 0 INZ(*ZEROS)
     D   IpServer                    15A   INZ(*BLANKS)
     D   IsSFTP                       1A   INZ('N')
     D   CdUser                      20A   INZ(*BLANKS)
     D   CdPassw                     20A   INZ(*BLANKS)
     D   NoPasta                     40A   INZ(*BLANKS)
     D   Formato                      6A   INZ(*BLANKS)
     D   TpFile                       4A   INZ(*BLANKS)
     D   DlChar                       1A   INZ(*BLANKS)
     D   UpChar                       1A   INZ(*BLANKS)
     D   IsS36File                    1A   INZ(*BLANKS)
     D   LenRecord                    3S 0 INZ(*ZEROS)
     D   SrcRePos                    21A   INZ(*BLANKS)
     D   SrcMbr                      10A   INZ(*BLANKS)
     D   DftLib                      10A   INZ(*BLANKS)
     D
     D*==> lista com o conteudo da pasta FTP do Servidor
     D LST#FTPList     DS                  QUALIFIED DIM(999)
     D   Template                     5S 0 INZ(*ZEROS)
     D   FileName                    40A   INZ(*BLANKS)
     D   TimeStamp                     Z
     D   Size                         9S 0 INZ(*ZEROS)
     D   ATTR                         7A   INZ(*BLANKS)
     D   IsDeleted                     N   INZ(*OFF)
     D   IsEmpty                       N   INZ(*ON)
     D WCurRec         S              3S 0
     D WIdx            S              3S 0
     D
     D*==> Campos de trabalho
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WMsgErro        S              7A
     D
     D*==========================================
     D*      Formatos de Registo
     D*==========================================
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
     P* Processo..: $InzFTPList
     P* Descrição.: Listar conteudo do Servidor
     P*==================================================================
     P $InzFTPList     B                   EXPORT
     D $InzFTPList     PI             7A
     D   Template                     5S 0 CONST
     D
     D*==> Campos de Trabalho
     D WTemplate       S              5S 0
     D WNoPasta        S             40A
     D WUser           S             10A
     D WPassw          S             15A
     D WServer         S             15A
     D WComando        S            120A
     D WSrcMbr         S             10A
     D WUserAndPass    S            120A
     D WMsgError       S              7A
     D*=================================================================
      /FREE
        //-------------------------------------------------------------
        // Assumir valores por defeito para os parametros facultativos
        //-------------------------------------------------------------
        LST#FTPList(1).IsEmpty = *ON;
        WMsgError   = *BLANKS;
        WTemplate   = Template;

        OPEN FGRLP200;
        CHAIN (WTemplate) RGRLP200;
        IF (%FOUND());
           WUser      = CDUSER;    // Utilizador
           WPassw     = CDPASSW;   // Palavra Chave
           WServer    = IPSERVER;  // IP do Servidor
           WNoPasta   = NOPASTA;   // Ficheiro AS400
        ELSE;
           RETURN 'FTP0008'; // A Template não está definida
        ENDIF;
        CLOSE FGRLP200;

        //------------------------------------------------
        // Criar Script FTP e enviar ficheiros
        //------------------------------------------------
        WUserAndPass = %trim(WUser)+' '+%TRIM(WPassw);
        WComando     = %TRIM(WNoPasta);
        WMsgErro = MakeTransf(WServer:WUserAndPass:WComando:
                              *BLANKS:*BLANKS:'S');

        RETURN WMsgErro;
      /END-FREE
     P $InzFTPList     E

     P*==================================================================
     P* Processo..: $GetFTPFirst
     P* Descrição.: Obter o primeiro ficheiro da pasta do servidor
     P*==================================================================
     P $GetFTPFirst    B                   EXPORT
     D $GetFTPFirst    PI            40A
     D   NomLib                      10A   CONST OPTIONS(*NOPASS)
     D   NomFile                     10A   CONST OPTIONS(*NOPASS)
     D   IcAction                      N   CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WNomLib         S             10A
     D WNomFile        S             10A
     D WIcAction       S               N
     D*==================================================================
      /FREE
         WNomLib   = *BLANKS;
         WNomFile  = *BLANKS;
         WIcAction = *OFF;
         IF (%PARMS() > *ZEROS);
            WNomLib = NomLib;
            IF (%PARMS() > 1);
               WNomFile = NomFile;
               IF (%PARMS() > 2);
                  WIcAction = IcAction;
               ENDIF;
            ENDIF;
         ENDIF;

         IF (WNomLib = *BLANKS);
            WCurRec = 1;
            IF (NOT LST#FTPList(WCurRec).IsEmpty);
               RETURN LST#FTPList(WCurRec).FileName;
            ELSE;
               RETURN *BLANKS;
            ENDIF;
            WCurRec += 1;
         ENDIF;
      /END-FREE
     P $GetFTPFirst    E

     P*==================================================================
     P* Processo..: $GetFTPNext
     P* Descrição.: Obter o ficheiro seguinte da pasta do servidor
     P*==================================================================
     P $GetFTPNext     B                   EXPORT
     D $GetFTPNext     PI            40A
     D   NomLib                      10A   CONST OPTIONS(*NOPASS)
     D   NomFile                     10A   CONST OPTIONS(*NOPASS)
     D   IcAction                      N   CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WNomLib         S             10A
     D WNomFile        S             10A
     D WIcAction       S               N
     D*==================================================================
      /FREE
         WNomLib   = *BLANKS;
         WNomFile  = *BLANKS;
         WIcAction = *OFF;
         IF (%PARMS() > *ZEROS);
            WNomLib = NomLib;
            IF (%PARMS() > 1);
               WNomFile = NomFile;
               IF (%PARMS() > 2);
                  WIcAction = IcAction;
               ENDIF;
            ENDIF;
         ENDIF;

         IF (WNomLib = *BLANKS);
            IF (NOT LST#FTPList(WCurRec).IsEmpty);
               WCurRec += 1;
               RETURN LST#FTPList(WCurRec).FileName;
            ELSE;
               RETURN *BLANKS;
            ENDIF;
         ENDIF;
      /END-FREE
     P $GetFTPNext     E

     P*==================================================================
     P* Processo..: $IsFTPEof
     P* Descrição.: Validar se ainda existem registos
     P*==================================================================
     P $IsFTPEof       B                   EXPORT
     D $IsFTPEof       PI              N
     D*==================================================================
      /FREE
         RETURN LST#FTPList(WCurRec).IsEmpty;
      /END-FREE
     P $IsFTPEof       E

     P*==================================================================
     P* Processo..: $ChkFTPFile
     P* Descrição.: Validar se o ficheiro existe no servidor
     P*==================================================================
     P $ChkFTPFile     B                   EXPORT
     D $ChkFTPFile     PI              N
     D   FileName                    40A   CONST
     D
     D*==> Campos de Trabalho
     D WLenFile        S              3S 0
     D*==================================================================
      /FREE
         WLenFile = %LEN(%TRIM(FileName));
         WIdx = 1;
         DOW (NOT LST#FTPList(WIdx).IsEmpty);
            IF (%SUBST(%TRIM(LST#FTPList(Widx).FileName):1:WLenFile) =
              %Subst(%TRIM(FileName):1:WLenFile));
               RETURN *ON;
            ENDIF;
            WIdx += 1;
         ENDDO;
         RETURN *OFF;
      /END-FREE
     P $ChkFTPFile     E

     P*==================================================================
     P* Processo..: $GetFTPFileLen
     P* Descrição.: Obter dimensao do Ficheiro no servidor
     P*==================================================================
     P $GetFTPFileLen  B                   EXPORT
     D $GetFTPFileLen  PI             9S 0
     D   FileName                    40A   CONST
     D*==================================================================
      /FREE
         IF (NOT LST#FTPList(WCurRec).IsEmpty);
            RETURN LST#FTPList(WCurRec).Size;
         ELSE;
            RETURN *ZEROS;
         ENDIF;
      /END-FREE
     P $GetFTPFileLen  E

     P*==================================================================
     P* Processo..: $GetFTPFileDat
     P* Descrição.: Obter a data e  hora do ficheiro no servidor
     P*==================================================================
     P $GetFTPFileDat  B                   EXPORT
     D $GetFTPFileDat  PI              Z
     D   FileName                    40A   CONST
     D
     D WResultado      S               Z
     D*==================================================================
      /FREE
         IF (NOT LST#FTPList(WCurRec).IsEmpty);
            RETURN LST#FTPList(WCurRec).TimeStamp;
         ELSE;
            RETURN %timestamp();
         ENDIF;
      /END-FREE
     P $GetFTPFileDat  E

     P*==================================================================
     P* Processo..: $GetFTPFileAtr
     P* Descrição.: Obter Atributo do Ficheiro no servidor
     P*==================================================================
     P $GetFTPFileAtr  B                   EXPORT
     D $GetFTPFileAtr  PI             7A
     D   FileName                    40A   CONST
     D*==================================================================
      /FREE
         IF (NOT LST#FTPList(WCurRec).IsEmpty);
            RETURN LST#FTPList(WCurRec).ATTR;
         ELSE;
            RETURN *BLANKS;
         ENDIF;
      /END-FREE
     P $GetFTPFileAtr  E

     P*==================================================================
     P* Processo..: $DelFTPFile
     P* Descrição.: Eliminar um ficheiro no Servidor
     P*==================================================================
     P $DelFTPFile     B                   EXPORT
     D $DelFTPFile     PI             7A
     D   Template                     5S 0 CONST
     D   FileName                    40A   CONST
     D
     D*==> Campos de Trabalho
     D WUser           S             10A
     D WPassw          S             15A
     D WServer         S             15A
     D WNoPasta        S             40A
     D WMsgErro        S              7A
     D WComando        S            120A
     D WUserAndPass    S            120A
     D*==================================================================
      /FREE
        OPEN FGRLP200;
        CHAIN (Template) RGRLP200;
        IF (%FOUND());
           WUser      = CDUSER;    // Utilizador
           WPassw     = CDPASSW;   // Palavra Chave
           WServer    = IPSERVER;  // IP do Servidor
           WNoPasta   = NOPASTA;   // Pasta do Servidor
        ELSE;
           RETURN 'FTP0008'; // A Template não está definida
        ENDIF;
        CLOSE FGRLP200;

        //------------------------------------------------
        // Criar Script FTP e enviar ficheiros
        //------------------------------------------------
        WUserAndPass = %trim(WUser)+' '+%TRIM(WPassw);
        WComando     = 'RM '+%TRIM(WNoPasta)+%TRIM(FileName);
        RETURN  MakeTransf(WServer:WUserAndPass:WComando);
      /END-FREE
     P $DelFTPFile     E

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
     D    DestFile                   40A   Const
     D
     D*==> Campos de Trabalho
     D WTemplate       S              5S 0
     D WNomFile        S             10A
     D WNomLib         S             10A
     D WLibFile        S            100A
     D WDestFile       S             40A
     D WTempalte       S              5S 0
     D WUser           S             10A
     D WPassw          S             15A
     D WServer         S             15A
     D WIsS36File      S              1A
     D WNoPasta        S             40A
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
        OPEN FGRLP200;
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
        CLOSE FGRLP200;

        //------------------------------------------------
        // Criar Script FTP e enviar ficheiros
        //------------------------------------------------
        IF (NOT $checkObject(WNomFile:WNomLib:'*FILE'));
           RETURN 'FTP0009'; // O ficheiro a enviar não existe
        ENDIF;

        WLibFile = $GetAS400Name(WNomLib:WNomFile);
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
     D    DestFile                   40A   Const
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
     D WDestFile       S             40A
     D WTempalte       S              5S 0
     D WUser           S             10A
     D WPassw          S             15A
     D WServer         S             15A
     D WIsS36File      S              1A
     D WNoPasta        S             40A
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

        OPEN FGRLP200;
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
        CLOSE FGRLP200;

        //--------------------------------------------------
        // Criar o fichero no AS400 para receber o ficheiro
        //--------------------------------------------------
        IF (%TRIM(WTpFile) = 'CVS');
           WMsgErro = $CrtFile(WNomFile:*ZEROS:WSrcRepos:WSrcMbr:WNomLib);
           IF (WMsgErro <> *Blanks);
              RETURN WMsgErro;
           ENDIF;
           WNomFile = 'TMP'+%CHAR(%subdt(%timestamp():*Hours))
                           +%CHAR(%subdt(%timestamp():*minutes))
                           +%CHAR(%subdt(%timestamp():*seconds));

           WSrcRepos  = *BLANKS;
           WSrcMbr    = *BLANKS;
        ENDIF;

        WMsgErro = $CrtFile(WNomFile:WLenRecord:WSrcRepos:WSrcMbr:WNomLib);
        IF (WMsgErro <> *Blanks);
           RETURN WMsgErro;
        ENDIF;
        WLibFile = $GetAS400Name(WNomLib:WNomFile);

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
        IF ((%TRIM(WTpFile) = 'CVS') AND (WMsgErro = *BLANKS));
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
     D*==> lista com o conteudo da pasta FTP do Servidor
     D LST#FTPParms    DS                  QUALIFIED DIM(10)
     D   TpField                      2A   INZ(*BLANKS)
     D   NmField                     20A   INZ(*BLANKS)
     D   ValDeft                     10A   INZ(*ZEROS)
     D   PosIni                       3S 0 INZ(*ZEROS)
     D   PosFim                       3S 0 INZ(*ZEROS)
     D   DimField                     3S 0 INZ(*ZEROS)
     D   IsEmpty                       N
     D
     D*==> Campos de Trabalhos
     D WFormato        S              6A
     D WIsS36File      S              1A
     D WIsToList       S              1A
     D WMsgErro        S              7A
     D WStartList      S               N
     D WSOperativo     S             40A
     D WCodRetorno     S              4S 0
     D WIdxName        S              3S 0
     D WIdxFolder      S              3S 0
     D WColuna         S             40A
     D*==================================================================
      /FREE
        WMsgErro   = *BLANKS;
        WFormato   = *BLANKS;
        WIsS36File = *BLANKS;
        WIsToList  = 'N';
        IF (%PARMS() > 3);
           WFormato   = Formato;
           IF (%PARMS() > 4);
              WIsS36File = IsS36File;
              IF (%PARMS() > 5);
                 WIsToList  = IsToList;
              ENDIF;
           ENDIF;
        ENDIF;

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
           WMsgErro = $CrtFile('INPUT':120);
           IF (WMsgErro <> *BLANKS);
              LEAVESR;
           ENDIF;

           WMsgErro = $CrtFile('OUTPUT':254);
           IF (WMsgErro <> *BLANKS);
              LEAVESR;
           ENDIF;

           //--------------------------------------------------------
           // Escrever script de Transferência
           //--------------------------------------------------------
           OPEN INPUT;
           FTPField = UserAndPass;
           WRITE INPUT FTPRecord;
           IF (WFormato <> *BLANKS);
              FTPField = WFormato;
              WRITE INPUT FTPRecord;
              FTPField = 'NAM 1';
              WRITE INPUT FTPRecord;
           ENDIF;
           IF (WIsToList = 'S');
              FTPField = 'CD ' + %TRIM(Comando);
              WRITE INPUT FTPRecord;
              FTPField = 'DIR';
              WRITE INPUT FTPRecord;
           ELSE;
              FTPField = Comando;
              WRITE INPUT FTPRecord;
           ENDIF;
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
           WMsgErro    = 'FTP0004'; // Ocorreram erros nas Transferência
           WStartList  = *OFF;
           WSOperativo = *BLANKS;

           OPEN OUTPUT;
           //----------------------------------------------------
           // Validar Sistema Operativo do Servidor FTP
           //----------------------------------------------------
           IF (WIsToList='S');
              READ OUTPUT ResRecord;
              DOW NOT %EOF();
                 IF (Res_StsCode00='230');
                    WStartList = *ON;
                 ELSEIF (WStartList);
                    WSOperativo = %TRIM(RES_ResText);
                    LEAVE;
                 ENDIF;
                 READ OUTPUT ResRecord;
              ENDDO;
              IF (WSOperativo <> *BLANKS);
                 EXSR GetDefList;
              ENDIF;
           ENDIF;
           WStartList = *OFF;
           WIdx = 1;

           //----------------------------------------------------
           // Validar o resultado
           //----------------------------------------------------
           SETLL *HIVAL OUTPUT;
           READP OUTPUT ResRecord;
           DOW NOT %EOF();
              MONITOR;
                 WCodRetorno = %DEC(RES_StsCode00:4:0);
              ON-ERROR;
                 WCodRetorno = *ZEROS;
              ENDMON;
              IF (((RES_StsCode01='2') AND (RES_StsCode00<>'221') AND
                (WIsToList='N')) OR ((RES_StsCode01='1') AND (WIsToList='S')))
                AND (WCodRetorno > *ZEROS);
                 WMsgErro = *BLANKS;
                 LEAVE;
              ELSEIF ((RES_StsCode01='2') AND (RES_StsCode00<>'221')
                 AND (WIsToList='S') AND (WCodRetorno > *ZEROS));
                 WStartList = *ON;
              ELSEIF ((RES_StsCode01 = '5') AND (WCodRetorno > *ZEROS));
                 LEAVE;
              ELSEIF (WStartList);
                 EXSR FillFTPList;
              ENDIF;
              READP OUTPUT ResRecord;
           ENDDO;
           CLOSE OUTPUT;
        ENDSR;

        //==============================================================
        // Subrotina.: GetDefList
        // Descrição.: Obter a estrutura da lista devolvida pelo
        //             Serividor aquando da execução do comando "DIR".
        //==============================================================
        BEGSR GetDefList;
           WIdx = 1;
           LST#FTPParms(WIdx).IsEmpty = *ON;
           OPEN FGRLP20201;
           CHAIN (WSOperativo) RGRLP202;
           IF (%FOUND());
              OPEN FGRLP20301;
              //-------------------------------------------
              // Obter a Posição do nome do ficheiro
              //-------------------------------------------
              SETLL (GRL_CDSERVER) RGRLP203;
              READE (GRL_CDSERVER) RGRLP203;
              DOW (NOT %EOF(FGRLP20301));
                 IF (GRL_TPFIELD = 'NM');
                    WIdxName = WIdx;
                 ELSEIF (GRL_TPFIELD = 'DR');
                    WIdxFolder = WIdx;
                 ENDIF;

                 LST#FTPParms(WIdx).TpField  = GRL_TPFIELD;
                 LST#FTPParms(WIdx).NmField  = GRL_NMFIELD;
                 LST#FTPParms(WIdx).ValDeft  = GRL_VALDEFT;
                 LST#FTPParms(WIdx).PosIni   = GRL_POSINI;
                 LST#FTPParms(WIdx).PosFim   = GRL_POSFIM;
                 LST#FTPParms(WIdx).DimField = GRL_POSFIM - GRL_POSINI + 1;
                 LST#FTPParms(WIdx).IsEmpty  = *OFF;
                 Widx += 1;
                 LST#FTPParms(WIdx).IsEmpty  = *ON;
                 READE (GRL_CDSERVER) RGRLP203;
              ENDDO;

              //-------------------------------------------
              // Obter condição para Directorio
              //-------------------------------------------
              CLOSE FGRLP20301;
           ENDIF;
           CLOSE FGRLP20201;
        ENDSR;

        //==============================================================
        // Subrotina.: FillFTPList
        // Descrição.: Carregar lista de ficheiros existentes no Servidor
        //             FTP.
        //==============================================================
        BEGSR FillFTPList;
           //----------------------------------------------
           // Excluir as Pastas ou Folders
           //----------------------------------------------
           WColuna = %SUBST(RES_ResText:LST#FTPParms(WIdxFolder).PosIni:
                                        LST#FTPParms(WIdxFolder).DimField);
           IF (%TRIM(WColuna) <> LST#FTPParms(WIdxFolder).ValDeft);
              WColuna = %SUBST(RES_ResText:LST#FTPParms(WIdxName).PosIni:
                                           LST#FTPParms(WIdxName).DimField);
              LST#FTPList(WIdx).FileName = %TRIM(WColuna);
              LST#FTPList(WIdx).IsEmpty = *OFF;
              WIdx += 1;
              LST#FTPList(WIdx).IsEmpty = *ON;
           ENDIF;
        ENDSR;
      /END-FREE
     P MakeTransf      E
