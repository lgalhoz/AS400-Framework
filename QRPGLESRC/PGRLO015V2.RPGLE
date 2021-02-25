     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PGRLO015
     H* Descrição.: Escutar a pasta da CMPSeguro e validar se
     H*             existem ficheiros a processar.
     H*===========================================================
     FFGRLP200  IF   E           K DISK    PREFIX(SEC_)
     FFGRLP210  UF   E           K DISK    PREFIX(SEC_)
     FFTMPT010  IF   F   40        DISK    USROPN
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Listar conteudo da Pasta FTP
     D $ListFTP        PR             7A
     D    Template                    5S 0 Const
     D
     D*==> Realizar a Transferência
     D MakeTransf      PR             7A
     D    Server                     15A   Const
     D    UserAndPass               120A   Const
     D    Comando                   120A   Const
     D    Formato                     6A   Const  Options(*NOPASS)
     D    IsS36File                   1A   Const  Options(*NOPASS)
     D
     D*==> Processo para gerar um ficheiro no AS400
     D CrtFile         PR             7A
     D   FileName                    10A   Const
     D   FileLen                      3S 0 Const
     D   LibFileSrc                  21A   Const Options(*NOPASS)
     D   MbrSource                   10A   Const Options(*NOPASS)
     D   LibName                     10A   Const Options(*NOPASS)
     D   IsToDelete                    N   Const Options(*NOPASS)
     D
     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Tratamento de Interfaces
     D ReadInterface   PR                  EXTPGM(DynamicCall)
     D   NomFile                     20A   CONST
     D   NomPasta                    40A   CONST
     D   MsgErro                      7A   CONST
     D DynamicCall     S             10A   INZ(*BLANKS)
     D
     D*==> Lista de Trabalho
     D LST#Pasta       DS                  QUALIFIED DIM(500)
     D    NomFile                    40A   INZ(*BLANKS)
     D    NomPasta                   40A   INZ(*BLANKS)
     D    IsProcess                   1A   INZ(*BLANKS)
     D    IsEmpty                      N   INZ(*ON)
     D
     D*==> Formato de Registo
     D RTMPT010        DS
     D   WFileName                   40A
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D WNomPasta       S             40A
     D WNomFile        S             20A
     D WLenFile        S              3S 0
     D WCdPrograma     S              5S 0
     D WCdTempl        S              5S 0
     D WNomPastaBk     S             40A
     D WSession        S              5S 0
     D WRecord         S              5S 0
     D WIdx            S              5S 0
     D WMsgErro        S              7A
     D*===========================================================
      /FREE
       //---------------------------------------------------
       // Sessão de ficheiros a tratar
       //---------------------------------------------------
       WSession = $SetFiles('FGRLP210 A INNER JOIN FGRLP200 B +
                             ON (A.CDTEMPL = B.CDTEMPL)');
       $SetFields('B.NOPASTA,A.NOMFILE,A.CDPROGR,+
                   A.CODDIRE,A.CDTEMPL');
       $SetSort('NOPASTA');

       //---------------------------------------------------
       // Tratar informação de registos a enviar
       //---------------------------------------------------
       WMsgErro = *BLANKS;
       LST#Pasta(1).IsEmpty = *ON;
       EXSR ExecRecebidos; // Tratar Registos Recebidos da CMPSeguro
       EXSR ExecEnviados;  // Registar registos enviados para a CMPSeguro

       //----------------------------------------------
       // Mover Interfaces desconhecidos
       //----------------------------------------------
       IF (NOT LST#Pasta(1).IsEmpty);
          EXSR MoveEstranhos;
          LST#Pasta(1).IsEmpty = *ON;
       ENDIF;

       $RmvDBSession(WSession);
       *INLR = *ON;
       RETURN;

       //=============================================================
       // Subrotina..: ListPasta
       // Descrição..: Listar ficheiros existentes na Pasta
       //=============================================================
       BEGSR ListPasta;
          //----------------------------------------------
          // Mover Interfaces desconhecidos
          //----------------------------------------------
          IF (NOT LST#Pasta(1).IsEmpty);
             EXSR MoveEstranhos;
             LST#Pasta(1).IsEmpty = *ON;
          ENDIF;

          //----------------------------------------------
          // Listar Conteudo da Pasta
          //----------------------------------------------
          MONITOR;
             WStdCmd = 'QSH CMD(''ls '+%TRIM(WNomPasta)+
                        '> /interfaces/indice.txt'')';
             WStdLen = %LEN(WStdCmd);
             Exec(WStdCmd:WStdLen);
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
             LEAVESR;
          ENDMON;

          //----------------------------------------------
          // Copiar Conteudo da Pasta para o AS400
          //----------------------------------------------
          WMsgErro = CrtFile('FTMPT010':40);
          WMsgErro = $GetFTP(8:'indice.txt':'QTEMP':'FTMPT010');
          MONITOR;
             WStdCmd = 'CPYFRMSTMF FROMSTMF(''/interfaces/indice.txt'') +
                     TOMBR(''/qsys.lib/qtemp.lib/FTMPT010.FILE/FTMPT010.MBR'') +
                     MBROPT(*REPLACE)';
             WStdLen = %LEN(WStdCmd);
             Exec(WStdCmd:WStdLen);
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
             LEAVESR;
          ENDMON;

          //----------------------------------------------
          // Eliminar ficheiro de Indice
          //----------------------------------------------
          MONITOR;
             WStdCmd = 'QSH CMD(''rm /interfaces/indice.txt'')';
             WStdLen = %LEN(WStdCmd);
             Exec(WStdCmd:WStdLen);
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
             LEAVESR;
          ENDMON;

          //----------------------------------------------
          // Actualizar lista de Trabalho
          //----------------------------------------------
          MONITOR;
             WIdx = 1;
             OPEN FTMPT010;
             READ FTMPT010 RTMPT010;
             DOW (NOT %EOF(FTMPT010));
                LST#Pasta(WIdx).NomFile   = WFileName;
                LST#Pasta(WIdx).NomPasta  = WNomPasta;
                LST#Pasta(WIdx).IsProcess = 'N';
                LST#Pasta(WIdx).IsEmpty   = *OFF;
                WIdx += 1;
                LST#Pasta(WIdx).IsEmpty   = *ON;
                READ FTMPT010 RTMPT010;
             ENDDO;
             CLOSE FTMPT010;
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
             LEAVESR;
          ENDMON;
       ENDSR;

       //=============================================================
       // Subrotina..: ExecRecebidos
       // Descrição..: Escutar pasta da CMPSeguro e tratar ficheiros
       //              recebidos.
       //=============================================================
       BEGSR ExecRecebidos;
          WNomPasta = *BLANKS;
          WNomPastaBk = *BLANKS;
          $SetCriteria('(A.CODDIRE = ''E'')');
          WRecord = 1;
          DOW ($GetRecord(WRecord) > *ZEROS);
             //----------------------------------------
             // Actualizar conteudo da Pasta
             //----------------------------------------
             WNomPasta = $GetField('B.NOPASTA');
             IF (WNomPasta <> WNomPastaBk);
                WNomPastaBk = WNomPasta;
                $ListFTP($GetFieldNum('A.CDTEMPL'));
                EXSR ListPasta;
             ENDIF;

             //----------------------------------------
             // Processar o ficheiro
             //----------------------------------------
             WNomFile = $GetField('A.NOMFILE');
             EXSR ExecFile;
             WRecord += 1;
          ENDDO;
       ENDSR;

       //=============================================================
       // Subrotina..: ExecEnviados
       // Descrição..: Inventariar Interfaces a enviar e ver se
       //              estão a ser enviados na periodicidade prevista.
       //=============================================================
       BEGSR ExecEnviados;
          WNomPasta = *BLANKS;
          WNomPastaBk = *BLANKS;
          $SetCriteria('(A.CODDIRE = ''S'')');
          WRecord = 1;
          DOW ($GetRecord(WRecord) > *ZEROS);
             WNomPasta = $GetField('B.NOPASTA');
             IF (WNomPasta <> WNomPastaBk);
                $ListFTP($GetFieldNum('A.CDTEMPL'));
                WNomPastaBk = WNomPasta;
                EXSR ListPasta;
             ENDIF;

             //----------------------------------------
             // Processar o ficheiro
             //----------------------------------------
             WNomFile    = $GetField('A.NOMFILE');
             WCdPrograma = $GetFieldNum('A.CDPROGR');
             EXSR ExecFile;
             WRecord += 1;
          ENDDO;
       ENDSR;

       //=============================================================
       // Subrotina..: ExecFile
       // Descrição..: Processar o ficheiro e trabalhar de acordo com
       //              o seu conteudo.
       //=============================================================
       BEGSR ExecFile;
          WLenFile = %LEN(%TRIM(WNomFile));
          WIdx = 1;
          DOW (NOT LST#Pasta(WIdx).IsEmpty);
             IF (%Subst(LST#Pasta(Widx).NomFile:1:WLenFile) =
                  %Subst(WNomFile:1:WLenFile));
                //----------------------------------------
                // Invocar o programa para o Interface
                //----------------------------------------
                IF (WCdPrograma > *ZEROS);
                   MONITOR;
                      DynamicCall = 'sss'; // Obter nome do Programa
                      ReadInterface(WnomFile:WNomPasta:WMsgErro);
                   ON-ERROR;
                      WMsgErro = 'XXXXXXX';
                   ENDMON;
                ENDIF;

                //----------------------------------------
                // Mover Interface e Actualizar Controlo
                //----------------------------------------
                IF (WMsgErro = *BLANKS);
                   // Mover Ficheiros para tratados
                   LST#Pasta(Widx).IsProcess = 'S';
                ENDIF;
             ENDIF;
             WIdx += 1;
          ENDDO;
       ENDSR;

       //=============================================================
       // Subrotina..: MoveEstranhos
       // Descrição..: Mover ficheiros desconhecidos ou não registados
       //=============================================================
       BEGSR MoveEstranhos;
          WIdx = 1;
          DOW (NOT LST#Pasta(WIdx).IsEmpty);
             IF (LST#Pasta(WIdx).IsProcess = 'N');
                // Mover este ficheiro para Erros
             ENDIF;
             WIdx += 1;
          ENDDO;
       ENDSR;
      /END-FREE

     P*==================================================================
     P* Processo..: $ListFTP
     P* Descrição.: Listar conteudo de uma pasta FTP.
     P*==================================================================
     P $ListFTP        B                   EXPORT
     D $ListFTP        PI             7A
     D    Template                    5S 0 Const
     D
     D*==> Campos de Trabalho
     D WTemplate       S              5S 0
     D WUser           S             10A
     D WPassw          S             15A
     D WServer         S             15A
     D WNoPasta        S             20A
     D WComando        S            100A
     D WUserAndPass    S            120A
     D*=================================================================
      /FREE
        WMsgErro  = *BLANKS; // Mensagem de Erro
        WTemplate = Template;

        CHAIN (WTemplate) RGRLP200;
        IF (%FOUND());
           WUser      = SEC_CDUSER;    // Utilizador
           WPassw     = SEC_CDPASSW;   // Palavra Chave
           WServer    = SEC_IPSERVER;  // IP do Servidor
           WNoPasta   = SEC_NOPASTA;   // Pasta do Servidor
        ELSE;
           RETURN 'FTP0008'; // A Template não está definida
        ENDIF;
        WMsgErro = CrtFile('FTMPT010':40);

        //------------------------------------------------
        // Criar Script FTP e enviar ficheiros
        //------------------------------------------------
        WUserAndPass = %trim(WUser)+' '+%TRIM(WPassw);
        WComando     = 'ls ' +%TRIM(WNoPasta);
        WMsgErro = MakeTransf(WServer:WUserAndPass:WComando:'TXT':'N');
        RETURN WMsgErro;
      /END-FREE
     P $ListFTP        E
