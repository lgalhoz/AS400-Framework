     H*NOMAIN
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP('QILE')
     H*AUT(*ALL)
     H*=================================================================
     H* Biblioteca.: Troca de ficheiros entre sistemas via FTP
     H* Descrição..: Conjunto de de funções para realizar a troca de
     H*              ficheiros entre um AS400 e uma máquina Microsoft,
     H*              Linux, Unix e AS400, através de FTP.
     H*=================================================================
     FGRLP200   IF   E           K DISK
     FOUTPUT    IF   F  100        DISK    USROPN EXTFILE('QTEMP/OUTPUT')
     FINPUT     O  A F  120        DISK    USROPN EXTFILE('QTEMP/INPUT')
     FFILEMBR   IF A F   80        DISK    USROPN
     F*=================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D
     D*==> Lista de Campos de Trabalho
     D FTPRecord       DS
     D    FTPField             1    120
     I*=================================================================
     IFILEMBR   AA
     I                                 14   23  $LIB
     I                                 24   33  $FILE
     I                                 65   74  $MBR
     IOUTPUT    AA
     I                                  1   80  $RESTEXT
     I*==================================================================
      /FREE
          IF ($SendFTP('QTEMP':'TESTE':'teste.txt'));
             DSPLY 'Erro';
          ENDIF;
        *INLR = *ON;
      /END-FREE
     C*==================================================================

     P*==================================================================
     P* Processo..: $SendFTP
     P* Descrição.: Transferir qualquer ficheiro AS/400, via FTP.
     P*             Este programa é para ser usado como ferramenta
     P*             entre o antigo AS/400 eo novo AS/400.
     P*==================================================================
     P $SendFTP        B                   EXPORT
     D $SendFTP        PI              N
     D    NomLib                     10A   Const
     D    NomFile                    10A   Const
     D    DestFile                   20A   Const
     D    User                       10A   Const OPTIONS(*NOPASS)
     D    Passw                      15A   Const OPTIONS(*NOPASS)
     D    Server                     15A   Const OPTIONS(*NOPASS)
     D    Fmt                         5A   Const OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WNomFile        S             10A
     D WNomLib         S             10A
     D WNomMbro        S             10A
     D WLibFile        S            100A
     D WDestFile       S             20A
     D WUser           S             10A
     D WPassw          S             15A
     D WServer         S             15A
     D WFmt            S              5A
     D WIcErro         S               N
     D
     D*==> Constantes
     D WDftUSer        C                   'ntorrado'
     D WDftPassw       C                   'Demomat@2011'
     D WDftServer      C                   '20.0.0.130'
     D WDftFmt         C                   'ASCII'
     D*=================================================================
      /FREE
        //-------------------------------------------------------------
        // Assumir valores por defeito para os parametros facultativos
        //-------------------------------------------------------------
        WIcErro   = *ON; // Com erros por defeiro
        WNomLib   = NomLib;     // P01
        WNomFile  = NomFile;    // P02
        WDestFile = DestFile;   // P03
        WUser     = WDftUSer;   // P04
        WPassw    = WDftPassw;  // P05
        WServer   = WDftServer; // P06
        WFmt      = WDftFmt;    // P07
        IF (%PARMS > 3);
           WUser = User;
           IF (%PARMS > 4);
              WPassw = Passw;
              IF (%PARMS > 5);
                 WServer = Server;
                 IF (%PARMS > 6);
                    WFmt = Fmt;
                 ENDIF;
              ENDIF;
           ENDIF;
        ENDIF;

        //------------------------------------------------
        // Criar Script FTP e enviar ficheiros
        //------------------------------------------------
        EXSR getMbrName;
        EXSR CrtFTPScript;
        EXSR SndFTP;

        RETURN WIcErro;

      /END-FREE
     P $SendFTP        E

     P*==================================================================
     P* Processo..: MakeTransf
     P* Descrição.: Preparar a transferência entre sistemas.
     P*==================================================================
     P MakeTransf      B                   EXPORT
     D MakeTransf      PI              N
     D    NomLib                     10A   Const
     D*==================================================================
      /FREE
        //==============================================================
        // Subrotina.: getMbrName
        // Descrição.: Obter nome do formato de registo do ficheiro
        //             envio de ficheiro S36.
        //==============================================================
        BEGSR getMbrName;
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
              WStdCmd = 'DSPFD FILE('+%TRIM(WNomLib)+'/'+%TRIM(WNomFile) + ') +
                        TYPE(*MBRLIST) OUTPUT(*OUTFILE) +
                        OUTFILE(QTEMP/FILEMBR)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           WNomMbro = *BLANKS;
           OPEN FILEMBR;
           READ FILEMBR;
           IF NOT %EOF();
              WNomMbro = $MBR;
              CLOSE FILEMBR;
           ENDIF;

           IF (WNomMbro <> *BLANKS);
              WLibFile = '/QSYS.LIB/'+%TRIM(WNomLIB)+'.LIB/'+%TRIM(WNomFile)+
                         '.FILE/'+%TRIM(WNomMbro)+'.MBR';
           ELSE;
              WIcErro = *ON;
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
        ENDSR;

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
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/INPUT)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;
           MONITOR;
              WStdCmd = 'CRTPF FILE(QTEMP/INPUT) RCDLEN(120)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WIcErro = *ON;
              LEAVESR;
           ENDMON;

           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/OUTPUT)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;
           MONITOR;
              WStdCmd = 'CRTSRCPF FILE(QTEMP/OUTPUT) RCDLEN(254) MBR(*FILE)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WIcErro = *ON;
              LEAVESR;
           ENDMON;

           //--------------------------------------------------------
           // Escrever script de Transferência
           //--------------------------------------------------------
           OPEN INPUT;
           FTPField = %trim(WUser)+' '+%TRIM(WPassw);
           WRITE INPUT FTPRecord;
           FTPField = WFmt;
           WRITE INPUT FTPRecord;
           FTPField = 'NAM 1';
           WRITE INPUT FTPRecord;
           FTPField = 'PUT '+%TRIM(WLibFile)+' '+%TRIM(WDestFile);
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
              WStdCmd = 'CALL PGM(RGRLO016C) PARM('''+%TRIM(WServer)+''')';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);

              //----------------------------------------------
              // Testar resultado do envio
              //----------------------------------------------
              EXSR CheckResult;

              //----------------------------------------------
              // Eliminar Eegistios Temporários
              //----------------------------------------------
              MONITOR;
                 WStdCmd = 'DLTF FILE(QTEMP/INPUT)';
                 WStdLen = %LEN(WStdCmd);
                 shell(WStdCmd:WStdLen);
              ON-ERROR;
              ENDMON;
              MONITOR;
                 WStdCmd = 'DLTF FILE(QTEMP/OUTPUT';
                 WStdLen = %LEN(WStdCmd);
                 shell(WStdCmd:WStdLen);
              ON-ERROR;
              ENDMON;
           ON-ERROR;
           ENDMON;
        ENDSR;

        //==============================================================
        // Subrotina.: CheckResult
        // Descrição.: Testar o resultado da Transferência para
        //             Confirmar se foi ou não com sucesso.
        //==============================================================
        BEGSR CheckResult;
           WIcErro = *ON;  // Erros
           OPEN OUTPUT;
           SETLL *HIVAL OUTPUT;
           READP OUTPUT;
           DOW NOT %EOF();
              IF (%SCAN('bytes transferidos em':$ResText) > 0);
                 WIcErro = *OFF;  // Sem Erros
                 LEAVE;
              ENDIF;
              READP OUTPUT;
           ENDDO;
           CLOSE OUTPUT;
        ENDSR;
      /END-FREE
     P MakeTransf      E
