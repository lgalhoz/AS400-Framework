     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PGRLO006
     H* Data......: 23/Nov/2012
     H* Descrição.: Realizar a troca automática de Interfaces entre
     H*             a Lusitania e a Jose Mata.
     H*
     H*             Mover os ficheiros da Lusitania para Temporário DB2
     H*             e colocar Interfaces no Share da Lusitania.
     H*==================================================================
     FFGRLP200  IF   E           K DISK
     FFGRLP201  IF   E           K DISK
     FOUTPUT    IF   F  100        DISK    USROPN
     FFTPSCRIPT UF A F  120        DISK    USROPN
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010

     D*---------------------------------------------
     D* Invocar um comando de AS/400 externo:
     D*---------------------------------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D*---------------------------------------------
     D* Lista de Parametros Recebidos
     D*---------------------------------------------
     D File            DS
     D   NomFile                     10A
     D   NomLib                      10A
     D
     D FTPParm         S            120A
     D WLib_File       S            100A
     D WAlias          S             10A
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WIcStatus       S               N
     D WIdx            S              3S 0
     I*=================================================================
     IFTPSCRIPT AA
     I                                  1  120  FTPRec
     IOUTPUT    AA
     I                                  1   80  $RESTEXT
     C*=================================================================
      /FREE

        //------------------------------------------------
        // Criar Script FTP e enviar ficheiros
        //------------------------------------------------
        EXSR CrtFTPScript;
        EXSR SndFTP;

        *INLR = *ON;
        //=================================================================
        // Subrotina.: SndFTP
        // Descrição.: Enviar o ficheiro via FPT para o servidor
        //             seleccionado com as cradenciais definidas
        //=================================================================
        BEGSR SndFTP;
           MONITOR;
              WStdCmd = 'OVRDBF FILE(INPUT) TOFILE(QTEMP/FTPSCRIPT)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);

              WStdCmd = 'OVRDBF FILE(OUTPUT) TOFILE(QTEMP/RESULT)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);

              WStdCmd = 'FTP RMTSYS(*INTNETADR) INTNETADR(''172.16.1.3'')';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);

              EXSR CheckResult;
              IF (WIcStatus = *ON);
                 // Transferência sem sucesso
              ENDIF;

              WStdCmd = 'DLTOVR FILE(*ALL)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;
        ENDSR;

        //==============================================================
        // Subrotina.: CheckResult
        // Descrição.: Testar o resultado da Transferência para
        //             Confirmar se foi ou não com sucesso.
        //==============================================================
        BEGSR CheckResult;
           WIcStatus = *ON;  // Erros
           OPEN OUTPUT;
           SETLL *HIVAL OUTPUT;
           READP OUTPUT;
           DOW NOT %EOF();
              IF (%SCAN('bytes transferidos em':$ResText) > 0);
                 WIcStatus = *OFF;  // Sem Erros
              // LEAVE;
              ENDIF;
              READP OUTPUT;
           ENDDO;
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
              WStdCmd = 'DLTF FILE(QTEMP/FTPSCRIPT)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/RESULT)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           MONITOR;
              WStdCmd = 'CRTPF FILE(QTEMP/FTPSCRIPT) RCDLEN(120)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           //--------------------------------------------------------
           // Escrever script de Transferência
           //--------------------------------------------------------
           OPEN FTPScript;
           WIdx = *ZEROS;
           DOW (WIdx < 9);
              WIdx += 1;
              FTPRec = TXTFTP(WIdx);
              FTPRec = $strReplace('%USRNAME':'jmata':FTPRec);
              FTPRec = $strReplace('%PASSW':'jmata!lusitania':FTPRec);
              FTPRec = $strReplace('%ORIGEM':'/qsys.lib/soapserv.lib/+
                                    fcatp010.file/fcatp010.mbr':FTPRec);
              FTPRec = $strReplace('%DESTINO':'fcatp010.txt':FTPRec);
              EXCEPT AAAAA;
           ENDDO;
           CLOSE FTPScript;
        ENDSR;
      /END-FREE
     O**********************************************************************
     OFTPSCRIPT EADD         AAAAA
     O                       FTPRec             120
