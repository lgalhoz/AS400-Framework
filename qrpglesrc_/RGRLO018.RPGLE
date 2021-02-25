     H NOMAIN
     H*=======================================================
      /Copy QRPGLESRC,SGRLO010
     P*=======================================================
     P* Processo..: $shellExec
     P* Objectivo.: Executar um Comando de AS/400
     P*=======================================================
     P $shellExec      B                   EXPORT
     D                 PI              N
     D   shellcmd                   300A   CONST OPTIONS(*VARSIZE)
     D
     D exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D WStdLen         S             15P 5
     D WResult         S               N
     C*=================================================================
      /FREE

        MONITOR;
           WStdLen = %LEN(ShellCmd);
           exec(ShellCmd:WStdLen);
        ON-ERROR;
        ENDMON;

        RETURN WResult;
      /END-FREE
     P $shellExec      E
