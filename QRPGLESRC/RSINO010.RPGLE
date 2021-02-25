     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PSINO010
     H* Descrição.: Apresentar os Sinistros da Apólice ou do Segurado
     H*==================================================================
     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D WComando        S            300A   INZ(*BLANKS)
     D PRamo           S              2S 0
     D PApolice        S              7S 0
     D WSegurado       S              6S 0
     D WRamoApolice    DS
     D  WRamo                  1      2  0
     D  Filler                 3      6
     D  WApolice               7     13  0
     D  WRAP                   7     13
     C*=================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PRamo
     C                   PARM                    PApolice
     C
      /FREE
        // WRamo    = %DEC(PRAMO:2:0);
        // WApolice = %DEC(PApolice:7:0);
        WRamo    = PRAMO;
        WApolice = PApolice;

        IF (WRamo = 50);
           EXSR GetSinAuto;
        ELSE;
           EXSR GetSinOR;
        ENDIF;

        RETURN;
        //================================================================
        // Subrotina.: GetSinAuto
        // Objectivo.: Obter lista de Sinistros Auto associados à Apólice.
        //================================================================
        BEGSR GetSinAuto;
           //--------------------------------------------------------
           // Carregar com dados o ficheiro de Sinistros da Apólice
           //--------------------------------------------------------
           MONITOR;
              WComando = 'DLTF FILE(QTEMP/HSTSIN)';
   29         ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
           ENDMON;
           MONITOR;
            WComando = 'CPYF FROMFILE(QS36F/G.SINPRO) TOFILE(QTEMP/HSTSIN) +
                  MBROPT(*REPLACE) CRTFILE(*YES) INCCHAR(*RCD 182 *EQ '''+
                  WRAP + ''')';
   29       ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
              RETURN;
           ENDMON;

           //--------------------------------------------------------
           // Apresentar os registos correspondentes ao Sinistro
           //--------------------------------------------------------
           WComando = 'RUNQRY QRY(SOURCE/HSTSINAUT) QRYFILE((QTEMP/HSTSIN)) +
                    OUTTYPE(*DISPLAY) OUTFORM(*DETAIL)';
   29      ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
        ENDSR;

        //================================================================
        // Subrotina.: GetSinOR
        // Objectivo.: Obter lista de Sinistros OR associados à Apólice.
        //================================================================
        BEGSR GetSinOR;
           //--------------------------------------------------------
           // Carregar com dados o ficheiro de Sinistros da Apólice
           //--------------------------------------------------------
           MONITOR;
              WComando = 'DLTF FILE(QTEMP/HSTSIN)';
   29         ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
           ENDMON;

           MONITOR;
            WComando = 'CPYF FROMFILE(QS36F/O.SINPRO) TOFILE(QTEMP/HSTSIN) +
                   MBROPT(*REPLACE) CRTFILE(*YES) INCCHAR(*RCD 182 *EQ '''+
                   WRAP + ''')';
   29       ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
             RETURN;
           ENDMON;

           //--------------------------------------------------------
           // Apresentar os registos correspondentes ao Sinistro
           //--------------------------------------------------------
           WComando = 'RUNQRY QRY(SOURCE/HSTSINOR) QRYFILE((QTEMP/HSTSIN))  +
                    OUTTYPE(*DISPLAY) OUTFORM(*DETAIL)';
   29      ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
        ENDSR;
      /END-FREE
