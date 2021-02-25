     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PTSTO014
     H* Descrição.: Procurar registos nas Tabelas S36
     H*==================================================================
     FTMPFILE   IF   F   10        DISK    USROPN  InfDS(InfDS)
     F                                     EXTFILE('QTEMP/TESTE')
     F*==============================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Contar numero de registos
     D InfDS           ds
     D  FileLib               93    102a
     D  NbrRecs              156    159i 0
     D
     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D WComando        S            300A   INZ(*BLANKS)
     D PRamo           S              2S 0
     D PApolice        S              7S 0
     D WSegurado       S              6S 0
     D WQtdRecords     S              6S 0
     D WRamoApolice    DS
     D  WRamo                  1      2  0
     D  WApolice               3      9  0
     D  WRAP                   1      9
     C*=================================================================
     C*    *ENTRY        PLIST
     C*                  PARM                    PRamo
     C*                  PARM                    PApolice
     C
      /FREE
        PRamo = 15;
        PApolice = 9984891;

        WRamo    = PRAMO;
        WApolice = PApolice;

        EXSR CrtSaco;
        WRamo = 15;
        WApolice = 9984891;
        EXSR CheckRegisto;
        EXSR FindApolice;

        WRamo = 05;
        WApolice = 9900006;
        EXSR CheckRegisto;
        EXSR FindApolice;

        WRamo = 50;
        WApolice = 9910527;
        EXSR CheckRegisto;
        EXSR FindApolice;

        WRamo = 15;
        WApolice = 9984891;
        EXSR CheckRegisto;
        EXSR FindApolice;

        *INLR = *ON;
        RETURN;
        //================================================================
        // Subrotina.: CrtSaco
        // Objectivo.: Criar Temporário para conter os registos.
        //================================================================
        BEGSR CrtSaco;
           WQtdRecords = *ZEROS;

           //--------------------------------------------------------
           // Criar ficheiro para receber os dados
           //--------------------------------------------------------
           MONITOR;
              WComando = 'DLTF FILE(QTEMP/TESTE)';
   29         ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
           ENDMON;

           MONITOR;
              WComando = 'CRTPF FILE(QTEMP/TESTE) RCDLEN(10)';
   29         ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
           ENDMON;
        ENDSR;

        //================================================================
        // Subrotina.: CheckRegisto
        // Objectivo.: Procurar a existência do registo.
        //================================================================
        BEGSR CheckRegisto;
           //--------------------------------------------------------
           // Invocar o Comando de Cópia e testar o resultado
           //--------------------------------------------------------
           MONITOR;
              WComando = 'CPYF FROMFILE(LGALHOZ/GGUIA01A) +
                          TOFILE(QTEMP/TESTE) +
                          MBROPT(*ADD) +
                          INCCHAR(*RCD 10 *EQ ''' + WRamoApolice +
                          ''') +
                          FMTOPT(*NOCHK)';
   29         ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
           ON-ERROR;
           ENDMON;
        ENDSR;

        //================================================================
        // Subrotina.: FindApolice
        // Objectivo.: Procurar a existência de uma Apólice.
        //================================================================
        BEGSR FindApolice;
           MONITOR;
              OPEN TMPFILE;
              IF (NbrRecs > WQtdRecords);
                 DSPLY 'Sucesso';
                 WQtdRecords = NbrRecs;
              ELSE;
                 DSPLY 'Erros';
              ENDIF;
              CLOSE TMPFILE;
           ON-ERROR;
           ENDMON;
        ENDSR;
      /END-FREE
