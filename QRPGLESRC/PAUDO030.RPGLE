     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PAUDO020
     H* Descrição.: Auditar Ficheiros de Recibos enviados
     H*
     H* CRTPF FILE(QTEMP/FINTT014) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*
     H*===========================================================
     FFINTT014  UF   E           K DISK    USROPN

     D*==> Lista de Parametros
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Lista de Letras
     D WValLetra       S              1A   DIM(15)
     D
     D*==> Campos de Trabalho
     D WComando        S            300A
     D WAno            S              2A
     D WMes            S              2A
     D WLetra          S              2S 0
     D WNomFile        S             10A
     D*===========================================================
      /FREE
       //-------------------------------------------------
       // Carregar Variáveis de Trabalho
       //-------------------------------------------------
       WValLetra(1) = '1';
       WValLetra(2) = '2';
       WValLetra(3) = '3';
       WValLetra(4) = '4';
       WValLetra(5) = '5';
       WValLetra(6) = '6';
       WValLetra(7) = '7';
       WValLetra(8) = '8';
       WValLetra(9) = '9';
       WValLetra(10) = 'A';
       WValLetra(11) = 'B';
       WValLetra(12) = 'C';
       WValLetra(13) = 'D';
       WValLetra(14) = 'E';
       WValLetra(15) = 'F';
       WMes = '01';
       WAno = '16';

       //-------------------------------------------------
       // Carregar Ficheiros de Recibos gerados no Sistema
       //-------------------------------------------------
       EXSR CrtTmpFile;
       EXSR FillContinuados;
       EXSR FillCarregamentos;
       EXSR FillManuais;
       EXSR FixFile;

       *INLR = *ON;
       RETURN;

       //======================================================================
       // Rotina.....:
       // Objectivo..:
       //======================================================================
       BEGSR CrtTmpFile;
          //-------------------------------------------------
          // Eliminar o ficheiro com os interfaces
          //-------------------------------------------------
          MONITOR;
             WComando = 'DLTF FILE(#JMPRDDBS/FINTT014)';
             ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
          // WMsgErro = 'XXXXXXX';
          ENDMON;

          //-------------------------------------------------
          // Criar ficheiro de Interface
          //-------------------------------------------------
          MONITOR;
             WComando = 'CRTPF FILE(#JMPRDDBS/FINTT014) +
                         SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)';
             ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
          // WMsgErro = 'XXXXXXX';
          ENDMON;
       ENDSR;

       //======================================================================
       // Rotina.....:
       // Objectivo..:
       //======================================================================
       BEGSR FillContinuados;
          WNomFile = 'GGUI' + %TRIM(WAno) + %TRIM(WMes);
          MONITOR;
             WComando = 'CPYF FROMFILE(QS36F/'+
                              %TRIM(WNomFile)+') +
                              TOFILE(#JMPRDDBS/FINTT014) +
                              MBROPT(*ADD) FMTOPT(*NOCHK)';
             ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
             // WMsgErro = 'XXXXXXX';
          ENDMON;
       ENDSR;

       //======================================================================
       // Rotina.....:
       // Objectivo..:
       //======================================================================
       BEGSR FillCarregamentos;
          FOR WLetra = 1 By 1 TO 15;
             WNomFile = 'GGUIA' + %TRIM(WMes) + WValLetra(WLetra);
             MONITOR;
                WComando = 'CPYF FROMFILE(QS36F/'+
                                 %TRIM(WNomFile)+') +
                                 TOFILE(#JMPRDDBS/FINTT014) +
                                 MBROPT(*ADD) FMTOPT(*NOCHK)';
                ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ON-ERROR;
                // WMsgErro = 'XXXXXXX';
             ENDMON;
          ENDFOR;
       ENDSR;
       //======================================================================
       // Rotina.....:
       // Objectivo..:
       //======================================================================
       BEGSR FillManuais;
          //--------------------------------------------------------------
          // Recibos Manuais de Automóvel
          //--------------------------------------------------------------
          FOR WLetra = 1 By 1 TO 15;
             WNomFile = 'GUIM1' + %TRIM(WMes) + WValLetra(WLetra);
             MONITOR;
                WComando = 'CPYF FROMFILE(QS36F/'+
                                 %TRIM(WNomFile)+') +
                                 TOFILE(#JMPRDDBS/FINTT014) +
                                 MBROPT(*ADD) FMTOPT(*NOCHK)';
                ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ON-ERROR;
                // WMsgErro = 'XXXXXXX';
             ENDMON;
          ENDFOR;

          //--------------------------------------------------------------
          // Recibos Manuais de Outros Ramos
          //--------------------------------------------------------------
          FOR WLetra = 1 By 1 TO 15;
             WNomFile = 'GUIM2' + %TRIM(WMes) + WValLetra(WLetra);
             MONITOR;
                WComando = 'CPYF FROMFILE(QS36F/'+
                                 %TRIM(WNomFile)+') +
                                 TOFILE(#JMPRDDBS/FINTT014) +
                                 MBROPT(*ADD) FMTOPT(*NOCHK)';
                ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ON-ERROR;
                // WMsgErro = 'XXXXXXX';
             ENDMON;
          ENDFOR;
       ENDSR;
       //======================================================================
       // Rotina.....:
       // Objectivo..:
       //======================================================================
       BEGSR FixFile;
          OPEN FINTT014;
          READ FINTT014;
          DOW (NOT %EOF(FINTT014));
             MONITOR;
                WFGA = WFGA;
             ON-ERROR;
                WFGA = *ZEROS;
             ENDMON;

             MONITOR;
                UPDATE RINTT014;
             ON-ERROR;
             ENDMON;
             READ FINTT014;
          ENDDO;
          CLOSE FINTT014;
       ENDSR;
      /END-FREE
