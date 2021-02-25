     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PAUDO020
     H* Descrição.: Auditar Ficheiros de Recibos enviados
     H*
     H* CRTPF FILE(QTEMP/FINTT005) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*
     H*===========================================================
     D*==> Lista de Parametros
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Lista de Letras
     D WValLetra       S              2A   DIM(18)
     D
     D*==> Campos de Trabalho
     D WComando        S            300A
     D WMes            S              2S 0
     D WAno            S              4S 0
     D WLetra          S              2S 0
     D WNomFile        S             10A
     D WNomLib         S             10A
     D*===========================================================
      /FREE

       //-------------------------------------------------
       // Eliminar o ficheiro com os interfaces
       //-------------------------------------------------
       MONITOR;
          WComando = 'DLTF FILE(QTEMP/FINTT005)';
          ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
       ON-ERROR;
       // WMsgErro = 'XXXXXXX';
       ENDMON;

       //-------------------------------------------------
       // Criar ficheiro de Interface
       //-------------------------------------------------
       MONITOR;
          WComando = 'CRTPF FILE(QTEMP/FINTT005) SRCFILE(#JMPRDSRC/QDDSSRC) +
                      SIZE(*NOMAX)';
          ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
       ON-ERROR;
       // WMsgErro = 'XXXXXXX';
       ENDMON;

       //-------------------------------------------------
       // Carregar o ficheiro com dados
       //-------------------------------------------------
       WValLetra(1) = 'A ';
       WValLetra(2) = 'B ';
       WValLetra(3) = 'C ';
       WValLetra(4) = 'D ';
       WValLetra(5) = 'E ';
       WValLetra(6) = 'F ';
       WValLetra(7)  = '01';
       WValLetra(8)  = '02';
       WValLetra(9)  = '03';
       WValLetra(10) = '04';
       WValLetra(11) = '05';
       WValLetra(12) = '06';
       WValLetra(13) = '07';
       WValLetra(14) = '08';
       WValLetra(15) = '09';
       WValLetra(16) = '10';
       WValLetra(17) = '11';
       WValLetra(18) = '12';

       FOR WAno = 2008 By 1 TO 2015;
          WNomLib = 'TLUSIT' + %CHAR(WAno);
          FOR WMes = 1 By 1 TO 12;
             FOR WLetra = 1 By 1 TO 18;
                   WNomFile = 'SINAUT' + %TRIM(%EDITW(WMes:'0  ')) +
                              WValLetra(WLetra);
                MONITOR;
                   WComando = 'CPYF FROMFILE('+ %TRIM(WNomLib) + '/' +
                               %TRIM(WNomFile) + ') ' +
                               'TOFILE(QTEMP/FINTT005) ' +
                               'MBROPT(*ADD) FMTOPT(*NOCHK)';
                   ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
                ON-ERROR;
                   // WMsgErro = 'XXXXXXX';
                ENDMON;
             ENDFOR;
          ENDFOR;
       ENDFOR;

       *INLR = *ON;
      /END-FREE
