     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PAUDO021
     H* Descrição.: Auditar Ficheiros de Recibos enviados
     H*
     H* CRTPF FILE(QTEMP/FINTT001) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*
     H*===========================================================
     D*==> Lista de Parametros
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Lista de Letras
     D WValLetra       S              1A   DIM(6)
     D
     D*==> Campos de Trabalho
     D WComando        S            300A
     D WAno            S              4S 0
     D WMes            S              2S 0
     D WLetra          S              1S 0
     D WNomFile        S             10A
     D*===========================================================
      /FREE

       //-------------------------------------------------
       // Eliminar o ficheiro com os interfaces
       //-------------------------------------------------
       MONITOR;
          WComando = 'DLTF FILE(#JMPRDDBS/FINTT001)';
          ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
       ON-ERROR;
       // WMsgErro = 'XXXXXXX';
       ENDMON;

       //-------------------------------------------------
       // Criar ficheiro de Interface
       //-------------------------------------------------
       MONITOR;
          WComando = 'CRTPF FILE(#JMPRDDBS/FINTT001) +
                      SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)';
          ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
       ON-ERROR;
       // WMsgErro = 'XXXXXXX';
       ENDMON;

       //-------------------------------------------------
       // Carregar o ficheiro com dados
       //-------------------------------------------------
       WValLetra(1) = 'A';
       WValLetra(2) = 'B';
       WValLetra(3) = 'C';
       WValLetra(4) = 'D';
       WValLetra(5) = 'E';
       WValLetra(6) = 'F';

       FOR WAno = 2013 By 1 TO 2016;
       FOR WMes = 1 By 1 TO 12;
          FOR WLetra = 1 By 1 TO 6;
             WNomFile = 'APOL' + %TRIM(%EDITW(WMes:'0  ')) +
                        WValLetra(WLetra);
             MONITOR;
                WComando = 'CPYF FROMFILE(TLUSIT'+%CHAR(WAno)+'/'+
                                 %TRIM(WNomFile)+') +
                                 TOFILE(#JMPRDDBS/FINTT001) +
                                 MBROPT(*ADD) FMTOPT(*NOCHK)';
                ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ON-ERROR;
                // WMsgErro = 'XXXXXXX';
             ENDMON;
          ENDFOR;
       ENDFOR;
       ENDFOR;

       *INLR = *ON;
      /END-FREE
