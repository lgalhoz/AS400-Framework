     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     FFTMPP001  IF   E           K DISK    PREFIX(GUI_)
     FGBASEDIA  UF   E           K DISK    PREFIX(OUT_)
     F                                     EXTFILE('QS36F/GBASEDIA')
     F*===========================================================
     D
     D*---------------------------------------------
     D* Invocar um comando de AS/400 externo:
     D*---------------------------------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WLibProd        S             10A   INZ('QS36F')
     C*===================================================================
      /FREE

         EXSR FillDiario; // Colocar Apólices Auto a Alterar

         *INLR = *ON;
         RETURN;

         //==============================================================
         // Subrotina..: FillDiario
         // Objectivo..: Adicionar registo ao Diário de Automóvel
         //==============================================================
         BEGSR FillDiario;
            WLibProd = 'QS36F';
            SETLL *LOVAL FTMPP001;
            READ FTMPP001;
            DOW (NOT %EOF);
               CHAIN(N) (GUI_RAMO:GUI_APOLICE) GBASEDIA;
               IF (NOT %FOUND());
                  MONITOR;
                     WStdCmd = 'CPYF FROMFILE(QS36F/GBASEGRA) TOFILE('+
                                %TRIM(WLibProd)+'/GBASEDIA) MBROPT(*ADD) +
                                INCREL((*IF RAMO *EQ 50) (*AND APOL *EQ ' +
                                %CHAR(GUI_APOLICE) + '))';
                     WStdLen = %LEN(WStdCmd);
                     shell(WStdCmd:WStdLen);
                  ON-ERROR;
                  ENDMON;
               ENDIF;
               READ FTMPP001;
            ENDDO;
         ENDSR;
      /END-FREE
