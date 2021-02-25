     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     FFTMPP003  IF   E           K DISK    PREFIX(GUI_)
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
            SETLL *LOVAL FTMPP003;
            READ FTMPP003;
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

               MONITOR;
                  CHAIN (GUI_RAMO:GUI_APOLICE) GBASEDIA;
                  IF (%FOUND());
                     OUT_DSCESP = GUI_DESCONT;
                     OUT_TB     = GUI_BONUS;
                     OUT_VIAG   = GUI_VIAGEM;
                     OUT_TALT = 2;
                     OUT_PJUR   = GUI_PJURID;
                     IF (OUT_PJUR = 'S') AND (OUT_DTINPJ = 0);
                        OUT_DTINPJ = 010115;
                     ENDIF;
                     IF (OUT_PJUR = 'S') AND (OUT_PRSPJ = 0);
                        OUT_PRSPJ  = 6,66;
                     ENDIF;
                     UPDATE BASEDIFA %FIELDS(OUT_DSCESP:OUT_TB:OUT_VIAG:
                                  OUT_TALT:OUT_PJUR:OUT_DTINPJ:OUT_PRSPJ);
                  ENDIF;
               ON-ERROR;
               ENDMON;
               READ FTMPP003;
            ENDDO;
         ENDSR;
      /END-FREE
