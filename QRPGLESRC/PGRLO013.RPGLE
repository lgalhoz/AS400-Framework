     H ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=========================================================
     H* Modulo....: PPRDO013
     H* Descrição.: Localizar programas com o controlo de
     H*             consolidações Activa e proceder à sua
     H*             terminação.
     H*=========================================================
     FFLCKJRN   IF   F   60        DISK    USROPN
     F*=========================================================
     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D WIcErro         S               N
     D*================================================================
     IFLCKJRN   NS
     I                                  1    3  SPACE01
     I                                  4   13  JOBNAME
     I                                 14   15  SPACE02
     I                                 15   25  USRNAME
     I                                 26   27  SPACE03
     I                                 28   33  JOB#NBR
     I*================================================================
      /FREE
       EXSR FillLckFile;
       IF (NOT WIcErro);
          EXSR EndJob;
       ENDIF;

        *INLR = *ON;
        RETURN;

       //======================================================================
       // Subrotina.: FillLckFile
       // Objectivo.: Actualizar Temporário com Trabalhos activos a terminar
       //
       //======================================================================
       BEGSR FillLckfile;
          //-------------------------------------------
          // listar Job's  com STRCMTCTL activo
          //-------------------------------------------
          MONITOR;
             WStdCmd = 'WRKOBJLCK OBJ(#JMPRDDBS/JMDBS) +
                        OBJTYPE(*JRN) OUTPUT(*PRINT)';
             WStdLen = %LEN(WStdCmd);
             EXEC(WStdCmd:WStdLen);
             WIcErro = *OFF;
          ON-ERROR;
             WIcErro = *ON;
             LEAVESR;
          ENDMON;

          //-------------------------------------------
          // Criar Ficheiro de trabalho e encher
          //-------------------------------------------
          MONITOR;
             WStdCmd = 'CRTPF FILE(QTEMP/FLCKJRN) RCDLEN(60)';
             WStdLen = %LEN(WStdCmd);
             EXEC(WStdCmd:WStdLen);
          ON-ERROR;
          ENDMON;

          MONITOR;
             WStdCmd = 'CPYSPLF FILE(QPDSPOLK) TOFILE(QTEMP/FLCKJRN) +
                        SPLNBR(*LAST)';
             WStdLen = %LEN(WStdCmd);
             EXEC(WStdCmd:WStdLen);
             WIcErro = *OFF;
          ON-ERROR;
             WIcErro = *ON;
          ENDMON;
       ENDSR;

       //======================================================================
       // Subrotina.: EndJob
       // Objectivo.: Proceder é terminação dos Trabalhos
       //
       //======================================================================
       BEGSR EndJob;
          OPEN FLCKJRN;
          READ FLCKJRN;
          DOW (NOT %EOF());
             IF (SPACE01 = *BLANKS);
             MONITOR;
                WStdCmd = 'ENDJOB JOB('+%TRIM(JOB#NBR)+'/'+
                                        %TRIM(USRNAME)+'/'+
                                        %TRIM(JOBNAME)+') +
                                       OPTION(*IMMED)';
                WStdLen = %LEN(WStdCmd);
                EXEC(WStdCmd:WStdLen);
             ON-ERROR;
             ENDMON;
             ENDIF;
             READ FLCKJRN;
          ENDDO;
          CLOSE FLCKJRN;
       ENDSR;
      /END-FREE
