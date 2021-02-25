     H NOMAIN
     H*=========================================================
     H* Modulo....: RDBSO012
     H* Descrição.: Conjunto de funções para gerir o Controlo de
     H*             de consolidações da Base de Dados.
     H*
     H*=========================================================
     FQADSPJRN  IF   E           K DISK    USROPN
     F*=========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO012

     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Obter O Estado de Consolidações
     D QTNRCMTI        PR                  ExtPgm('QTNRCMTI')
     D   rcvvar                   65535a   options(*varsize)
     D   rcvvarlen                   10i 0 const
     D   format                       8a   const
     D   errorCode                32767a   options(*varsize)
     D
     D ErrorCode       ds                  qualified
     D   bytesProv                   10i 0 inz(0)
     D   bytesAvail                  10i 0 inz(0)
     D
     D ResultSize      S             10i 0 inz(%size(Result))
     D Result          DS                  qualified
     D                                     based(p_Result)
     D   bytesrtn                    10i 0
     D   bytesavail                  10i 0
     D   Tudo                       110A   overlay(Result:1)
     D   Status                       1A   overlay(Result:9)
     D   Ambito                       1A   overlay(Result:20)
     D   Diario                      10A   overlay(Result:26)
     D   Biblioteca                  10A   overlay(Result:36)
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D*================================================================

     P*=========================================================
     P* Processo..: $getCMTStatus
     P* Objectivo.: Obter o estado do Controlo de Consolidações
     P*=========================================================
     P $getCMTStatus   B                   EXPORT
     D                 PI              N
     D
     D ResultSize      S             10i 0 inz(%size(Result))
     D Result          DS                  qualified
     D                                     based(p_Result)
     D   bytesrtn                    10i 0
     D   bytesavail                  10i 0
     D   Tudo                       110A   overlay(Result:1)
     D   Status                       1A   overlay(Result:9)
     D   Ambito                       1A   overlay(Result:20)
     D   Diario                      10A   overlay(Result:26)
     D   Biblioteca                  10A   overlay(Result:36)
     D   WResult       S               N
     D*=========================================================
      /FREE
         // ------------------------------------------------
         //  Invocar a API para Obter as Entradas do Diario
         //  Para o Job Activo.
         // ------------------------------------------------
         p_Result = %alloc(ResultSize);
         QTNRCMTI(Result:ResultSize:'CMTI0100':ErrorCode);
         IF (ResultSize < Result.BytesAvail);
            ResultSize = Result.BytesAvail;
            p_Result = %realloc(p_Result: ResultSize);
         ENDIF;

         IF (Result.Status = 'A');
            return *ON; // Activo
         ELSE;
            return *OFF; // Desactivo
         ENDIF;
      /END-FREE
     P $getCMTStatus   E

     P*=========================================================
     P* Processo..: $getCMTAmbito
     P* Objectivo.: Obter o Ambito do Controlo de Consolidações
     P*=========================================================
     P $getCMTAmbito   B                   EXPORT
     D                 PI             8A
     D   WResult       S              8A
      /FREE
         // ------------------------------------------------
         //  Invocar a API para determinar o estado do
         //  controlo de Consolidações.
         // ------------------------------------------------
         p_Result = %alloc(ResultSize);
         QTNRCMTI(Result:ResultSize:'CMTI0100':ErrorCode);
         IF (ResultSize < Result.BytesAvail);
            ResultSize = Result.BytesAvail;
            p_Result = %realloc(p_Result: ResultSize);
         ENDIF;

         IF (Result.Ambito = 'J');
            return '*JOB';
         ELSEIF (Result.Ambito = 'A');
            return '*ACTGRP';
         ELSE;
            return *BLANKS;
         ENDIF;
      /END-FREE
     P $getCMTAmbito   E

     P*=========================================================
     P* Processo..: $endCommitment
     P* Objectivo.: Terminar Controlo de Consolidações
     P*=========================================================
     P $endCommitment  B                   EXPORT
     D                 PI              N
      /FREE
        MONITOR;
           WStdCmd = 'ENDCMTCTL';
           WStdLen = %LEN(WStdCmd);
           EXEC(WStdCmd:WStdLen);
           RETURN *ON;
        ON-ERROR;
           RETURN *OFF; // Ocorreream erros
        ENDMON;
      /END-FREE
     P $endCommitment  E

     P*=========================================================
     P* Processo..: $strCommitment
     P* Objectivo.: Iniciar o Controlo de Consolidações
     P*=========================================================
     P $strCommitment  B                   EXPORT
     D                 PI              N
     D     Ambito                     8A   Const Options(*NOPASS)
     D     Diario                    10A   Const Options(*NOPASS)
     D     Biblioteca                10A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WAmbito         S              8A   INZ('*JOB')
     D WDiario         S             10A   INZ('JMDBS')
     D WBiblioteca     S             10A   INZ('#JMPRDDBS')
     D*=========================================================
      /FREE
        WAmbito     = '*JOB';
        IF (%PARMS() > *ZEROS);
           WAmbito     = Ambito;
           IF (%PARMS() > 1);
              WDiario     = Diario;
              IF (%PARMS() > 2);
                 WBiblioteca = Biblioteca;
              ENDIF;
           ENDIF;
        ENDIF;

        MONITOR;
           WStdCmd = 'STRCMTCTL LCKLVL(*ALL) +
                               CMTSCOPE('+%TRIM(WAmbito)+') +
                               DFTJRN('+%TRIM(WBiblioteca)+'/'+
                                        %TRIM(WDiario)+')  +
                               OMTJRNE(*LUWID)';
           WStdLen = %LEN(WStdCmd);
           EXEC(WStdCmd:WStdLen);
           RETURN *ON;
        ON-ERROR;
           RETURN *OFF; // Ocorreream erros
        ENDMON;
      /END-FREE
     P $strCommitment  E

     P*=========================================================
     P* Processo..: $isCMTPending
     P* Objectivo.: Validae se Existem Transacções pendentes
     P*             para este efeito, vai listas as entradas
     P*             para o trabalho activo e vai validar se
     P*             encontra o fecho de uma trasacção.
     P*
     P* Observação: O código foi alterado de forma a tornar
     P*             a pesquisa da entrada mais célere.
     P*=========================================================
     P $isCMTPending   B                   EXPORT
     D                 PI              N
     D
     D*==> Obter Entradas do Diário (desenvolver quando tiver tempo)
     D*RTVJRNE         PR                  ExtPgm('QjoRetrieveJournalEntries')
     D*  rcvvar                   65535a   options(*varsize)
     D*  rcvvarlen                   10i 0 const
     D*  Diario                      20a   const
     D*  Formato                      8a   const
     D*  Resultado                32767a   options(*varsize)
     D*  ErrorCode                32767a   options(*varsize)
     D
     D WIsSaved        S               N
     D WStsOpera       S              1A
     D*=========================================================
      /FREE
        //----------------------------------------------
        // listar Entradas para o Job
        //----------------------------------------------
        WIsSaved = *OFF;
        IN *LOCK LDA#;
        IF (L#JNRENT = *ZEROS);
           L#JNRENT = 1;
        ENDIF;
        MONITOR;
           WStdCmd = 'DSPJRN JRN(#JMPRDDBS/JMDBS) +
                      FROMENTLRG('+%CHAR(L#JNRENT)+') +
                      JOB('+%CHAR(S#JOB#)+'/'+%TRIM(S#USR)+'/'+
                      %TRIM(S#JOB)+') +
                      OUTPUT(*OUTFILE) +
                      OUTFILE(QTEMP/JRNSTS) +
                      OUTMBR(*FIRST *REPLACE)';
           WStdLen = %LEN(WStdCmd);
           EXEC(WStdCmd:WStdLen);
        ON-ERROR;
           RETURN *OFF; // Ocorreream erros
        ENDMON;

        //----------------------------------------------
        // Abrir Ficheiro
        //----------------------------------------------
        MONITOR;
           WStdCmd = 'OVRDBF FILE(QADSPJRN) TOFILE(QTEMP/JRNSTS)';
           WStdLen = %LEN(WStdCmd);
           EXEC(WStdCmd:WStdLen);
        ON-ERROR;
           RETURN *OFF; // Ocorreream erros
        ENDMON;

        WStsOpera = *BLANKS;
        OPEN QADSPJRN;
        READ QADSPJRN;
        DOW NOT (%EOF());
           //----------------------------------
           // Guardar primeira entrada do Job
           //----------------------------------
           IF (NOT WIsSaved);
              L#JNRENT  = JOSEQN;
              WIsSaved = *ON;
           ENDIF;

           //----------------------------------
           // Validar o estado da Transacção
           //----------------------------------
           IF (JOENTT = 'SC');
              WStsOpera = 'S';
           ELSEIF (JOENTT = 'CM') OR (JOENTT = 'RB');
              L#JNRENT  = JOSEQN;
              WStsOpera = 'C';
           ENDIF;
           READ QADSPJRN;
        ENDDO;
        CLOSE QADSPJRN;
        OUT LDA#;
        UNLOCK LDA#;

        IF (WStsOpera = 'S');
           RETURN *ON;
        ELSE;
           RETURN *OFF;
        ENDIF;
      /END-FREE
     P $isCMTPending   E
