     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PGRLO015
     H* Descrição.: Escutar a pasta da CMPSeguro e validar se
     H*             existem ficheiros a processar.
     H*===========================================================
     FFGRLP050  UF   E           K DISK    PREFIX(SEC_)
     FFTMPT010  IF   F   40        DISK    USROPN
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Tratamento de Interfaces
     D ReadInterface   PR                  EXTPGM(DynamicCall)
     D   NomFile                     20A   CONST
     D   NomPasta                    40A   CONST
     D   MsgErro                      7A   CONST
     D DynamicCall     S             10A   INZ(*BLANKS)
     D
     D*==> Lista de Trabalho
     D LST#Pasta       DS                  QUALIFIED DIM(100)
     D    NomFile                    40A   INZ(*BLANKS)
     D    NomPasta                   40A   INZ(*BLANKS)
     D    IsProcess                   1A   INZ(*BLANKS)
     D    IsEmpty                      N   INZ(*ON)
     D
     D*==> Formato de Registo
     D RTMPT010        DS
     D   WFileName                   40A
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D WNomPasta       S             40A
     D WNomFile        S             20A
     D WLenFile        S              3S 0
     D WCdPrograma     S              5S 0
     D WNomPastaBk     S             40A
     D WSession        S              5S 0
     D WRecord         S              5S 0
     D WIdx            S              5S 0
     D WMsgErro        S              7A
     D*===========================================================
      /FREE
       //---------------------------------------------------
       // Sessão de ficheiros a tratar
       //---------------------------------------------------
       WSession = $SetFiles('FGRLP050');
       $SetSort('NOMPAST');

       //---------------------------------------------------
       // Tratar informação de registos a enviar
       //---------------------------------------------------
       WMsgErro = *BLANKS;
       LST#Pasta(1).IsEmpty = *ON;
       EXSR ExecRecebidos; // Tratar Registos Recebidos da CMPSeguro
       EXSR ExecEnviados;  // Registar registos enviados para a CMPSeguro

       //----------------------------------------------
       // Mover Interfaces desconhecidos
       //----------------------------------------------
       IF (NOT LST#Pasta(1).IsEmpty);
          EXSR MoveEstranhos;
          LST#Pasta(1).IsEmpty = *ON;
       ENDIF;

       $RmvDBSession(WSession);
       *INLR = *ON;
       RETURN;

       //=============================================================
       // Subrotina..: ListPasta
       // Descrição..: Listar ficheiros existentes na Pasta
       //=============================================================
       BEGSR ListPasta;
          //----------------------------------------------
          // Mover Interfaces desconhecidos
          //----------------------------------------------
          IF (NOT LST#Pasta(1).IsEmpty);
             EXSR MoveEstranhos;
             LST#Pasta(1).IsEmpty = *ON;
          ENDIF;

          //----------------------------------------------
          // Eliminar Ficheiro de Indice
          //----------------------------------------------
          MONITOR;
             WStdCmd = 'DLTF FILE(QTEMP/FTMPT010)';
             WStdLen = %LEN(WStdCmd);
             Exec(WStdCmd:WStdLen);
          ON-ERROR;
          ENDMON;

          //----------------------------------------------
          // Criar Ficheiro de Indice
          //----------------------------------------------
          MONITOR;
             WStdCmd = 'CRTPF FILE(QTEMP/FTMPT010) RCDLEN(40)';
             WStdLen = %LEN(WStdCmd);
             Exec(WStdCmd:WStdLen);
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
             LEAVESR;
          ENDMON;

          //----------------------------------------------
          // Listar Conteudo da Pasta
          //----------------------------------------------
          MONITOR;
             WStdCmd = 'QSH CMD(''ls '+%TRIM(WNomPasta)+
                        '> /interfaces/indice.txt'')';
             WStdLen = %LEN(WStdCmd);
             Exec(WStdCmd:WStdLen);
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
             LEAVESR;
          ENDMON;

          //----------------------------------------------
          // Copiar Conteudo da Pasta para o AS400
          //----------------------------------------------
          MONITOR;
             WStdCmd = 'CPYFRMSTMF FROMSTMF(''/interfaces/indice.txt'') +
                     TOMBR(''/qsys.lib/qtemp.lib/FTMPT010.FILE/FTMPT010.MBR'') +
                     MBROPT(*REPLACE)';
             WStdLen = %LEN(WStdCmd);
             Exec(WStdCmd:WStdLen);
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
             LEAVESR;
          ENDMON;

          //----------------------------------------------
          // Eliminar ficheiro de Indice
          //----------------------------------------------
          MONITOR;
             WStdCmd = 'QSH CMD(''rm /interfaces/indice.txt'')';
             WStdLen = %LEN(WStdCmd);
             Exec(WStdCmd:WStdLen);
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
             LEAVESR;
          ENDMON;

          //----------------------------------------------
          // Actualizar lista de Trabalho
          //----------------------------------------------
          MONITOR;
             WIdx = 1;
             OPEN FTMPT010;
             READ FTMPT010 RTMPT010;
             DOW (NOT %EOF(FTMPT010));
                LST#Pasta(WIdx).NomFile   = WFileName;
                LST#Pasta(WIdx).NomPasta  = WNomPasta;
                LST#Pasta(WIdx).IsProcess = 'N';
                LST#Pasta(WIdx).IsEmpty   = *OFF;
                WIdx += 1;
                LST#Pasta(WIdx).IsEmpty   = *ON;
                READ FTMPT010 RTMPT010;
             ENDDO;
             CLOSE FTMPT010;
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
             LEAVESR;
          ENDMON;
       ENDSR;

       //=============================================================
       // Subrotina..: ExecRecebidos
       // Descrição..: Escutar pasta da CMPSeguro e tratar ficheiros
       //              recebidos.
       //=============================================================
       BEGSR ExecRecebidos;
          WNomPasta = *BLANKS;
          WNomPastaBk = *BLANKS;
          $SetCriteria('(CODDIRE = ''E'')');
          WRecord = 1;
          DOW ($GetRecord(WRecord) > *ZEROS);
             //----------------------------------------
             // Actualizar conteudo da Pasta
             //----------------------------------------
             WNomPasta = $GetField('NOMPAST');
             IF (WNomPasta <> WNomPastaBk);
                WNomPastaBk = WNomPasta;
                EXSR ListPasta;
             ENDIF;

             //----------------------------------------
             // Processar o ficheiro
             //----------------------------------------
             WNomFile = $GetField('NOMFILE');
             EXSR ExecFile;
             WRecord += 1;
          ENDDO;
       ENDSR;

       //=============================================================
       // Subrotina..: ExecEnviados
       // Descrição..: Inventariar Interfaces a enviar e ver se
       //              estão a ser enviados na periodicidade prevista.
       //=============================================================
       BEGSR ExecEnviados;
          WNomPasta = *BLANKS;
          WNomPastaBk = *BLANKS;
          $SetCriteria('(CODDIRE = ''S'')');
          WRecord = 1;
          DOW ($GetRecord(WRecord) > *ZEROS);
             WNomPasta = $GetField('NOMPAST');
             IF (WNomPasta <> WNomPastaBk);
                WNomPastaBk = WNomPasta;
                EXSR ListPasta;
             ENDIF;

             //----------------------------------------
             // Processar o ficheiro
             //----------------------------------------
             WNomFile    = $GetField('NOMFILE');
             WCdPrograma = $GetFieldNum('CDPROGR');
             EXSR ExecFile;
             WRecord += 1;
          ENDDO;
       ENDSR;

       //=============================================================
       // Subrotina..: ExecFile
       // Descrição..: Processar o ficheiro e trabalhar de acordo com
       //              o seu conteudo.
       //=============================================================
       BEGSR ExecFile;
          WLenFile = %LEN(%TRIM(WNomFile));
          WIdx = 1;
          DOW (NOT LST#Pasta(WIdx).IsEmpty);
             IF (%Subst(LST#Pasta(Widx).NomFile:1:WLenFile) =
                  %Subst(WNomFile:1:WLenFile));
                MONITOR;
                   DynamicCall = 'sss'; // Obter nome do Programa
                   ReadInterface(WnomFile:WNomPasta:WMsgErro);
                   IF (WMsgErro = *BLANKS);
                      // Invocar Rotina de tratamento
                      LST#Pasta(Widx).IsProcess = 'S';
                   ENDIF;
                ON-ERROR;
                ENDMON;
             ENDIF;
             WIdx += 1;
          ENDDO;
       ENDSR;

       //=============================================================
       // Subrotina..: MoveEstranhos
       // Descrição..: Mover ficheiros desconhecidos ou não registados
       //=============================================================
       BEGSR MoveEstranhos;
          WIdx = 1;
          DOW (NOT LST#Pasta(WIdx).IsEmpty);
             IF (LST#Pasta(WIdx).IsProcess = 'N');
                // Mover este ficheiro para Erros
             ENDIF;
             WIdx += 1;
          ENDDO;
       ENDSR;
      /END-FREE
