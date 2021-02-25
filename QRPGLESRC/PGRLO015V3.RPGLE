     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PGRLO015
     H* Descrição.: Escutar a pasta da Lusitania e validar se
     H*             existem ficheiros a processar.
     H*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D
     D*==> Tratamento de Interfaces
     D ReadInterface   PR                  EXTPGM(DynamicCall)
     D   NomFile                     40A   CONST
     D   CdInter                      9S 0 CONST
     D   Accao                        1A   CONST
     D   Resposta                     1A   CONST
     D   MsgErro                      7A   CONST
     D DynamicCall     S             10A   INZ(*BLANKS)
     D WAction         S              1A   INZ(*BLANKS)
     D WResposta       S              1A   INZ(*BLANKS)
     D
     D*==> Lista de Trabalho
     D LST#Pasta       DS                  QUALIFIED DIM(1000)
     D   NomFile                     40A   INZ(*BLANKS)
     D   NomPasta                    40A   INZ(*BLANKS)
     D   IsProcess                    1A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D WIdx            S              5S 0
     D
     D*==> Formato de Registo
     D RTMPT010        DS
     D   WFileName                   40A
     D
     D*==> Campos de Trabalho
     D WNomPasta       S             40A
     D WNomFile        S             40A
     D WLenFile        S              3S 0
     D WCdPrograma     S              5S 0
     D WCdTempl        S              5S 0
     D WNomPastaBk     S             40A
     D WSession        S              5S 0
     D WRecord         S              5S 0
     D WCdInter        S              9S 0
     D WMsgErro        S              7A
     D
     D*==> Declaração de Constantes
     D ACT#Validar     C                   'V'
     D ACT#Processar   C                   'P'
     D*===========================================================
      /FREE
       //---------------------------------------------------
       // Sessão de ficheiros a tratar
       //---------------------------------------------------
       WSession = $SetFiles('FGRLP210 A INNER JOIN FGRLP200 B +
                             ON (A.CDTEMPL = B.CDTEMPL)');
       $SetFields('B.NOPASTA,A.NOMFILE,A.CDPROGR,+
                   A.CODDIRE,A.CDTEMPL,A.CDINTER');
       $SetSort('NOPASTA');

       //---------------------------------------------------
       // Tratar informação de registos a enviar
       //---------------------------------------------------
       WMsgErro = *BLANKS;
       LST#Pasta(1).IsEmpty = *ON;
       EXSR ExecRecebidos; // Tratar Registos Recebidos da Lusitania
       EXSR ExecEnviados;  // Registar registos enviados para a Lusitania

       $RmvDBSession(WSession);
       *INLR = *ON;
       RETURN;

       //=============================================================
       // Subrotina..: ExecRecebidos
       // Descrição..: Escutar pasta da Lusitania e tratar ficheiros
       //              recebidos.
       //=============================================================
       BEGSR ExecRecebidos;
          WNomPasta = *BLANKS;
          WNomPastaBk = *BLANKS;
          $SetCriteria('(A.CODDIRE = ''E'')');
          WRecord = 1;
          DOW ($GetRecord(WRecord) > *ZEROS);
             //----------------------------------------
             // Actualizar conteudo da Pasta
             //----------------------------------------
             WNomPasta = $GetField('B.NOPASTA');
             WCdTempl    = $GetFieldNum('A.CDTEMPL');
             WCdPrograma = $GetFieldNum('A.CDPROGR');
             WCdInter    = $GetFieldNum('A.CDINTER');
             IF (WNomPasta <> WNomPastaBk);
                IF (WNomPastaBk <> *BLANKS);
                   EXSR MoveFiles;
                ENDIF;
                WNomPastaBk = WNomPasta;
                $InzFTPList(WCdTempl);
                WIdx = 1;
                WNomFile = $GetFtpFirst();
                DOW (WNomFile <> *BLANKS);
                   LST#Pasta(WIdx).NomFile   = WNomFile;
                   LST#Pasta(WIdx).NomPasta  = $GetField('B.NOPASTA');
                   LST#Pasta(WIdx).IsProcess = 'N';
                   LST#Pasta(WIdx).IsEmpty   = *OFF;
                   WIdx                     += 1;
                   LST#Pasta(WIdx).IsEmpty   = *ON;
                   WNomFile = $GetFtpNext();
                ENDDO;
             ENDIF;

             //----------------------------------------
             // Processar o ficheiro
             //----------------------------------------
             WNomFile = $GetField('A.NOMFILE');
             IF ($ChkFTPFile(WNomFile));
                EXSR LerInterface;
             ENDIF;
             WRecord += 1;
          ENDDO;
          IF (WNomPastaBk <> *BLANKS);
             EXSR MoveFiles;
          ENDIF;
       ENDSR;

       //=============================================================
       // Subrotina..: LerInterface
       // Descrição..: Ler o interface Recebido da Lustiania.
       //=============================================================
       BEGSR LerInterface;
          WLenFile = %LEN(%TRIM(WNomFile));
          WIdx = 1;
          DOW (NOT LST#Pasta(WIdx).IsEmpty);
             IF (%SUBST(%TRIM(LST#Pasta(Widx).NomFile):1:WLenFile) =
               %Subst(%TRIM(WNomFile):1:WLenFile));

                //----------------------------------------
                // Invocar o programa para o Interface
                //----------------------------------------
                IF (WCdPrograma > *ZEROS);
                   MONITOR;
                      DynamicCall = $GetFunction(WCdPrograma);
                      WAction     = ACT#Validar;
                      WResposta   = *BLANKS;
                      ReadInterface(LST#Pasta(Widx).NomFile:WCdInter:
                                    WAction:WResposta:WMsgErro);
                      IF (WResposta = 'S');
                         WMsgErro = $GetFTP(WCdTempl:LST#Pasta(Widx).NomFile:
                                         'QTEMP':'INTFILE');
                         IF (WMsgErro = *BLANKS);
                            WAction = ACT#Processar;
                            ReadInterface(LST#Pasta(Widx).NomFile:WCdInter:
                                          WAction:WResposta:WMsgErro);
                         ENDIF;
                      ENDIF;
                   ON-ERROR;
                      WMsgErro = 'XXXXXXX';
                   ENDMON;
                ENDIF;

                //----------------------------------------
                // Mover Interface e Actualizar Controlo
                //----------------------------------------
                IF (WMsgErro = *BLANKS);
                   LST#Pasta(Widx).IsProcess = 'S';
                ENDIF;
             ENDIF;
             WIdx += 1;
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
          $SetCriteria('(A.CODDIRE = ''S'')');
          WRecord = 1;
          DOW ($GetRecord(WRecord) > *ZEROS);
             WNomPasta = $GetField('B.NOPASTA');
             IF (WNomPasta <> WNomPastaBk);
                //$ListFTP($GetFieldNum('A.CDTEMPL'));
                WNomPastaBk = WNomPasta;
                // EXSR ListPasta;
             ENDIF;

             //----------------------------------------
             // Processar o ficheiro
             //----------------------------------------
             WNomFile    = $GetField('A.NOMFILE');
             WCdPrograma = $GetFieldNum('A.CDPROGR');
             //EXSR ExecFile;
             WRecord += 1;
          ENDDO;
       ENDSR;

       //=============================================================
       // Subrotina..: MoveFiles
       // Descrição..: Mover ficheiros processados para as respectivas
       //              pastas.
       //=============================================================
       BEGSR MoveFiles;
          WIdx = 1;
          DOW (NOT LST#Pasta(WIdx).IsEmpty);
             IF (LST#Pasta(WIdx).IsProcess = 'N');
                // Mover este ficheiro para Erros
             ENDIF;
             WIdx += 1;
          ENDDO;
       ENDSR;
      /END-FREE
