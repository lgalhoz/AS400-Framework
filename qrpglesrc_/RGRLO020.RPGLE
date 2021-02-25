     H NOMAIN
     H*==================================================================
     H* Descrição.: Grupo de Funções para ilstar ficheiros do AS/400
     H*             existentes nas bibliotecas de produção. E validar se
     H*             algum documento consta num desses ficheiros. Esta
     H*             necessidade deve-se à forma como estão desenvolvidos
     H*             os Sistemas.
     H*             Os resultado não estão a ser obtidos da melhor forma
     H*             no futuro, deverá ser revista para usar API's de
     H*             sistema, em vez de comandos.
     H*==================================================================
     FFH456SX   IF   E             DISK    USROPN EXTFILE('QTEMP/FH456SX')
     FFH458SX   IF   F   10        DISK    USROPN  InfDS(InfDS)
     F                                     EXTFILE('QTEMP/FH458SX')
     F*==================================================================
      /Copy QRPGLESRC,SGRLO015

     D*--> Contar numero de registos
     D InfDS           ds
     D  FileLib               93    102a
     D  NbrRecs              156    159i 0
     D*--> Invocar um comando de AS/400 externo: -----------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*--> Histórico de ficheiros
     D Lst#Hist        DS                  QUALIFIED DIM(KteLimite)
     D   Date                         8S 0 INZ(*ZEROS)
     D   File                        10A   INZ(*BLANKS)
     D   Lib                         10A   INZ(*BLANKS)
     D   Next                         5S 0 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D WFirst          S              5S 0 INZ(*ZEROS)
     D WCursor         S              5S 0 INZ(*ZEROS)
     D WIdx            S              5S 0 INZ(*ZEROS)
     D KteLimite       C                   1000
     D
     D*===================================================================

     P*===================================================================
     P* Processo..: $InzListHS
     P* Descrição.: Inicializar a lista de Trabalho
     P*===================================================================
     P $InzListHS      B                   EXPORT
     D                 PI
      /FREE
         WIdx   = *ZEROS;
         WFirst = *ZEROS;
      /END-FREE
     P $InzListHS      E

     P*===================================================================
     P* Processo..: $FillListHS
     P* Descrição.: Converter uma data num Inteiro
     P*===================================================================
     P $FillListHS     B                   EXPORT
     D                 PI
     D    LibName                    10A   Const
     D    FileName                   10A   Const
     D
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
      /FREE
       //-------------------------------------------------
       // Se excedeu a capacidade da lista então sai
       //-------------------------------------------------
       IF (WIdx = KteLimite);
          RETURN;
       ENDIF;

       //-------------------------------------------------
       // Carregar ficheiro de dados com o inventário
       //-------------------------------------------------
       MONITOR;
          WStdCmd = 'DSPFD FILE('+%TRIM(LibName)+'/' +
                     %TRIM(FileName)+') +
                     TYPE(*BASATR) +
                     OUTPUT(*OUTFILE) OUTFILE(QTEMP/FH456SX)';
          WStdLen = %LEN(WStdCmd);
          Shell(WStdCmd:WStdLen);
       ON-ERROR;
          RETURN;
       ENDMON;

       //-------------------------------------------------
       // Carregar Lista de Ficheiros
       //-------------------------------------------------
       MONITOR;
          OPEN FH456SX;
          READ FH456SX;
          DOW (NOT %EOF() AND (WIdx < KteLimite));
             WIdx += 1;
             Lst#Hist(Widx).Date    = %DEC(ATFCDT:6:0)+20000000;
             Lst#Hist(WIdx).File    = ATFILE;
             Lst#Hist(WIdx).Lib     = ATLIB;
             Lst#Hist(WIdx).Next    = *ZEROS;
             Lst#Hist(WIdx).IsEmpty = *OFF;
             READ FH456SX;
          ENDDO;
          CLOSE FH456SX;
       ON-ERROR;
          RETURN;
       ENDMON;
      /END-FREE
     P $FillListHS     E

     P*===================================================================
     P* Processo..: $GetFirstHS
     P* Descrição.: Obter o primeiro registo da Lista
     P*===================================================================
     P $GetFirstHS     B                   EXPORT
     D                 PI                  LIKEDS(Fil#Info)
      /FREE
         WCursor = WFirst;
         RETURN $GetNextHS;
      /END-FREE
     P $GetFirstHS     E

     P*===================================================================
     P* Processo..: $GetNextHS
     P* Descrição.: Obter o primeiro registo da Lista
     P*===================================================================
     P $GetNextHS      B                   EXPORT
     D                 PI                  LIKEDS(Fil#Info)
      /FREE
         IF (WFirst = *ZEROS);
            $SortHS();
            WCursor = WFirst;
         ENDIF;

         IF (WFirst = *ZEROS);
            RETURN;
         ENDIF;

         Fil#Info.Date = Lst#Hist(WCursor).Date;
         Fil#Info.File = Lst#Hist(WCursor).File;
         Fil#Info.Lib  = Lst#Hist(WCursor).Lib;
         WCursor = Lst#Hist(WCursor).Next;

         RETURN Fil#Info;
      /END-FREE
     P $GetNextHS      E

     P*===================================================================
     P* Processo..: $IsEofHS
     P* Descrição.: Obter o primeiro registo da Lista
     P*===================================================================
     P $EofHS          B                   EXPORT
     D                 PI              N
      /FREE
         IF (WFirst = *ZEROS) AND (WIdx > *ZEROS);
            RETURN *OFF;
         ELSEIF (WCursor > *ZEROS);
             RETURN *OFF;
         ELSE;
            RETURN *ON;
         ENDIF;
      /END-FREE
     P $EofHS          E

     P*===================================================================
     P* Processo..: $SortHS
     P* Descrição.: Obter o primeiro registo da Lista
     P*===================================================================
     P $SortHS         B                   EXPORT
     D                 PI
     D WLastRec        S              5S 0 INZ(*ZEROS)
     D WNextRec        S              5S 0 INZ(*ZEROS)
     D WHighDate       S              8S 0 INZ(*ZEROS)
      /FREE
        WFirst    = *ZEROS;
        WLastRec  = *ZEROS;
        DOU (WNextRec = *ZEROS);
           WHighDate = *ZEROS;
           WNextRec = *ZEROS;
           WIdx = 1;
           DOW (Lst#Hist(Widx).IsEmpty = *OFF) AND (WIdx < KteLimite);
              IF ((Lst#Hist(Widx).Date >= WHighDate) AND (WIdx<>WLastRec)
                 AND (Lst#Hist(Widx).Next = *ZEROS));
                 WHighDate  = Lst#Hist(Widx).Date;
                 WNextRec = WIdx;
              ENDIF;
              WIdx += 1;
           ENDDO;
           IF (WFirst = *ZEROS);
              WFirst  = WNextRec;
              ELSEIF (WNextRec<>WLastRec);
                 Lst#Hist(WLastRec).Next = WNextRec;
           ENDIF;
           WLastRec = WNextRec;
        ENDDO;
      /END-FREE
     P $SortHS         E

     P*===================================================================
     P* Processo..: $FindRec
     P* Descrição.: Procurar um registo num qualquer ficheiro
     P*===================================================================
     P $FindRec        B                   EXPORT
     D                 PI              N
     D    LibName                    10A   Const
     D    FileName                   10A   Const
     D    Chave                      20A   Const
     D    PosInic                     5S 0 Const
     D
     D WResult         S               N
     D WQtdRecords     S              5S 0
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D*==============================================================
      /FREE
        EXSR CrtSaco;
        EXSR CheckRegisto;
        EXSR Find;
        RETURN WResult;

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
              WStdCmd = 'DLTF FILE(QTEMP/TESTE)';
              WStdLen = %LEN(WStdCmd);
              Shell(%TRIM(WStdCmd):WStdLen);
           ON-ERROR;
           ENDMON;

           MONITOR;
              WStdCmd = 'CRTPF FILE(QTEMP/TESTE) RCDLEN(10)';
              WStdLen = %LEN(WStdCmd);
              Shell(%TRIM(WStdCmd):WStdLen);
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
              WStdCmd = 'CPYF FROMFILE('+%TRIM(LibName)+'/'+
                          %TRIM(FileName)+') TOFILE(QTEMP/FH458SX) +
                          MBROPT(*ADD) +
                          INCCHAR(*RCD ' + %CHAR(PosInic) + ' *EQ ''' +
                          %TRIM(Chave) + ''') +
                          FMTOPT(*NOCHK)';
              WStdLen = %LEN(WStdCmd);
              Shell(%TRIM(WStdCmd):WStdLen);
           ON-ERROR;
           ENDMON;
        ENDSR;

        //================================================================
        // Subrotina.: Find
        // Objectivo.: Procurar a existência de um Registo.
        //================================================================
        BEGSR Find;
           MONITOR;
              OPEN FH458SX;
              IF (NbrRecs > WQtdRecords);
                 WQtdRecords = NbrRecs;
                 WResult = *ON;
              ELSE;
                 WResult = *OFF;
              ENDIF;
              CLOSE FH458SX;
           ON-ERROR;
           ENDMON;
        ENDSR;
      /END-FREE
     P $FindRec        E
