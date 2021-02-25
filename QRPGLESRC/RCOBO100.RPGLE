     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RCOBO100
     H* Descrição.: Gerar Ficheiro de Cabeçalho e Actualizar IDD's
     H*
     H*             Criada a constante DAT#Exception, para prever a data
     H*             do sistema forçada.
     H*==================================================================
     FGCOBGR    IF   E           K DISK    PREFIX(REC_)
     FTBIDDEF   UF A E           K DISK
     FTBIDDPF   UF   E           K DISK
     FTBIDDPL1  IF   E           K DISK    RENAME(TBIDDP:TBIDDL)
     FFCOBP220  UF   E           K DISK
     FFCOBP22002IF   E           K DISK    RENAME(RCOBP220:RCOBP22002)
     FFCOBP221  IF   E           K DISK
     F*=========================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Chamada Dinâmica a funções
     D CopyFile        PR                  EXTPGM('RCOBO110')
     D   FileCode                    10P 0 Const Options(*NOPASS)
     D   MsgError                     7A   Const Options(*NOPASS)
     D
     D*==> Chamada Dinâmica a funções
     D CallDynamic     PR                  EXTPGM(WDynamic)
     D   CodFileAct                  10P 0 Const
     D   NumNIB                      21S 0 Const
     D   MnteRecibo                  13S 2 Const
     D   CodRecibo                    9S 0 Const
     D   CodADC                      11S 0 Const
     D   ADCSepa                     12A   Const
     D   SitADC                       1A   Const
     D   DatADC                       8S 0 Const
     D   CodSegura                    8S 0 Const
     D   NuseqSaved                   5S 0 Const
     D   DatCobranca                  8S 0 Const
     D   MsgErro                      7A   Const
     D   CodFileAnt                  10P 0 Const Options(*NOPASS)
     D   QtdeRegist                   6S 0 Const Options(*NOPASS)
     D   MnteRegist                  13S 2 Const Options(*NOPASS)
     D WDynamic        S             10A
     D
     D*==> Estrutura do Nome do Ficheiro
     D                 DS
     D  WCodFile               1     10S 0
     D  WDatfile               1      8S 0
     D  WSeqFile               9     10S 0
     D
     D*==> Campos de Trabalho
     D  PNumRecibo     S              9S 0
     D  WNumRecibo     S              9S 0
     D
     D WNuseqSaved     S              5S 0
     D WMsgErro        S              7A
     D WTipInterface   S              3A
     D WDatSelect      S              8S 0
     D WDatActual      S              8S 0
     D WIcSelect       S               N
     D WCodADC         S              9S 0
     D WADCSepa        S             12A
     D WSitADC         S              1A
     D WSavSequ        S              2S 0
     D WSavCodFile     S             10S 0
     D WQtdeRegis      S              4S 0
     D WMnteRegis      S             13S 2
     D
     D*==> Definição de constantes
     D IDD#Aprovado    C                   '2'
     D IDD#Enviado     C                   '3'
     D KLimMax         C                   1
     D ADC#Regista     C                   '1'
     D ADC#Enviada     C                   '2'
     D ADC#SemEfeito   C                   '3'
     D ADC#Pendente    C                   '4'
     D ADC#Cancela     C                   '5'
     D ADC#Alterada    C                   '6'
     D ADC#AProvada    C                   '7'
     D DAT#Exception   C                   20160104
     D*=========================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WMsgErro
     C                   PARM                    PNumRecibo
     C
      /FREE
         WMsgErro = *BLANKS;
         MONITOR;
            WNumRecibo = PNumRecibo;
         ON-ERROR;
            WNumRecibo = *ZEROS;
         ENDMON;

         WTipInterface = 'XML';
         IF (WTipInterface = 'TXT');
            WDynamic = 'RCOBO101';
         ELSE;
            WDynamic = 'RCOBO102';
         ENDIF;

         WQtdeRegis    = *ZEROS;
         WMnteRegis    = *ZEROS;
         WDatActual    = $DateToInt(%DATE(UDATE));
         WIcSelect     = *OFF;
         WDatSelect    = *ZEROS;

         //--------------------------------------------
         // Gerar mais do que um recibo no interface
         //--------------------------------------------
         IF (WNumRecibo = *ZEROS);
            SETLL (WDatActual) TBIDDL;
            READE (WDatActual) TBIDDL;
            DOW (NOT %EOF(TBIDDPL1));
               EXSR Processar;
               IF (WQtdeRegis >= KLimMax);
                  LEAVE;
               ENDIF;
               READE (WDatActual) TBIDDL;
            ENDDO;
         ELSE;
            CHAIN(N) (WNumRecibo) TBIDDP;
            IF (%FOUND(TBIDDPF));
               //---------------------------------------------
               // Código para prever a data de sitema forçada
               // pela informática após o fecho do ano.
               //---------------------------------------------
               IF (WDatActual = DAT#Exception);
                  WDatActual = IDDATA;
               ENDIF;

               IF (IDDATA = WDatActual);
                  EXSR Processar;
               ELSE;
                  WMsgErro = 'COB0104'; // Não está na data
               ENDIF;
            ELSE;
               WMsgErro = 'COB0105'; // Não encontrou o recibo
               RETURN;
            ENDIF;
         ENDIF;

         //--------------------------------------------
         // Se encontrou registo então gravar resumo
         //--------------------------------------------
         IF (WIcSelect) AND (WQtdeRegis > *ZEROS);
            CallDynamic(WCodFile:*ZEROS:*ZEROS:*ZEROS:*ZEROS:*BLANKS:*BLANKS
                        :*ZEROS:*ZEROS:WNuseqSaved:*ZEROS:WMsgErro:WSavCodFile:
                        WQtdeRegis:WMnteRegis);
            CHAIN (WCodFile) TBIDDEF;
            IF (%FOUND(TBIDDEF));
                TETREG = WQtdeRegis; // Quantidade Enviado
                TEVALR = WMnteRegis;
                UPDATE TBIDDE %FIELDS(TETREG:TEVALR);
                EXSR SendToShare;
            ENDIF;
         ELSEIF (WIcSelect AND (WQtdeRegis = *ZEROS));
            CHAIN (WCodFile) TBIDDEF;
            IF (%FOUND(TBIDDEF));
               DELETE TBIDDEF;
            ENDIF;
         ENDIF;

         *INLR = *ON;
         RETURN;

        //=============================================================
        // Subrotina.: Processar
        // Objectivo.: Procesar a iformação do registo
        //=============================================================
        BEGSR Processar;
           IF (IDSIT = IDD#Aprovado) AND (IDDATA = WDatActual);
              IF (NOT WIcSelect); //**** AND (WDatActual >= IDDATA);
                 WDatSelect = IDDATA;
                 WIcSelect = *ON;
                 EXSR GetCodFile;
              ENDIF;

              CHAIN (IDREC) GCOBGR;
              CHAIN (IDNADC:REC_GCRAMO:REC_GCAPOL) RCOBP22002;
              IF (%FOUND(FCOBP22002));
                 WCodADC  = CDNADC;
                 WSitADC  = SITADC;
                 WADCSepa = ADCSEPA;
                 IF (WSitADC = ADC#Enviada) OR (WSitADC = ADC#SemEfeito) OR
                   (WSitADC = ADC#Cancela);
                    WMsgErro = 'COB0103';
                    LEAVESR;
                 ENDIF;

                 //--------------------------------------------------
                 // Validar as datas da Intrução
                 //--------------------------------------------------
                 IF ((WSitADC = ADC#AProvada) AND
                      (IDDTPP < $AddToDate(IDDATA:3:'U')))
                   OR ((WSitADC <> ADC#AProvada) AND
                      (IDDTPP < $AddToDate(IDDATA:6:'U')));
                    WMsgErro = 'COB0120';
                    LEAVESR;
                 ENDIF;

                 CHAIN (WCodADC:CDNIB) RCOBP221;
                 IF (%FOUND(FCOBP221));
                    CallDynamic(WCodFile:IDNIB:IDVAL:IDREC:IDNADC:WADCSepa
                       :WSitADC:DATALT:IDNSEG:WNuseqSaved:IDDTPP
                       :WMsgErro:WSavCodFile);
                    IF (WMsgErro = *BLANKS);
                       CHAIN (IDREC) TBIDDP;
                       IDFENV = WCodFile;
                       IDSIT  = IDD#Enviado;
                       WQtdeRegis   += 1;
                       WMnteRegis   += IDVAL;
                       UPDATE TBIDDP %FIELDS(IDFENV:IDSIT);

                       IF (WSitADC = ADC#Regista) OR (WSitADC = ADC#Pendente)
                         OR (WSitADC = ADC#Alterada);
                          CHAIN (WCodADC) RCOBP220;
                          IF (%FOUND(FCOBP220));
                             SITADC = ADC#Enviada;
                             UPDATE RCOBP220 %FIELDS(SITADC);
                          ENDIF;
                       ENDIF;
                    ENDIF;

                    //--------------------------------------
                    // Limitar os registos por XML
                    //--------------------------------------
                    IF (WQtdeRegis >= KLimMax);
                       LEAVESR;
                    ENDIF;
                 ELSE;
                    WMsgErro = 'COB0244'; // Não encontrou a ADC
                    LEAVESR;
                 ENDIF;
              ENDIF;
           ENDIF;
        ENDSR;

       //=============================================================
       // Subrotina.: GetCodFile
       // Objectivo.: Obter o Código para o ficheiro
       //=============================================================
       BEGSR GetCodFile;
          //--------------------------------------------
          // Localizar o último registo
          //--------------------------------------------
          WDatFile = WDatSelect;
          WSeqFile = 1;
          CHAIN (WCodFile) TBIDDE;
          DOW (%FOUND(TBIDDEF));
             WSeqFile += 1;
             CHAIN (WCodFile) TBIDDE;
          ENDDO;
          WDatFile = WDatSelect;

          //----------------------------------------------
          // Actualizar informação do Ficheiro
          //----------------------------------------------
          TEFENV = WCodFile;   // Código do Ficheiro
          TEDATE = $cvtData(WDatSelect:'YYMD':'DMYY'); // Data do registo
          TEDATR = *ZEROS;     // Data transferência
          TEDATP = *ZEROS;     // Data do Pagamento
          TETREG = WQtdeRegis; // Quantidade Enviado
          TEREGA = *ZEROS;     // Quantidade Aceite
          TEVALR = *ZEROS;     // Montante Enviado
          TEVALP = *ZEROS;     // Montante Pago
          TETIPF = 'XML';      // Formato do Ficheiro
          WRITE TBIDDE;
       ENDSR;

       //=============================================================
       // Subrotina.: SendToShare
       // Objectivo.: Colocar o ficheiro no Share
       //=============================================================
       BEGSR SendToShare;
          CopyFile(WCodFile:WMsgErro);
          IF (WMsgErro <> *BLANKS);
             $ShowDialog(WMsgErro);
          ENDIF;
       ENDSR;
      /END-FREE
