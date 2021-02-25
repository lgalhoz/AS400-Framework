     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*==================================================================
     H* Programa..: RCOBO130
     H* Descrição.: Este programa substitui o CLP, com o acréscimo de novas
     H*             funcionalidade para validação e actualização do estado
     H*             da ADC.
     H*==================================================================
     FFCOBP220  UF   E           K DISK    PREFIX(ADC_)
     FFCOBP22002IF   E           K DISK    PREFIX(ADC_)
     F                                     RENAME(RCOBP220:RCOBP22002)
     FTBIDDEF   UF   E           K DISK    PREFIX(IDD_)
     FTBIDDPF   UF   E           K DISK    PREFIX(IDD_)
     FTBIDDPL3  IF   E           K DISK    PREFIX(IDD_) RENAME(TBIDDP:RTBIDDPL3)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Programas de Cobrança e Reemisão de Documentos
     D CallTB2300      PR                  EXTPGM(WDynamic)
     D   CodFile                     10A   CONST
     D WDynamic        S             10A
     D
     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D*==> Campos de Trabalho
     D WCodFile        S             10P 0
     D WCodErro        S              7A
     D WNumRecibo      S              9S 0
     D WFormato        S              3A
     D WUpdFile        S               N
     D WValRecibo      S             13S 2
     D WNumADC         S             10S 0
     D WCodRamo        S              2S 0
     D WNumApol        S              7S 0
     D
     D*==> Definição de Constantes
     D IDD#Enviado     C                   '3'
     D IDD#Confirmado  C                   '4'
     D IDD#Cancelado   C                   '5'
     D REC#Pendente    C                   0
     D REC#Cobrado     C                   2
     D ADC#Enviado     C                   '2'
     D ADC#Cancelada   C                   '7'
     D ADC#Aprovado    C                   '7'
     D ADC#Pendente    C                   '4'
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodFile
     C                   PARM                    WCodErro
     C
      /FREE
       WCodErro = *BLANKS;
       WUpdFile = *OFF;

       //----------------------------------------------------------------
       // Obter o formato do ficheiro enviado
       //----------------------------------------------------------------
       CHAIN(N) (WCodFile) TBIDDEF;
       IF (NOT %FOUND(TBIDDEF));
          WCodErro = 'COB0097';
       ELSE;
          WFormato = IDD_TETIPF;
       ENDIF;


       //----------------------------------------------------------------
       // Executar o ficheiro
       //----------------------------------------------------------------
       SETLL (WCodFile) RTBIDDPL3;
       READE (WCodFile) RTBIDDPL3;
       DOW (NOT %EOF(TBIDDPL3));
          EXSR Validar;
          IF (WCodErro <> *BLANKS);
             LEAVE;
          ENDIF;

          EXSR ReceberXML;
          IF (WCodErro <> *BLANKS);
             LEAVE;
          ENDIF;

          IF (WFormato = 'XML');
             EXSR chgADC;
             IF (WCodErro <> *BLANKS);
                LEAVE;
             ENDIF;
          ENDIF;
          READE (WCodFile) RTBIDDPL3;
       ENDDO;

       *INLR = *ON;

       //=======================================================================
       // Subrotina.: Validar
       // Objectivo.: Validar a situação do Recibo antes de proceder à cobrança
       //=======================================================================
       BEGSR Validar;
          //------------------------------------------------
          // Parâmetros Recebidos
          //------------------------------------------------
          MONITOR;
             IF (WCodFile = *ZEROS);
                WCodErro = 'COB0094';
             ENDIF;
          ON-ERROR;
             WCodErro = 'COB0094';
          ENDMON;

          WNumRecibo = IDD_IDREC;
          WValRecibo = IDD_IDVAL;
          WNumADC    = IDD_IDNADC;

          //-------------------------------------------------------
          // Validar se a Instrução pode ser processada
          //-------------------------------------------------------
          IF (IDD_IDSIT <> IDD#Enviado) OR (IDD_IDCRET <> *ZEROS);
             WCodErro = 'COB0084'; // A Intrução não pode ser processada
             LEAVESR;
          ENDIF;
       ENDSR;

       //=======================================================================
       // Subrotina.: ReceberXML
       // Objectivo.: Retornar o ficheiro XML
       //=======================================================================
       BEGSR ReceberXML;
          //------------------------------------------------------------
          // Actualizar informação do Ficheiro
          //------------------------------------------------------------
          IF (NOT WUpdFile);
             WUpdFile = *ON;
             CHAIN (WCodFile) TBIDDEF;
             IDD_TEDATR = $CvtData($DateToInt(%DATE()):'YYMD':'DMYY');
             IDD_TEREGA = *ZEROS;
             IDD_TEVALP = *ZEROS;
             UPDATE TBIDDE %FIELDS(IDD_TEDATR:IDD_TEREGA:IDD_TEVALP);
          ENDIF;

          //------------------------------------------------------------
          // Actualizar o recibo
          //------------------------------------------------------------
          CHAIN (WNumRecibo) TBIDDP;
          IDD_IDSIT  = IDD#Cancelado;
          IDD_IDCRET = 7;
          UPDATE TBIDDP %FIELDS(IDD_IDSIT:IDD_IDCRET);
       ENDSR;

       //=======================================================================
       // Subrotina.: ChgADC
       // Objectivo.: Mudar o estado da ADC, para aprovada
       //=======================================================================
       BEGSR ChgADC;
          CHAIN (WNumADC) RCOBP22002;
          IF (%FOUND(FCOBP22002));
             IF (ADC_SITADC = ADC#Enviado);
                CHAIN (ADC_CDNADC) RCOBP220;
                ADC_SITADC = ADC#Cancelada;
                UPDATE RCOBP220 %FIELDS(ADC_SITADC);
             ENDIF;
          ENDIF;
       ENDSR;

       //=======================================================================
       // Subrotina.: OpenDbsFiles
       // Objectivo.: Abrir Ficheiros de Base de Dados
       //=======================================================================
        BEGSR OpenDbsfiles;
           //----------------------------------------------------------
           // Abrir Ficheiro de Recibos Activos
           //----------------------------------------------------------
           MONITOR;
              WStdCmd = 'OVRDBF FILE(SDRCA) TOFILE(QS36F/SD.1.RCA) SHARE(*NO)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WCodErro = 'COB0087';
              LEAVESR;
           ENDMON;

           //----------------------------------------------------------
           // Abrir Ficheiro de Numeração
           //----------------------------------------------------------
           MONITOR;
              WStdCmd = 'OVRDBF FILE(SD1CG2) TOFILE(QS36F/SD.1.CG2) SHARE(*NO)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WCodErro = 'COB0088';
              LEAVESR;
           ENDMON;

           //----------------------------------------------------------
           // Abrir Ficheiro de Pessoas e Empresas
           //----------------------------------------------------------
           MONITOR;
              WStdCmd = 'OVRDBF FILE(SD1PE) TOFILE(SD.1.PE)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WCodErro = 'COB0089';
              LEAVESR;
           ENDMON;

        ENDSR;

       //=======================================================================
       // Subrotina.: OpenPrtFiles
       // Objectivo.: Abrir Printer Files
       //=======================================================================
        BEGSR OpenPrtfiles;
           //----------------------------------------------------------
           // Abrir Mapa de Registo diários SDD
           //----------------------------------------------------------
           MONITOR;
              WStdCmd = 'OVRPRTF FILE(TB2301M) HOLD(*YES)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WCodErro = 'COB0090';
              LEAVESR;
           ENDMON;

           //----------------------------------------------------------
           // Abrir Carta para o Mediador
           //----------------------------------------------------------
           MONITOR;
              WStdCmd = 'OVRPRTF FILE(TB2302M) PAGESIZE(*N 90) CPI(12) +
                         DRAWER(2) PAGRTT(0) DUPLEX(*YES) HOLD(*YES)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WCodErro = 'COB0091';
              LEAVESR;
           ENDMON;

           //----------------------------------------------------------
           // Abrir Carta para o Segurado
           //----------------------------------------------------------
           MONITOR;
              WStdCmd = 'OVRPRTF FILE(TB2303M) PAGESIZE(*N 90) CPI(12) +
                         DRAWER(2) PAGRTT(0) DUPLEX(*NO) HOLD(*YES)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WCodErro = 'COB0092';
              LEAVESR;
           ENDMON;
        ENDSR;
      /END-FREE
