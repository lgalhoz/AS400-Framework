     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*=====================================================================
     H* Programa...: PINTO025
     H* Objectivo..: Carregar Interface de Processos de Sinistros para
     H*              Integrar nos Sistemas de Informação da Lusitania.
     H*
     H* Observações: Este programa é invocado na cadeia de geração
     H*              automática de Interfaces.
     H*=====================================================================
     FOPSINIST  UF A F  384        DISK    USROPN
     FFINTP001  UF   E           K DISK    PREFIX(INT_)
     FFINTP004  UF A E           K DISK    PREFIX(INT_)
     FFSINP105  IF   E           K DISK
     FFSINP11402IF   E           K DISK    PREFIX(REL_)
     FGSINPRO   IF   E           K DISK    EXTFILE('G.SINPRO') PREFIX(PRC_)
     F*===================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*---------------------------------------------
     D* Invocar um comando de AS/400 externo:
     D*---------------------------------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WDatEncerra     S              8S 0
     D WMotEncerra     S              2A
     D WIcErro         S               N
     D P_DatInicio     S              8S 0
     D P_DatAltera     S              8S 0
     D WDatInicio      S               D
     D WDatAltera      S              8S 0
     D WDatRefer       S               D
     D WAaProc         S                   LIKE(REL_AAPROC)
     D WNuProc         S                   LIKE(REL_NUPROC)
     D
     D WCdRemessa      S                   LIKE(INT_CDREMESSA)
     D WNomAS400       S             10A
     D WNomPASE        S             40A
     D WSessionID      S              5S 0
     D WDatChar        S             20A
     D
     D ProcCodif       DS
     D   ProcAno               1      2S 0
     D   PDifer                3      3S 0
     D   ProcNum               4      7S 0
     D
     D*==> Função para converter datas
     D CvtDate         PR             8S 0
     D    WDatIN                       D   CONST OPTIONS(*NULLIND)
     D
     D CvtAS400Date    PR             8S 0
     D    WDatIN                      6S 0 CONST
     D
     D LAPOLT          S             10A   INZ(*BLANKS)
     D LPROCT          S             20A   INZ(*BLANKS)
     D LNRESP          S              3S 0 INZ(*ZEROS)
     D LRESP           S              4S 0 INZ(*ZEROS)
     D LLIPAG          S              1A   INZ(*BLANKS)
     D
     D*---> Informação FSA
     D CODPOST         S              4S 0 INZ(*ZEROS)
     D CODFREG         S              3S 0 INZ(*ZEROS)
     D
     D
     D KEYPRO          S              7A   INZ(*BLANKS)
     D LDTTRF          S              8S 0 INZ(*ZEROS)
     D
     D*==> Para converter me Maiúsuclas ou Minusculas
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyz'
     D*===================================================================
     IOPSINIST  NS  01
     I                             S    1    2 0LRAMO
     I                             A    3    4  LMOD
     I                             A    5    6  LSMOD
     I                             S    7   13 0LAPOL
     I                             S   14   17 0LANO
     I                             S   18   22 0LNPROC
     I                             S   23   30 0LDTSIN
     I                             S   31   38 0LDTABE
     I                             A   39   40  LCENC
     I                             S   41   48 0LDTENC
     I                             S   49   56 0LDTRAB
     I                             A   57   57  LIDS
     I*---> Informação IDS
     I                             A   58   59  LNCASO
     I                             S   60   67 0LDTAVI
     I                             A   68   85  LMARCT
     I                             A   86   97  LMATRT
     I                             A   98  101  LCOMPT
     I                             A  102  104  LCIDST
     I*---> Informação CPRS
     I                             S  105  112 0LDATENTDOC
     I                             A  113  113  ICREGESTR
     I                             A  114  114  ICFACEXC
     I                             A  115  115  ICSUSFRA
     I                             A  116  116  ICDECLEI
     I*---> Informação FSA
     I                             S  117  120 0CODPOST
     I                             S  121  123 0CODFREG
     I*---> Informação Apólice Terceiro
     I                             A  124  133  LAPOLT
     I                             A  134  153  LPROCT
     I                             S  154  156 0LNRESP
     I                             S  157  160 0LRESP
     I                             A  161  161  LLIPAG
     I*---> Informação Controlo Jose Mata
     I                             A  370  376  KEYPRO
     I                             S  377  384 0LDTTRF
     I*===========================================================
      /FREE
        //-----------------------------------------------------
        // Validar se está em execução o gerador de Interfaces
        //-----------------------------------------------------
        SETLL (*HIVAL) RINTP001;
        READP RINTP001;
        IF (INT_CDPRCEXEC <> 4) OR %EOF();
           *INLR = *ON;
           RETURN;
        ENDIF;
        WCdRemessa = INT_CDREMESSA;
        WDatInicio = INT_DTINICIO;
        WDatAltera = %SUBDT(INT_DTINICIO:*YEARS)*10000+
                     %SUBDT(INT_DTINICIO:*MONTHS)*100+
                     %SUBDT(INT_DTINICIO:*DAYS);
        WIcErro = *OFF;

        EXSR CrtOPSinist;
        IF (NOT WIcErro);
           OPEN OPSinist;
           EXSR AddSinistros;
           CLOSE OPSinist;
        ENDIF;
        EXSR SendSFTP;
        *INLR = *ON;

        //==============================================================
        // Subrotina.: addSinistro
        // Descrição.: Adicionar registos de Sinistros.
        //             Para o efeito selecciona todos os registos
        //             encerrados a seguir à data inserida como
        //             Parâmetro, junto com os processos ainda abertos.
        //==============================================================
        BEGSR AddSinistros;
           SETLL *LOVAL RSINP105;
           READ RSINP105;
           DOW (NOT %EOF());
              IF (%NULLIND(SIN10506)=*ON) OR
                 (SIN10506 >= WDatInicio);
                 IF (SIN10508 = -1);
                    IF (CvtDate(SIN10512) >= WDatAltera);
                       MONITOR;
                          IF (%NULLIND(SIN10506)=*ON);
                             WDatEncerra    = *ZEROS;
                             WMotEncerra    = *BLANKS;
                          ELSE;
                             WDatEncerra    = CvtDate(SIN10506);
                             WMotEncerra    = '02';
                          ENDIF;
                          ProcCodif = NRPROC;
                          WAaProc = ProcAno;
                          WNuProc = ProcNum;
                          EXSR GetApolice;    // Obter dados do AS/400
                          EXSR AddRecord;     // gravar os dados
                       ON-ERROR;
                       ENDMON;
                    ENDIF;
                 ENDIF;
              ENDIF;
              READ RSINP105;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: getApolice
        // Descrição.: Obter elementos em falta localizados no AS/400.
        //==============================================================
        BEGSR getApolice;
           LAPOL  = *ZEROS; // Número da Apólice
           LDTSIN = *ZEROS; // Data do Sinistro
           LDTABE = *ZEROS; // Data de Abertura do Processo
           LDTRAB = *ZEROS; // Data de Reabertura do Processo

           //------------------------------------------
           // Obter os dados da Apólice partindo do
           // Processo de Sinistro.
           //------------------------------------------
           CHAIN ('P':WAaProc:WNuProc) GSINPROF;
           IF (%FOUND());
              LAPOL    = PRC_AP;                  // Número da Apólice
              LDTSIN   = cvtAS400Date(PRC_DTSIN); // Data do Sinistro
              LDTABE   = cvtAS400Date(PRC_DTINIC);// data de Abertura
              LDTRAB   = cvtAS400Date(PRC_DTRINI);// Data Reabertura
              CODPOST  = %INT(PRC_CPLSIN/1000);    // Código Postal FSA
              CODFREG  = PRC_CPLSIN - CODPOST*1000;// Freguesia FSA
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: addRecord
        // Descrição.: Adicionar um novo registo à Tabela de Lesados.
        //==============================================================
        BEGSR AddRecord;
           LRAMO          = 31;       // Ramo Automóvel (Cod. Lusitania)
           LMOD           = '00';     // Modalidade (Cod. Lusitania)
           LSMOD          = '00';     // Sub-Modalidade (Cod.Lusitania)
           LANO           = 2000 + WAaProc;  // Ano do Processo
           LNPROC         = 90000 + WNuProc; // Número Processo
           LCENC          = WMotEncerra;     // Motivo de Encerramento
           LDTENC         = WDatEncerra;     // Data de Encerramento
           LIDS           = 'N';             // Processo IDS devedor

           //-------------------------------------------
           // Informação do Processo IDS
           //-------------------------------------------
           LNCASO         = *BLANKS; // Número do Caso IDS
           LDTAVI         = *ZEROS;  // Data do Nosso Aviso à congênere
           LMARCT         = *BLANKS; // Marca Veículo do Terceiro
           LMATRT         = *BLANKS; // Matrícula Veículo do Terceiro
           LCOMPT         = *BLANKS; // Código da Seguradora (Congênere)
           LCIDST         = *BLANKS; // Centro IDS

           LDATENTDOC     = cvtDate(DATAPART);
           ICREGESTR      = SIN10502; //Regularização no Estrangeiro
           ICFACEXC       = SIN10504; //Factores Exceptionais (informar S/N)
           ICSUSFRA       = SIN10503; //Suspeita de Fraude (informar S/N)
           ICDECLEI       = 'S';      // Ao abrigo do Dec.Lei 83/2006

           //-----------------------------------------------
           // Só regista se encontrou correspondência
           //-----------------------------------------------
           IF (LAPOL > *ZEROS);
              EXCEPT ADDRec;
           ELSE;
              WIcErro = *ON;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: CrtOPSinist
        // Descrição.: Construir o ficheiro Interface de Sinistros Auto
        //==============================================================
        BEGSR CrtOPSinist;
           //--------------------------------------------------------
           // Cria Script de Transferência
           //--------------------------------------------------------
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/OPSINIST)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           IF (NOT WIcErro);
              MONITOR;
                 WStdCmd = 'CRTPF FILE(QTEMP/OPSINIST) RCDLEN(384)';
                 WStdLen = %LEN(WStdCmd);
                 shell(WStdCmd:WStdLen);
              ON-ERROR;
                 WIcErro = *ON;
              ENDMON;
           ENDIF;
        ENDSR;

        //================================================================
        // Subrotina..: SendSFTP
        // Objectivo..: Colocar o ficheiro de Interface gerado no share
        //              da Lusitania.
        //================================================================
        BEGSR SendSFTP;
           WNomAS400 = 'OPLSEG';
           WSessionID = $SetFiles(%TRIM(WNomAS400));
           MONITOR;
              INT_NURECORD = $GetQtdeReg();
           ON-ERROR;
              INT_NURECORD = *ZEROS;
           ENDMON;
           $RmvDBSession(WSessionID);

           IF (INT_NURECORD > *ZEROS);
           MONITOR;
              WDatChar = %CHAR(%SUBDT(%DATE():*YEARS)*10000+
                               %SUBDT(%DATE():*MONTHS)*100+
                               %SUBDT(%DATE():*DAYS)) + '_' +
                         %CHAR(%SUBDT(%TIME():*HOURS)*10000+
                               %SUBDT(%TIME():*MINUTES)*100+
                               %SUBDT(%TIME():*SECONDS));
              WNomPase = %TRIM(WNomAS400)+'_'+%TRIM(WDatChar);
              WStdCmd = 'QSYS/CPYTOSTMF FROMMBR(''/qsys.lib/qtemp.lib/'+
                                     %TRIM(WNomAS400)+'.FILE/'+
                                     %TRIM(WNomAS400)+'.MBR'') +
                                     TOSTMF(''/interfaces/pendente/'+
                                     %TRIM(WNomPASE) + ''') STMFOPT(*REPLACE) +
                                     STMFCCSID(*STDASCII)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WIcErro = *ON;
           ENDMON;
           ENDIF;

           CHAIN (WCdRemessa) RINTP001;
           IF (%FOUND());
              INT_CDPRCEXEC = 3;
              INT_INT00102  = %TIMESTAMP();
              UPDATE RINTP001 %FIELDS(INT_CDPRCEXEC:INT_INT00102);
           ENDIF;
        ENDSR;

       //========================================================
       // Subrotina...: RegOutput
       // Objectivo...: Registar ficheiros gerados
       //========================================================
       BEGSR RegOutput;
          INT_CDREMESSA  = WCdRemessa;
          INT_NUSEQU     = $GetNextKey('FINTP004':'NUSEQU':
                          'CDREMESSA = ''' + %CHAR(WCdRemessa) + '''');
          INT_NOMAS400   = WNomAs400;
          INT_NOMPASE    = WNomPase;
          INT_ICENVIADO  = *ON;
          INT_DTENVIO    = %DATE();
          WRITE RINTP004;
       ENDSR;
      /END-FREE
     C*====================================================================
     OOPSINIST  EADD         ADDREC
     O                       LRAMO                2                             NÚMERO
     O                       LMOD                 4                             NÚMERO
     O                       LSMOD                6                             NOME
     O                       LAPOL               13
     O                       LANO                17
     O                       LNPROC              22
     O                       LDTSIN              30
     O                       LDTABE              38
     O                       LCENC               40
     O                       LDTENC              48
     O                       LDTRAB              56
     O                       LIDS                57
     O*---> Informação IDS
     O                       LNCASO              59
     O                       LDTAVI              67
     O                       LMARCT              85
     O                       LMATRT              97
     O                       LCOMPT             101
     O                       LCIDST             104
     O*---> Informação CPRS
     O                       LDATENTDOC         112
     O                       ICREGESTR          113
     O                       ICFACEXC           114
     O                       ICSUSFRA           115
     O                       ICDECLEI           116
     O*---> Informação FSA
     O                       CODPOST            120
     O                       CODFREG            123
     O*---> Informação Apólice Terceiro
     O                       LAPOLT             133
     O                       LPROCT             153
     O                       LNRESP             156
     O                       LRESP              160
     O                       LLIPAG             161
     O*---> Informação Controlo Jose Mata
     O                       KEYPRO             376
     O                       LDTTRF             384
     P*==================================================================
     P* Processo..: CvtDate
     P* Objectivo.: Convertar um campo de um formato data em
     P*             numerico de 8 posições
     P*==================================================================
     P CvtDate         B                   EXPORT
     D CvtDate         PI             8S 0
     D    DatIN                        D   Const OPTIONS(*NULLIND)
     D
     D WRetDate        S              8S 0
     D WDatChar        S             10A
     D WNewDate        S              8A
     D*==================================================================
      /FREE
         WRetDate = *ZEROS;

         MONITOR;
            WDatChar = %CHAR(DatIN:*ISO);
            WNewDate = %SUBST(WDatChar:1:4) +
                       %SUBST(WDatChar:6:2) +
                       %SUBST(WDatChar:9:2);
            WRetDate = %INT(WNewDate);
         ON-ERROR;
         ENDMON;

         RETURN WRetDate;
      /END-FREE
     P CvtDate         E
     P*==================================================================
     P* Processo..: CvtAS400Date
     P* Objectivo.: converter um campo de data do AS400 (AAMMDD) num
     P*             numerico de 8 posições
     P*==================================================================
     P CvtAS400Date    B                   EXPORT
     D CvtAS400Date    PI             8S 0
     D    DatIN                       6S 0 Const
     D
     D DatAS400        DS
     D  Ano                    1      2S 0
     D  Mes                    3      4S 0
     D  Dia                    5      6S 0
     D
     D WDatChar        S              6A
     D WRetDate        S              8S 0
     D*==================================================================
      /FREE
         WRetDate = *ZEROS;

         MONITOR;
            IF (DatIN < 100000);
               WDatChar = '0'+%CHAR(DatIN);
            ELSE;
               WDatChar = %CHAR(DatIN);
            ENDIF;
            DatAS400 = WDatChar;
            IF (Dia > *ZEROS);
               WRetDate = ((2000+Ano)*10000)+(Mes*100)+Dia;
            ENDIF;
         ON-ERROR;
         ENDMON;

         RETURN WRetDate;
      /END-FREE
     P CvtAS400Date    E
