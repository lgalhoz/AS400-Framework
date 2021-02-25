     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*==================================================================
     H* Programa..: RCOBO130
     H* Descrição.: Cobrar recibos XML, devolvidos pelo Banco. Criado a
     H*             a partir do Original SOURCE/QRPGLESRC(TB2300R)
     H* Observação:
     H*
     H*==================================================================
     FSD1CG2    UF   F   80        DISK    EXTFILE('SD.1.CG2')
     FSDRCA     UF   F  326     6AIDISK    KEYLOC(5) EXTFILE('SD.1.RCA')
     FTBIDDPF   UF   E           K DISK    PREFIX(IDD_) RENAME(TBIDDP:RTBIDDPL4)
     FTBIDDGF   UF A E           K DISK    PREFIX(IDD_)
     F
     FSD1RE2    IF   E           K DISK    PREFIX(REC_)
     FTBIDDPL3  IF   E           K DISK    PREFIX(IDD_) RENAME(TBIDDP:RTBIDDPL3)
     FGFSEG     IF   E           K DISK    PREFIX(EXT_)
     FSD1PE     IF   E           K DISK    EXTFILE('SD.1.PE')
     FSD1CP     IF   E           K DISK
     F
     FTB2301M   O    E             PRINTER USROPN PREFIX(PRT_)
     FTB2302M   O    E             PRINTER USROPN PREFIX(PRT_)
     FTB2303M   O    E             PRINTER USROPN PREFIX(PRT_)
     F*==================================================================
     D*==> Recibos do Corretor
     D R#RCA         E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D
     D*==> Númeração Cartas de Agradecimento
     D R#SD1CG2        DS
     D   NUM_COD01             1      2S 0
     D   NUM_FIL01             3     29A
     D   NUM_COD02            30     35S 0
     D   NUM_FIL02            36     50A
     D   NUM_CARTA            51     56S 0
     D   NUM_FIL03            57     80A
     D
     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D*==> Campos de Trabalho
     D WCodFile        S             10S 0
     D WNumCarta       S              7S 0
     D WNumOrdem       S              6S 0
     D WNumRecibo      S              9S 0
     D WCodMediador    S              8S 0
     D WCodSegurado    S              8S 0
     D WCodEntidade    S              8S 0
     D WCodErro        S              7A
     D WModo           S              1A
     D
     D*==> Declaração de Constantes
     D MOD#Impressao   C                   'I'
     D IDD#Enviado     C                   '3'
     D IDD#Confirmado  C                   '4'
     D REC#Pendente    C                   0
     D REC#Cobrado     C                   2
     D MAP#Segurado    C                   'S'
     D MAP#Mediador    C                   'M'
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodFile
     C                   PARM                    WModo
     C                   PARM                    WCodErro
     C
      /FREE
        WCodErro    = *BLANKS;
        WIcResumo   = *OFF;
        WIcMediador = *OFF;
        WIcSegurado = *OFF;

        SETLL (WCodFile) RTBIDDPL3;
        READE (WCodFile) RTBIDDPL3;
        DOW (NOT %EOF(TBIDDPL3));
          //-------------------------------------------------------
          // Validar se a Intrução e/ou recibo pode ser processado
          //-------------------------------------------------------
          EXSR Validar;
          IF (WCodErro <> *BLANKS);
             LEAVE;
          ENDIF;

          //-------------------------------------------------------
          // Obter os dados do Segurado ou Mediador
          //-------------------------------------------------------
          EXSR GetEntidade;
          IF (WCodErro <> *BLANKS);
             LEAVE;
          ENDIF;

          //-------------------------------------------------------
          // Cobrar o recibo e gerar documento
          //-------------------------------------------------------
          IF (WModo <> MOD#Impressao);
             EXSR Cobrar;
             IF (WCodErro <> *BLANKS);
                LEAVE;
             ENDIF;
          ENDIF;

          //-------------------------------------------------------
          // Imprimir os Mapas
          //-------------------------------------------------------
          EXSR Imprimir;
          IF (WCodErro <> *BLANKS);
             LEAVE;
          ENDIF;
          READE (WCodFile) RTBIDDPL3;
       ENDDO;

       *INLR = *ON;

       //=======================================================================
       // Subrotina.: Validar
       // Objectivo.: Validar a situação do Recibo antes de proceder à cobrança
       //=======================================================================
       BEGSR Validar;
          WNumRecibo = IDD_IDREC;
          WCodSegurado = IDD_IDNSEG;
          WCodMediador = IDD_IDCCOB;
          IF (WCodMediador <> *ZEROS);
             WCodMediador += 60000;
             WCodEntidade = WCodMediador;
          ELSE;
             WCodEntidade = WCodSegurado;
          ENDIF;

          //-------------------------------------------------------
          // Validar se a Instrução pode ser processada
          //-------------------------------------------------------
          IF (WModo = MOD#Impressao);
             IF (IDD_IDSIT <> IDD#Confirmado);
                WCodErro = 'COBXXXX'; // A Intrução não pode ser processada
                LEAVESR;
             ENDIF;
          ELSE;
             IF (IDD_IDSIT <> IDD#Enviado);
                WCodErro = 'COBXXXX'; // A Intrução não pode ser processada
                LEAVESR;
             ENDIF;
          ENDIF;

          //-------------------------------------------------------
          // Validar Recibo no Corretor
          //-------------------------------------------------------
          WNumOrdem = *ZEROS;
          CHAIN (054:%CHAR(WNumRecibo)) SD1RE2;
          IF (NOT %FOUND(SD1RE2));
             WCodErro = 'COBXXXX'; // O Recibo não existe
          ELSE;
             WNumOrdem = REC_NORD;
             IF (WModo = MOD#Impressao);
                IF (REC_SIT <> REC#Cobrado);
                   WCodErro = 'COBXXXX'; // O Recibo não está Cobrado
                ENDIF;
             ELSE;
                IF (REC_SIT <> REC#Pendente);
                   WCodErro = 'COBXXXX'; // O Recibo não está pendente
                ENDIF;
             ENDIF;
          ENDIF;
       ENDSR;

       //=======================================================================
       // Subrotina.: GetEntidade
       // Objectivo.: Obter a Informação do Segurado ou Mediador
       //=======================================================================
       BEGSR GetEntidade;
          CHAIN (WCodEntidade) SD1PE;
          IF (%FOUND(SD1PE));
             WKNome = NOME;
          ELSE;
             WCodErro = 'COBXXXX'; // Segurado ou Mediador Inexistente
             LEAVESR;
          ENDIF;
       ENDSR;

       //=======================================================================
       // Subrotina.: Cobrar
       // Objectivo.: Proceder à Cobrança dos registos
       //=======================================================================
       BEGSR Cobrar;
          //-------------------------------------------------------
          // Obter o último número de Carta Disponível
          //-------------------------------------------------------
          EXSR GetNumCarta;
          IF (WCodErro <> *BLANKS);
             LEAVESR;
          ENDIF;

          //-----------------------------------------------
          // Escrita do Cabeçalho e detalhe do Mapa
          //-----------------------------------------------
          IF (NOT WIcCabeca);
             WRITE RCAB;
             IF (WIdMedCobra);
                WRITE CARCAB;
             ENDIF;
             WIcCabeca = *ON;
          ENDIF;
          WRITE RDET; // Detalhe do Mapa

          IF (WIdMedCobra);
             WRITE CARDET;
          ENDIF;

          //-----------------------------------------------
          // Cobrar o Recibo
          //-----------------------------------------------
          IDATA = *DATE;
          OD    = ID;
          OM    = IM;
          OA    = IA;
          USER  = 'C.I. SDD';
          ENTID = 'SD   ';
          CHAIN (WNagra) TBIDDG;
          IF (NOT %EOF(TBIDDG));
             VPAGX = IGVAL;
          ELSE;
             VPAGX = IDVAL;
          ENDIF;
          PGANT     = *ZEROS;
          DPAG      = *ZEROS;
          DESC      = *ZEROS;
          FPX       = 3;
          NDOC      = WCodFile;
          NDOC      = 'SDD';
          DESC      = *ZEROS;
          REC_CSEG  = WKENT;
          ONAGR     = IDNAGR;
          EXCEPT  URCA;

          IF (IDCCOB <> *ZEROS);
             // IN63
          ENDIF;
       ENDSR;

       //=======================================================================
       // Subrotina.: GetNumCarta
       // Objectivo.: Obter o último número disponível para a Carta
       //=======================================================================
       BEGSR GetNumCarta;
          CHAIN 1 SD1CG2 R#SD1CG2;
          IF (%FOUND(SD1CG2));
             NUM_CARTA  += 1;
             WNumCarta  = NUM_CARTA;
             UPDATE SD1CG3 R#SD1CG2;
          ELSE;
             WCodErro = 'COBXXXX'; // Erro a obter o número da Carta
          ENDIF;
       ENDSR;
       //=======================================================================
       // Subrotina.: CART
       // Objectivo.:
       //=======================================================================
       BEGSR CART;
     C                   SETOFF                                       59
     C                   Z-ADD     0             TIRS
     C*                    Z-ADDWNAGRA     NAGRAX  70
     C     CPOST         CHAIN     SD1CP                              35
     C                   MOVEL     DPOST         CPDESC
     C     1             IFEQ      1
     C     TCOM          IFGT      0
     C                   SETON                                        60
     C     IDENT         IFEQ      'I'
     C                   SETON                                        59
     C     TCOM          MULT(H)   .2            TIRS
     C                   ENDIF
     C                   Z-ADD     TCOM          TBASE
     C     TBASE         MULT(H)   2             TSELO
     C     TSELO         DIV(H)    102           TSELO
     C                   ELSE
     C                   SETOFF                                       60
     C                   Z-ADD     0             TIRS
     C                   Z-ADD     0             TSELO
     C                   Z-ADD     0             TBASE
     C                   ENDIF
     C                   Z-ADD     TPREM         TOTAL
     C                   SUB       TCOM          TOTAL
     C                   ADD       TIRS          TOTAL
     C                   ADD       TSELO         TOTAL
     C     TVAL          SUB       TOTAL         TDIF
     C*===> Formatar a morada correcta
      /FREE
        //----------------------------------------
        // Procurar o registo no ficheiro GFSEG
        //----------------------------------------
        WKMORADAN = MORADA;
        CHAIN (NSEG) FSEGF1;
        IF (%FOUND());
            IF (%SUBST(EXT_MORAD:1:36) = MORADA);
               WKMORADAN = EXT_MORAD;
            ENDIF;
        ENDIF;
      /END-FREE
     C*==================================
     C   63              WRITE     CARTA
     C  N63              WRITE     CARTAS
     C                   ELSE
     C                   ENDIF
     C                   ENDSR

       //=======================================================================
       // Subrotina.: CART
       // Objectivo.:
       //=======================================================================
        BEGSR OpenPrtfiles;
           //----------------------------------------------------------
           // Abrir Mapa de Registo diários SDD
           //----------------------------------------------------------
           IF (NOT WIcDiario);
              WIcDiario = *ON;
              MONITOR;
                 WStdCmd = 'OVRPRTF FILE(TB2301M) HOLD(*YES)';
                 WStdLen = %LEN(WStdCmd);
                 shell(WStdCmd:WStdLen);
                 OPEN TB2301M;
              ON-ERROR;
                 WCodErro = 'COBXXXX';
                 LEAVESR;
              ENDMON;
           ENDIF;

           //----------------------------------------------------------
           // Abrir Carta para o Mediador
           //----------------------------------------------------------
           IF (WTipMapa = MAP#Mediador) AND (NOT WIcMediador);
              WIcMediador = *ON;
              MONITOR;
                 WStdCmd = 'FILE(TB2302M) PAGESIZE(*N 90) CPI(12) +
                            DRAWER(2) PAGRTT(0) DUPLEX(*YES) HOLD(*YES)';
                 WStdLen = %LEN(WStdCmd);
                 shell(WStdCmd:WStdLen);
                 OPEN TB2302M;
              ON-ERROR;
                 WCodErro = 'COBXXXX';
                 LEAVESR;
              ENDMON;
           ENDIF;

           //----------------------------------------------------------
           // Abrir Carta para o Segurado
           //----------------------------------------------------------
           IF (WTipMapa = MAP#Segurado) AND (NOT WIcSegurado);
              WIcSegurado = *ON;
              MONITOR;
                 WStdCmd = 'OVRPRTF FILE(TB2303M) PAGESIZE(*N 90) CPI(12) +
                            DRAWER(2) PAGRTT(0) DUPLEX(*NO) HOLD(*YES)';
                 WStdLen = %LEN(WStdCmd);
                 shell(WStdCmd:WStdLen);
                 OPEN TB2303M;
              ON-ERROR;
                 WCodErro = 'COBXXXX';
                 LEAVESR;
              ENDMON;
           ENDIF;
        ENDSR;
      /END-FREE
     O*=========================================================================
     OSD1CG2    E            UPDN
     O                       UAGCH               56
     OSDRCA     E            URCA
     O                                            1 '0'
     O                                            2 '0'
     O                       ODATA              183
     O                       USER               207
     O                       ENTID              212
     O                       FPX                213
     O                       NDOC               223
     O                       VPAGX              230P
     O                       DESC               235P
     O                       PGANT              249P
     O                       DPAG               256P
     O                       ONAGR              262
     O                       REC_CSEG           268
     O                                          302 'M'
     O                                          307 '1'
     O                                          309 '2'
