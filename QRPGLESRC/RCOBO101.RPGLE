     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RCOBO101
     H* Descrição.: Gerar o interface de Texto EDR (Debitos Directos)
     H*==================================================================
     H* Estrutura do ficheiro a Gerar (EDR)
     H*-----------------------------------------
     H*     <0> ]=====> Header do Ficheiro
     H*      <1> ]====> Header do Lote
     H*       <2-7> ]=> Detalhe (1..n registos)
     H*      <8> ]====> Trailer do Lote
     H*     <9> ]=====> Trailer do Ficheiro
     H*
     H*==================================================================
     FTBIDDFF   UF A E           K DISK
     F*=========================================================================
      /COPY QRPGLESRC,SGRLO010

     D*==> Campos de Trabalho
     D  WNuseqSaved    S              5S 0
     D  WMsgErro       S              7A
     D  WCodFileAct    S             10P 0
     D  WNumNIB        S             21S 0
     D  WMnteRecibo    S             13S 2
     D  WCodRecibo     S              9S 0
     D  WCodADC        S             11S 0
     D  WADCSepa       S             11A
     D  WSitADC        S              1A
     D  WCodSegura     S              8S 0
     D  PCodFileAnt    S             10P 0
     D  PQtdeRegist    S              6S 0
     D  PMnteRegist    S             13S 2
     D  PDatDiaAlf     S              8S 0
     D  WCodFileAnt    S             10P 0
     D  WQtdeRegist    S              6S 0
     D  WMnteRegist    S             13S 2
     D
     D  WDatSelect     S              8S 0
     D  WDatActual     S              8S 0
     D  WIcSelect      S               N
     D  WSavSequ       S              2S 0
     D  WSavCodFile    S             10S 0
     D  WQtdeRegis     S              4S 0
     D  WMnteRegis     S             13S 2
     D*=========================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodFileAct
     C                   PARM                    WNumNIB
     C                   PARM                    WMnteRecibo
     C                   PARM                    WCodRecibo
     C                   PARM                    WCodADC
     C                   PARM                    WADCSepa
     C                   PARM                    WSitADC
     C                   PARM                    WCodSegura
     C                   PARM                    WNuseqSaved
     C                   PARM                    PDatDiaAlf
     C                   PARM                    WMsgErro
     C                   PARM                    PCodFileAnt
     C                   PARM                    PQtdeRegist
     C                   PARM                    PMnteRegist
      /FREE
       WDatActual = $DateToInt(%DATE());

       //------------------------------------------------
       // Criar o Cabeçalho se for o Primeiro Registo
       //------------------------------------------------
       CHAIN(N) (WCodFileAct) TBIDDF;
       IF (NOT %FOUND(TBIDDFF));
          MONITOR;
             WCodFileAnt = PCodFileAnt;
          ON-ERROR;
             RETURN;
          ENDMON;
          EXSR CrtHDRFile;
          EXSR CrtHDRLote;
       ENDIF;

       //------------------------------------------------
       // Criar o Registo de Detalhe
       //------------------------------------------------
       IF (WNumNIB > *ZEROS);
          EXSR CrtDetLote;
       ENDIF;

       //------------------------------------------------
       // Criar o Registo de Resumo
       //------------------------------------------------
       IF (WNumNIB = *ZEROS);
          MONITOR;
             WQtdeRegist = PQtdeRegist;
          ON-ERROR;
             RETURN;
          ENDMON;
          MONITOR;
             PMnteRegist = PMnteRegist;
          ON-ERROR;
             RETURN;
          ENDMON;
          EXSR CrtResLote;
          EXSR CrtResFile;
       ENDIF;
       RETURN;

       //=============================================================
       // Subrotina.: CrtHDRFile
       // Objectivo.: Criar o Header do ficheiro (Tipo 1)
       //=============================================================
       BEGSR CrtHDRFile;

          //----------------------------------------------
          // Criar registos "Header do Ficheiro"
          //----------------------------------------------
          TFFENV = WCodFileAct;
          TFNLIN = 1;
          %SUBST(TFTEXT: 1: 1) = '0';                // Identificação "Header do Ficheiro"
          %SUBST(TFTEXT: 2: 1) = 'D';                // Aplicação
          %SUBST(TFTEXT: 3: 3) = 'EDR';              // Identificação do Ficheiro
          %SUBST(TFTEXT: 6: 6) = '102706';           // ID Atribuido pela SIBS à Jose Mata, Lda
          %SUBST(TFTEXT:12: 2) = '01';               // Adicção ao Código do Credor
          %SUBST(TFTEXT:14: 2) = '00';               // Valor Reservado
          %SUBST(TFTEXT:16: 4) = '0018';             // Banco Credor (TOTTA)
          %SUBST(TFTEXT:20: 2) = '01';               // Valores Reservado
          %SUBST(TFTEXT:22:10) = %CHAR(WCodFileAct); // Ficheiro Actual
          %SUBST(TFTEXT:32:10) = %CHAR(WCodFileAnt); // Ficheiro Anterior
          %SUBST(TFTEXT:42:1)  = '1';                // Fecho de Telecompensação
          %SUBST(TFTEXT:43:13) = *ZEROS;             // Reservado
          %SUBST(TFTEXT:56:8)  = *ZEROS;             // A Preencher pelo Banco
          %SUBST(TFTEXT:64:31) = *ZEROS;             // Reservado
          WRITE TBIDDF;
       ENDSR;

       //=============================================================
       // Subrotina.: CrtHDRLote
       // Objectivo.: Criar o Header do Lote (Tipo 2)
       //=============================================================
       BEGSR CrtHDRLote;
          TFFENV = WCodFileAct;
          TFNLIN += 1;
          %SUBST(TFTEXT: 1: 1) = '1';                // Identificação "Header do Lote"
          %SUBST(TFTEXT: 2: 4) = '0001';             // Identificação da Linha do Lote
          %SUBST(TFTEXT: 6: 3) = '001';              // ??? CLCSR
          %SUBST(TFTEXT: 9: 2) = '60';               // Tipo de Serviço "Seguros Diversos"
          %SUBST(TFTEXT:11: 6) = '102706';           // ID Atribuido pela SIBS à Jose Mata, Lda
          %SUBST(TFTEXT:17: 4) = '0018';             // Identificação do Banco Credor
          %SUBST(TFTEXT:21: 3) = '978';              // Código de Moeda (ISO 4217:978) = Euros
          %SUBST(TFTEXT:24:21) = *BLANKS;            // Reservado
          %SUBST(TFTEXT:45:20) = *BLANKS;            // Reservado
          %SUBST(TFTEXT:65: 8) = %CHAR(WDatActual);  // Data do Ficheiro
          %SUBST(TFTEXT:73:22) = *ZEROS;             // Reservado
          WRITE TBIDDF;
       ENDSR;

       //=============================================================
       // Subrotina.: CrtDetLote
       // Objectivo.: Escrever o detalhe do registo
       //=============================================================
       BEGSR CrtDetLote;
          TFFENV = WCodFileAct;
          TFNLIN += 1;
          %SUBST(TFTEXT: 1: 1) = '2';                // Identificação "Detalhe do Lote"
          %SUBST(TFTEXT: 2:21) = %TRIM(%EDITW(       // Identificação NIB do Cliente
                                 WNumNIB:'0                     '));
          %SUBST(TFTEXT:23:13) = %TRIM(%EDITW(       // Valor do Recibo
                                 WMnteRecibo:'0             '));
          %SUBST(TFTEXT:36:11) = %TRIM(%EDITW(       // Código da ADC do Cliente
                                 WCodADC:'0           '));
          %SUBST(TFTEXT:47: 9) = *ZEROS;             // Reservado
          %SUBST(TFTEXT:56:21) = *BLANKS;            // Reservado
          %SUBST(TFTEXT:77: 9) = %TRIM(%EDITW(       // Número do Recibo
                                 WCodRecibo:'0         '));
          %SUBST(TFTEXT:86: 7) = *BLANKS;            // Reservado
          %SUBST(TFTEXT:93: 2) = *ZEROS;             // Código de Retorno
          WRITE TBIDDF;
       ENDSR;

       //=============================================================
       // Subrotina.: CrtResLote
       // Objectivo.: Criar Resumo do Lote (Tipo 8)
       //=============================================================
       BEGSR CrtResLote;
          TFFENV = WCodFileAct;
          TFNLIN += 1;
          %SUBST(TFTEXT: 1: 1) = '8';                // Identificação "Header do Lote"
          %SUBST(TFTEXT: 2: 6) = %TRIM(%EDITW(       // Quantidade de Registos do Lote
                                 WQtdeRegis:'0      '));
          %SUBST(TFTEXT: 8:17) = *ZEROS;             // Reservado
          %SUBST(TFTEXT:25:13) = %TRIM(%EDITW(       // Valor Total a Creditar do Lote
                                  WMnteRegis:'0             '));
          %SUBST(TFTEXT:38:30) = *ZEROS;             // Reservado
          %SUBST(TFTEXT:68:27) = *ZEROS;             // Reservado
          WRITE TBIDDF;
       ENDSR;

       //=============================================================
       // Subrotina.: CrtResFile
       // Objectivo.: Criar Resumo do Ficheiro (Tipo 9)
       //=============================================================
       BEGSR CrtResFile;
          TFFENV = WCodFileAct;
          TFNLIN += 1;
          %SUBST(TFTEXT: 1: 1) = '9';                // Identificação "Resumo do Fichero"
          %SUBST(TFTEXT: 2: 6) = %TRIM(%EDITW(       // Total de Registos do Ficheiro
                                 (WQtdeRegis+2):'0      '));
          %SUBST(TFTEXT: 8:13) = *ZEROS;             // Reservado
          %SUBST(TFTEXT:21:13) = %TRIM(%EDITW(       // Valor Total a Creditar
                                 WMnteRegis:'0             '));
          %SUBST(TFTEXT:34:30) = *ZEROS;             // Reservado
          %SUBST(TFTEXT:64:31) = *ZEROS;             // Reservado
          WRITE TBIDDF;
       ENDSR;
      /END-FREE
