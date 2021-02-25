     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*==================================================================
     H* Programa..: RCOBO124
     H* Descrição.: Actualizar a situação dos Recibos em função dos
     H*             ficheiros de retorno (IDD, SIBS, entre outros).
     H*==================================================================
     FSDRCA     UF A F  326     6AIDISK    KEYLOC(5)
     FTBIDDPF   UF A E           K DISK
     F                                     EXTFILE('SD.1.RCA')
     F*TB2201M   O    E             PRINTER USROPN
     F*==================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Formato de Registo dos Recibo do Corretor
     D SdRcaRec      E DS                  ExtName('SD.1.REC') PREFIX('RC_')
     D
     D*==> Campos de Trabalho
     D WCodFicheiro    S             10S 0
     D WNumRecibo      S              9S 0
     D WNumOrdemNum    S              7S 0
     D WNumOrdemAlf    S              6A
     D WNumCarta       S              6S 0
     D WCodEntidade    S              6S 0
     D WPagamento      S             13S 0
     D WCodRetorno     S              2S 0
     D WMsgErro        S              7A
     D
     D*==> Constantes
     D KTE#CodUser     C                   'C.I. SDD'
     D KTE#CodPaga     C                   'SD   '
     D KTE#FormPaga    C                   3
     D KTE#Cobrado     C                   2
     D KTE#Processado  C                   '4'
     D KTE#Anulado     C                   '5'
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNumRecibo
     C                   PARM                    WnumCarta
     C                   PARM                    WNumOrdemNum
     C                   PARM                    WCodRetorno
     C                   PARM                    WMsgErro
      /FREE
       //-------------------------------------------------
       // Obter a Informação da Instrução
       //-------------------------------------------------
       CHAIN (WNumRecibo) TBIDDP;
       IF (NOT %FOUND(TBIDDPF));
          WMsgErro = 'COB0041';
          RETURN;
       ENDIF;

       IF (WCodRetorno <> *ZEROS);
          EXSR MakePagamento;
       ELSE;
          EXSR AnulaIDD;
       ENDIF;

       RETURN;
       *INLR = *ON;

       //===================================================
       // Subrotina.: MakePagamento
       // Objectivo.: Actualizar a informação do pagamento
       //===================================================
       BEGSR MakePagamento;
          WCodFicheiro = IDFENV;
          WPagamento   = IDVAL;
          IF (IDDCOB <> *ZEROS);
             WCodEntidade = IDDCOB + 60000;
          ELSE;
             WCodEntidade = IDNSEG;
          ENDIF;

          //-------------------------------------------------
          // Ler a informação do Recibo no Corretor
          //-------------------------------------------------
          WNumOrdemAlf = %CHAR(WNumOrdemNum);
          CHAIN WNumOrdemAlf SDRCA SdRcaRec;
          IF (NOT %FOUND(SDRCA));
             WMsgErro = 'COB0040';
             RETURN;
          ENDIF;

          //-------------------------------------------------
          // Actualizar a informação do Recibo
          //-------------------------------------------------
          RC_TPCOB   = '0';
          RC_CODFACT = '0';
          RC_DTCOB   = $CvtData($DateToInt(%DATE()):'YYMMDD':'DMY');
          RC_USER    = KTE#CodUser;
          RC_DPAG    = KTE#CodPaga;
          RC_FP      = KTE#FormPaga;
          RC_NDP     = %CHAR(WCodFicheiro);
          RC_VDESC   = *ZEROS;     // DESC
          RC_DIFPAG  = *ZEROS;     // DPAG
          RC_VALIE   = *ZEROS;     // PGANT
          RC_VPAG    = WPagamento; // VPAGX
          RC_UNACHX  = WNumCarta;  // ONAGR
          RC_CSEGX   = WCodEntidade;
          RC_CDALT   = 'M';
          RC_SAIDA   = '1';
          RC_CODSIT  = KTE#Cobrado;
          UPDATE SDRCA SdRcaRec;

          //-------------------------------------------------
          // Actualizar a informação da IDD
          //-------------------------------------------------
          IDSIT  = KTE#Processado;
          IDCRET = WCodRetorno;
          IDNAGR = WNumDocumento;
          UPDATE TBIDDP %FIELDS(IDSIT:IDCRET:IDNAGR);
       ENDSR;

       //===================================================
       // Subrotina.: AnulaIDD
       // Objectivo.: Anular a Instrução de Débito.
       //===================================================
       BEGSR AnulaIDD;
          IDSIT  = KTE#Anulado;
          IDCRET = WCodRetorno;
          UPDATE TBIDDP %FIELDS(IDSIT:IDCRET);
       ENDSR;
      /END-FREE
