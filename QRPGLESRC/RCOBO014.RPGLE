     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RCOBO014
     H* Objectivo.: Agendar Contactos
     H* Descrição.: Actualizar a tabela de Contactos, com a
     H*             informação dos recibos que estão em Falta de
     H*             pagamento, recibos com valores de comissão
     H*             diferentes, prémios diferentes do esperado,
     H*             e apólices em falta.
     H*
     H*===========================================================
     FSD1REC    IF   F  326     7AIDISK    KEYLOC(270) EXTFILE('SD.1.REC')
     FFINTP028  IF   E           K DISK    PREFIX(INT_)
     FFCOBP010  UF A E           K DISK    PREFIX(COB_)
     FFCOBP01001IF   E           K DISK    PREFIX(COB_)
     F                                     RENAME(RCOBP010:RCOBP01001)
     FFCOBP011  UF A E           K DISK    PREFIX(COB_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura do registo de Recibos
     D R#SD1REC      E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D
     D*==> Campos de Trabalho
     D WDatPxCont      S              8S 0
     D WDatLimite      S              8S 0
     D WDatActual      S              8S 0
     D WCodContacto    S              9S 0
     D WTipEntidade    S              1A
     D WCodEstado      S              1A
     D WNumOrdem       S              9S 0
     D WCodOperacao    S              5S 0
     D WNumOrdRC       S              7S 0
     D WCodErro        S              7A
     D WIcExiste       S               N
     D
     D*==> Definição de Constantes
     D REC#Anulado     C                   '9'
     D REC#Pago        C                   '7'
     D REC#Cobrado     C                   '6'
     D REC#SEfeito     C                   '8'
     D TRC#Indemniza   C                   '7'
     D TIP#Recibo      C                   'R'
     D TIP#Apolice     C                   'A'
     D TIP#Interface   C                   'I'
     D STS#Fechado     C                   'F'
     D STS#Aberto      C                   'A'
     D MOT#Atraso      C                   'AT'
     D CNT#Telefone    C                   'TL'
     D*===========================================================
      /FREE
        WCodErro     = *BLANKS;
        WCodContacto = *ZEROS;

        //--------------------------------------------------------
        // Seleccionar recibos em José Mata, Lda com data expirada
        //--------------------------------------------------------
        SETLL *LOVAL SD1REC;
        READ SD1REC R#SD1REC;
        DOW (NOT %EOF(SD1REC));
           WDatPxCont = *ZEROS;
           MONITOR;
              WDatLimite = %DEC(REC_DTLIM:6:0);
           ON-ERROR;
              WDatLimite = *ZEROS;
           ENDMON;
           WDatLimite = $CvtData(WDatLimite:'DMY':'YYMD');
           WDatActual = $DateToInt(%DATE());
           IF (WDatLimite <= WDatActual) AND ((REC_CODSIT = 0)
             OR (REC_CODSIT = 4)) AND (REC_CODMOV = 15)
             AND (WDatLimite > 20000000) AND (REC_VTREC > *ZEROS);
              WDatPxCont   = WDatLimite;
              WNumOrdem    = %DEC(REC_NORD2:7:0);
              WTipEntidade = TIP#Recibo;
              EXSR Agendar;
           ENDIF;
           READ SD1REC R#SD1REC;
        ENDDO;

        //--------------------------------------------------------
        // Seleccionar recibos Standard cuja data limite expirou
        //--------------------------------------------------------
        SETLL *LOVAL RINTP028;
        READ RINTP028;
        DOW (NOT %EOF(FINTP028));
           WDatPxCont = *ZEROS;
           IF (INT_CDESTAD <> REC#Anulado) AND (INT_CDESTAD <> REC#Pago)
             AND (INT_CDESTAD <> REC#SEfeito) AND (INT_CDESTAD <> REC#Cobrado)
              AND (INT_TPRECIB <> TRC#Indemniza);
              IF (INT_DTLIMITE < %DATE());
                 WDatPxCont = $DateToInt(INT_DTLIMITE);

                 //---------------------------------------------------
                 // Validar se o registo já existe
                 //---------------------------------------------------
                 IF (WNumOrdRC > *ZEROS);
                    WNumOrdRC = INT_NUMORDRC;
                    WNumOrdem = INT_NUMORDRC;
                    CHAIN WNumOrdRC SD1REC R#SD1REC;
                    IF (%FOUND(SD1REC));
                       IF (REC_CODSIT = 2);
                          WCodEstado = '7'; // Cobrado
                       ELSEIF (REC_CODSIT = 1) OR (REC_CODSIT = 3);
                          WCodEstado = '9'; // Anulado
                       ENDIF;
                    ENDIF;
                 ELSE;
                    WNumOrdem = INT_CDREGIS;
                 ENDIF;
                 WTipEntidade = TIP#Interface;
                 EXSR Agendar;
              ENDIF;
           ENDIF;
           READ RINTP028;
        ENDDO;

        *INLR = *ON;

        //==========================================================================
        // Subrotina.: Agendar
        // Objectivo.: Agendar um contacto se este ainda não existir.
        //==========================================================================
        BEGSR Agendar;
           //---------------------------------------------------
           // Validar o estado do recibo em José Mata, LDA
           //---------------------------------------------------
           WCodEstado = '0'; // Cobrado
           WIcExiste    = *OFF;
           SETLL (WTipEntidade:WNumOrdem) RCOBP01001;
           READE (WTipEntidade:WNumOrdem) RCOBP01001;
           DOW (NOT %EOF(FCOBP01001));
              WCodContacto = COB_CDCONT;
              IF (COB_ICESTAD = STS#Aberto);
                 WIcExiste = *ON;
                 LEAVE;
              ENDIF;
              READE (WTipEntidade:WNumOrdem) RCOBP01001;
           ENDDO;

           IF (NOT WIcExiste) AND (WCodEstado = '0');
              IF (WCodContacto = *ZEROS);
                 WCodContacto  = $GetNextKey('FCOBP010':'CDCONT');
              ELSE;
                 WCodContacto += 1;
              ENDIF;

              WCodOperacao =  1;
              COB_CDCONT  = WCodContacto;
              COB_MTCONT  = MOT#Atraso;
              COB_ICESTAD = STS#Aberto;
              COB_NUOPERA = WCodOperacao;
              COB_TPENTID = WTipEntidade;
              COB_NUORDEM = WNumOrdem;
              COB_DTPXCNT = WDatPxCont;
              COB_HRPXCNT = 080000;
              COB_DTINICIO= $DateToInt(%DATE());
              COB_DTTERMO = *ZEROS;
              WRITE RCOBP010;
              EXSR ActOperacao;

           ELSEIF (WCodEstado <> '0');
              CHAIN (WCodContacto) RCOBP010;
              IF (%FOUND(FCOBP010));
                 COB_ICESTAD = STS#Fechado;
                 COB_DTPXCNT = 9991231;
                 COB_HRPXCNT = 235959;
                 COB_NUOPERA += 1;
                 WCodOperacao  = COB_NUOPERA;
                 UPDATE RCOBP010 %FIELDS(COB_ICESTAD:COB_NUOPERA:
                                         COB_HRPXCNT:COB_DTPXCNT);
                 EXSR ActOperacao;
              ENDIF;
           ENDIF;
        ENDSR;

        //===============================================================
        // Subrotina.: ActOperacao
        // Objectivo.: Actualização a operação sobre a acção
        //===============================================================
        BEGSR ActOperacao;
           COB_CDCONT  = WCodContacto;
           COB_NUOPERA = WCodOperacao;
           COB_DTCONT  = *ZEROS;
           COB_HRCONT  = *ZEROS;
           COB_USCONT  = *BLANKS;
           COB_TPPXCNT = CNT#Telefone;
           COB_DTPXCNT = WDatPxCont;
           COB_HRPXCNT = 080000;
           WRITE RCOBP011;
        ENDSR;
      /END-FREE
