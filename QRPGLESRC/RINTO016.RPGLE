     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RINTO016
     H* Descrição.: Criar o Recibo em Demo Comp, Lda
     H*
     H*===========================================================
     FFINTP023  IF   E           K DISK    PREFIX(COM_)
     FFINTP02501IF   E           K DISK    PREFIX(COM_)
     F                                     RENAME(RINTP025:RINTP02501)
     FFINTP028  UF A E           K DISK    PREFIX(INT_)
     FFINTP029  UF A E           K DISK    PREFIX(INT_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> API para criação de Recibos
     D CriarRecibo     PR                  EXTPGM('RCOBO010')
     D   TipRecibo                    1A   Const
     D   NumOrdAP                     7S 0 Const
     D   DatInicio                    8S 0 Const
     D   DatTermo                     8S 0 Const
     D   PremSimples                 11S 2 Const
     D   PremTotal                   11S 2 Const
     D   NumOrdem                     7S 0 Const
     D   CodErro                      7A   Const
     D   NumRecibo                    9S 0 Const Options(*NOPASS)
     D   DatEntrada                   8S 0 Const Options(*NOPASS)
     D   DatLimite                    8S 0 Const Options(*NOPASS)
     D   Ajuste                       5S 4 Const Options(*NOPASS)
     D   Comiss01                    11S 2 Const Options(*NOPASS)
     D   Comiss02                    11S 2 Const Options(*NOPASS)
     D   Comiss03                    11S 2 Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WNumOrdem       S              7S 0
     D WTipRecibo      S              1A
     D WCdRegis        S              9S 0
     D WCodErro        S              7A
     D WAjuste         S              5S 4
     D
     D*==> Constantes
     D STS#CriaRecibo  C                   '3'
     D STS#Facturar    C                   '4'
     D
     D REC#Novo        C                   '1'
     D REC#Estorno     C                   '5'
     D REC#Continua    C                   '2'
     D REC#Indemniza   C                   'I'
     D TIP#Premio      C                   'P'
     D TIP#Estorno     C                   'E'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdRegis
     C                   PARM                    WCodErro
     C
      /FREE
        //-----------------------------------------------------------------
        // ForCompr os dados a  inserir no Recibo
        //-----------------------------------------------------------------
        WCodErro = *BLANKS;
        CHAIN(N) (WCdRegis) RINTP028;
        IF (%FOUND(FINTP028));
           IF (INT_CDESTAD = STS#CriaRecibo);
              IF (INT_TPRECIB = REC#Novo) OR (INT_TPRECIB = REC#Continua);
                 WTipRecibo = TIP#Premio;
              ELSEIF (INT_TPRECIB = REC#ESTORNO);
                 WTipRecibo = TIP#Estorno;
              ELSE;
                 WCodErro = 'INT0013';
                 *INLR = *ON;
                 RETURN;
              ENDIF;

              WNumOrdem = *ZEROS;
              EXSR GetDesvio;
              CriarRecibo(WTipRecibo:INT_NUMORDAP:$DateToInt(INT_DTINICIO):
                          $DateToInt(INT_DTTERMO):INT_IMPRSIM:INT_IMTOTREC:
                          WNumOrdem:WCodErro:%DEC(INT_NUMREC:9:0):
                          $DateToInt(INT_DTENTRAD):$DateToInt(INT_DTLIMITE):
                          WAjuste:INT_IMCOMIS1:INT_IMCOMIS2:INT_IMCOMIS3);
              INT_NUMORDRC = WNumOrdem;
              IF (WCodErro = *BLANKS) AND (INT_NUMORDRC > *ZEROS);
                 EXSR ActRegisto;
              ENDIF;
           ENDIF;
        ENDIF;

        *INLR = *ON;

        //=======================================================================
        // Subrotina.: ActRegisto
        // Objectivo.: Actualizar o registo
        //=======================================================================
        BEGSR ActRegisto;
           CHAIN (WCdRegis) RINTP028;
           INT_CDESTAD = STS#Facturar;
           INT_NUMORDRC = WNumOrdem;
           UPDATE RINTP028 %FIELDS(INT_NUMORDRC:INT_CDESTAD);

           INT_NUOPERA  = $GetNextKey('FINTP029':'NUOPERA':
                          '(CDREGIS = '''+%CHAR(INT_CDREGIS)+''')');
           INT_CDERRO  = WCodErro;
           INT_DTOPERA = %DATE();
           INT_HROPERA = %TIME();
           INT_USOPERA = S#USR;
           WRITE RINTP029;
        ENDSR;

        //=======================================================================
        // Subrotina.: GetDesvio
        // Objectivo.: Obtem o desvio entre a comissão esperada e a calculada, e
        //             em função deste, determina o valor do factor para ajustar
        //             o valor da comissão.
        //=======================================================================
        BEGSR GetDesvio;
           WAjuste = 1;
           SETLL (INT_CDREGIS) RINTP02501;
           READE (INT_CDREGIS) RINTP02501;
           DOW (NOT %EOF(FINTP02501));
              CHAIN (COM_CDEXCEPT) RINTP023;
              IF (%FOUND(FINTP023));
                 IF (COM_TPEXCEPT = 'E') AND (COM_CDESTADO = 'A');
                     WAjuste = %DECH(COM_PRTOTCL / COM_PRTOTPR:5:4);
                 ENDIF;
              ENDIF;
              READE (INT_CDREGIS) RINTP02501;
           ENDDO;
        ENDSR;
      /END-FREE
