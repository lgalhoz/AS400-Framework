     H*====================================================================
     H* Programa...: PSINT102
     H* Objectivo..: Normalizar Informação dos Processos de Sinistros
     H*              Automóvel, com a informação complementar de Provisões,
     H*              Pagamentos.
     H*====================================================================
     H ALWNULL(*USRCTL) DFTACTGRP(*NO) BNDDIR('JMDIR') ACTGRP(*NEW)
     H FIXNBR(*ZONED)
     H*====================================================================
     FGSINPRO   IF   F  384     7AIDISK    EXTFILE('G.SINPRO') KEYLOC(2)
     FGSINPRV   IF   F  128     8AIDISK    EXTFILE('G.SINPRV') KEYLOC(2)
     FGSINPAG   IF   F  300     8AIDISK    EXTFILE('G.SINPAG') KEYLOC(2)
     FFSINT10001IF   E           K DISK    PREFIX(SIN_)
     F                                     RENAME(RSINT100:RSINT10001)
     FFSINT118  UF A E           K DISK    PREFIX(SIN_)
     FFSINT128  UF A E           K DISK    PREFIX(SIN_)
     F*====================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*--> Processos
     D ConvN           PR            15S 5
     D    PARMPRO                    15S 5 Const
     D    PARMSIN                    15S 5 Const
     D    TIPO                        1A   Const Options(*NOPASS)
     D
     D ConvA           PR           100A
     D    PARMPRO                   100A   Const
     D    PARMSIN                   100A   Const
     D
     D*--> Formatos de Registo
     D R#SINPRO      E DS                  EXTNAME('G.SINPRO') PREFIX('PRO_')
     D R#SINPRV      E DS                  EXTNAME('G.SINPRV') PREFIX('PRV_')
     D R#SINPAG      E DS                  EXTNAME('G.SINPAG') PREFIX('PAG_')
     D
     D*--> Declaração das Chaves
     D K#SINPRO        DS                  QUALIFIED
     D   Chave                 1      7A
     D   Tipo                  1      1A
     D   Ano                   2      3S 0
     D   Sequencia             4      7S 0
     D
     D*--> Campos de Trabalho
     D WAAPROC         S                   LIKE(SIN_AAPROC)
     D WNSEQ           S                   LIKE(SIN_NSEQ)
     D WNordSin        S                   LIKE(SIN_NORDSIN) INZ(*ZEROS)
     D WRamo           S                   LIKE(SIN_RAMO) INZ(*ZEROS)
     D WNumApol        S                   LIKE(SIN_APOLICE) INZ(*ZEROS)
     D WAction         S              1A   INZ(*BLANKS)
     D WIsToChange     S              1A   INZ('N')
     D WCodErro        S              7A   INZ(*BLANKS)
     D
     D*--> Constantes
     D ACT#Update      C                   'U'
     D ACT#Insert      C                   'I'
     D ACT#Delete      C                   'D'
     D*===================[ Campos renomeados ]============================
      /FREE
       //------------------------------------------
       // Actualizar informação a partir do GSINPRV
       //------------------------------------------
       SETLL *LOVAL GSINPRV;
       READ GSINPRV R#SINPRV;
       DOW NOT %EOF(GSINPRV);
          WAAPROC = 2000 + PRV_AAPROC;
          WNSEQ   = 90000 + PRV_NSEQ;
          EXSR GetApolice;
          EXSR GetProcesso;
          EXSR ActProvisoes;
          READ GSINPRV R#SINPRV;
       ENDDO;

       //------------------------------------------
       // Actualizar informação a partir do GSINPAG
       //------------------------------------------
       SETLL *LOVAL GSINPAG;
       READ GSINPAG R#SINPAG;
       DOW NOT %EOF(GSINPAG);
          WAAPROC  = 2000 + PAG_AAPROC;
          WNSEQ    = 90000 + PAG_NSEQ;
          EXSR GetApolice;
          EXSR GetProcesso;
          EXSR ActPagamentos;
          READ GSINPAG R#SINPAG;
       ENDDO;

       *INLR = *ON;

       //==============================================================
       // Subrotina..: GetApolice
       // Objectivo..: Obter os dados da Apólice
       //==============================================================
       BEGSR GetApolice;
          WRamo              = *ZEROS;
          WNumApol           = *ZEROS;
          K#SINPRO.Chave     = *BLANKS;
          K#SINPRO.Tipo      = *BLANKS;
          K#SINPRO.Ano       = WAAPROC - 2000;
          K#SINPRO.Sequencia = WNSEQ - 90000;
          CHAIN K#SINPRO GSINPRO R#SINPRO;
          IF (NOT %FOUND(GSINPRO));
             K#SINPRO.Tipo = 'P';
             CHAIN K#SINPRO GSINPRO R#SINPRO;
          ENDIF;

          IF (%FOUND(GSINPRO));
             WRamo = PRO_RM;
             WNumApol = PRO_AP;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina..: GetProcesso
       // Objectivo..: Obter o código de Processo Gerados
       //==============================================================
       BEGSR GetProcesso;
          //--------------------------------------------------------------
          // Obter o Código Interno do Processo
          //--------------------------------------------------------------
          CHAIN (WAAPROC:WNSEQ:WRamo:WNumApol) RSINT10001;
          IF (%FOUND(FSINT10001));
             WNordSin = SIN_NORDSIN;
          ELSE;
             WCoderro = 'ER00000';
             LEAVESR;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina..: ActProvisoes
       // Objectivo..: Actualizar os Provisoes de Sinistros Automóvel
       //==============================================================
       BEGSR ActProvisoes;
          //--------------------------------------------------------------
          // Obter o Código da Linha da Provisão
          //--------------------------------------------------------------
          CHAIN (WNordSin:PRV_NLINHA) RSINT118;
          IF (%FOUND(FSINT118));
             WAction  = ACT#Update;
             WIsToChange = 'N';
          ELSE;
             WIsToChange = 'S';
             WAction = ACT#Insert;
             SIN_NORDSIN = WNordSin;
             SIN_NLINHA  = ConvN(PRV_NLINHA :SIN_NLINHA );
          ENDIF;

          //--------------------------------------------------------------
          // ForCompr campos dos Provisões de Sinistro
          //--------------------------------------------------------------
          SIN_TIPMOV  = ConvN(PRV_TIPMOV :SIN_TIPMOV );
          SIN_DTMOV   = ConvN(PRV_DTMOV  :SIN_DTMOV  :'D');
          SIN_DESCR   = ConvA(PRV_DESCR  :SIN_DESCR  );
          SIN_VALOR   = ConvN(PRV_VALOR  :SIN_VALOR  );
          SIN_VALR1   = ConvN(PRV_VALR1  :SIN_VALR1  );
          SIN_VALR3   = ConvN(PRV_VALR3  :SIN_VALR3  );
          SIN_VALR45  = ConvN(PRV_VALR45 :SIN_VALR45 );
          SIN_VLMIP   = ConvN(PRV_VLMIP  :SIN_VLMIP  );
          SIN_VLDTRA  = ConvN(PRV_VLDTRA :SIN_VLDTRA );
          SIN_CTAIND  = ConvA(PRV_CTAIND :SIN_CTAIND );
          SIN_SIT1    = ConvA(PRV_SIT1   :SIN_SIT1   );
          SIN_SIT2    = ConvA(PRV_SIT2   :SIN_SIT2   );
          SIN_SIT3    = ConvA(PRV_SIT3   :SIN_SIT3   );
          SIN_VLDMAT  = ConvN(PRV_VLDMAT :SIN_VLDMAT );
          SIN_VLDCOR  = ConvN(PRV_VLDCOR :SIN_VLDCOR );
          SIN_AASIN   = ConvN(PRV_AASIN  :SIN_AASIN  :'A');
          SIN_MMSIN   = ConvN(PRV_MMSIN  :SIN_MMSIN  );
          SIN_DTUACT  = ConvN(PRV_DTUACT :SIN_DTUACT :'D');
          SIN_USERID  = ConvA(PRV_USERID :SIN_USERID );

          //--------------------------------------------------------------
          // Actualizar ou criar o registo
          //--------------------------------------------------------------
          IF (WIsToChange = 'S');
             IF (WAction = ACT#Insert);
                MONITOR;
                   WRITE RSINT118;
                ON-ERROR;
                   WCodErro = 'IOE1111'; // Erro na inserção do registo FSINT100
                ENDMON;
             ELSEIF (WAction = ACT#Update);
                MONITOR;
                   UPDATE RSINT118 %FIELDS(SIN_TIPMOV:SIN_DTMOV:SIN_DESCR:
                                           SIN_VALOR:SIN_VALR1:SIN_VALR3:
                                           SIN_VALR45:SIN_VLMIP:SIN_VLDTRA:
                                           SIN_CTAIND:SIN_SIT1:SIN_SIT2:
                                           SIN_SIT3:SIN_VLDMAT:SIN_VLDCOR:
                                           SIN_AASIN:SIN_MMSIN:SIN_DTUACT:
                                           SIN_USERID);
                ON-ERROR;
                   WCodErro = 'IOE1112'; // Erro na Alteração do registo FSINT100
                ENDMON;
             ENDIF;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina..: ActPagamentos
       // Objectivo..: Actualizar os Pagamentos de Sinistros Automóvel
       //==============================================================
       BEGSR ActPagamentos;
          //--------------------------------------------------------------
          // Obter o Código da Linha da Provisão
          //--------------------------------------------------------------
          CHAIN (WNordSin:PAG_NLINHA) RSINT128;
          IF (%FOUND(FSINT128));
             WAction  = ACT#Update;
             WIsToChange = 'N';
          ELSE;
             WIsToChange = 'S';
             WAction = ACT#Insert;
             SIN_NORDSIN = WNordSin;
             SIN_NLINHA  = ConvN(PAG_NLINHA :SIN_NLINHA );
          ENDIF;

          //--------------------------------------------------------------
          // ForCompr campos dos Pagamentos de Sinistro
          //--------------------------------------------------------------
          SIN_TIPMOV  = ConvN(PAG_TIPMOV :SIN_TIPMOV );
          SIN_DTMOV   = ConvN(PAG_DTMOV  :SIN_DTMOV  :'D');
          SIN_DESCR   = ConvA(PAG_DESCR  :SIN_DESCR  );
          SIN_VALOR   = ConvN(PAG_VALOR  :SIN_VALOR  );
          SIN_VALR1   = ConvN(PAG_VALR1  :SIN_VALR1  );
          SIN_VALR3   = ConvN(PAG_VALR3  :SIN_VALR3  );
          SIN_VALR45  = ConvN(PAG_VALR45 :SIN_VALR45 );
          SIN_VLMIP   = ConvN(PAG_VLMIP  :SIN_VLMIP  );
          SIN_VLDTRA  = ConvN(PAG_VLDTRA :SIN_VLDTRA );
          SIN_CTAIND  = ConvA(PAG_CTAIND :SIN_CTAIND );
          SIN_PAGFIN  = ConvA(PAG_PAGFIN :SIN_PAGFIN );
          SIN_P100    = ConvA(PAG_P100   :SIN_P100   );
          SIN_FORPAG  = ConvA(PAG_FORPAG :SIN_FORPAG );
          SIN_DTPAG   = ConvN(PAG_DTPAG  :SIN_DTPAG  :'D');
          SIN_DOCREF  = ConvA(PAG_DOCREF :SIN_DOCREF );
          SIN_CD08    = ConvA(PAG_CD08   :SIN_CD08   );
          SIN_BANCO   = ConvN(PAG_BANCO  :SIN_BANCO  );
          SIN_DEPEN   = ConvN(PAG_DEPEN  :SIN_DEPEN  );
          SIN_TIPENT  = ConvN(PAG_TIPENT :SIN_TIPENT );
          SIN_NUMENT  = ConvN(PAG_NUMENT :SIN_NUMENT );
          SIN_NOMBEN  = ConvA(PAG_NOMBEN :SIN_NOMBEN );
          SIN_NORBEN  = ConvA(PAG_NORBEN :SIN_NORBEN );
          SIN_LOCBEN  = ConvA(PAG_LOCBEN :SIN_LOCBEN );
          SIN_CPBEN   = ConvN(PAG_CPBEN  :SIN_CPBEN  );
          SIN_DTFACT  = ConvN(PAG_DTFACT :SIN_DTFACT :'D');
          SIN_AASIN   = ConvN(PAG_AASIN  :SIN_AASIN  :'A');
          SIN_MMSIN   = ConvN(PAG_MMSIN  :SIN_MMSIN  );
          SIN_NODOC   = ConvA(PAG_FILL3  :SIN_NODOC  );
          SIN_VLDMAT  = ConvN(PAG_VLDMAT :SIN_VLDMAT );
          SIN_VLDCOR  = ConvN(PAG_VLDCOR :SIN_VLDCOR );
          SIN_NRECIB  = ConvA(PAG_NRECIB :SIN_NRECIB );
          SIN_RECEMI  = ConvA(PAG_RECEMI :SIN_RECEMI );
          SIN_ENVIO   = ConvA(PAG_ENVIO  :SIN_ENVIO  );
          SIN_CHEMI   = ConvA(PAG_CHEMI  :SIN_CHEMI  );
          SIN_DTUACT  = ConvN(PAG_TIPENT :SIN_DTUACT :'D');
          SIN_USERID  = ConvA(PAG_USERID :SIN_USERID );

          //--------------------------------------------------------------
          // Actualizar ou criar o registo
          //--------------------------------------------------------------
          IF (WIsToChange = 'S');
             IF (WAction = ACT#Insert);
                MONITOR;
                   WRITE RSINT128;
                ON-ERROR;
                   WCodErro = 'IOE1111'; // Erro na inserção do registo FSINT100
                ENDMON;
             ELSEIF (WAction = ACT#Update);
                MONITOR;
                   UPDATE RSINT128 %FIELDS(SIN_TIPMOV:SIN_DTMOV:SIN_DESCR:
                                           SIN_VALOR:SIN_VALR1:SIN_VALR3:
                                           SIN_VALR45:SIN_VLMIP:SIN_VLDTRA:
                                           SIN_CTAIND:SIN_PAGFIN:SIN_P100:
                                           SIN_FORPAG:SIN_DTPAG:SIN_DOCREF:
                                           SIN_CD08:SIN_BANCO:SIN_DEPEN:
                                           SIN_TIPENT:SIN_NUMENT:SIN_NOMBEN:
                                           SIN_NORBEN:SIN_LOCBEN:SIN_CPBEN:
                                           SIN_DTFACT:SIN_AASIN:SIN_MMSIN:
                                           SIN_NODOC:SIN_VLDMAT:SIN_VLDCOR:
                                           SIN_NRECIB:SIN_RECEMI:SIN_ENVIO:
                                           SIN_CHEMI:SIN_DTUACT:SIN_USERID);
                ON-ERROR;
                   WCodErro = 'IOE1112'; // Erro na Alteração do registo FSINT100
                ENDMON;
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE

     P*=========================================================
     P* Processo..: ConvN
     P* Descrição.: Converter Campos numéricos, e validar se
     P*             existem alterações, em relação ao original.
     P*=========================================================
     P ConvN           B                   EXPORT
     D                 PI            15S 5
     D    PARMPRO                    15S 5 Const
     D    PARMSIN                    15S 5 Const
     D    TIPO                        1A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WNumero         S             15S 5
     D WTipo           S              1A
     D*===========================================================
      /FREE
       MONITOR;
          WNumero = PARMPRO;
       ON-ERROR;
          WNumero = *ZEROS;
       ENDMON;
       WTipo = 'G';
       IF (%PARMS() > 2);
          WTipo = TIPO;
       ENDIF;

       IF (WTipo = 'D') AND (WNumero > *ZEROS);
          WNumero += 20000000;
       ELSEIF (WTipo = 'A') AND (WNumero > *ZEROS);
         IF (WNumero >= 30);
            WNumero += 1900;
         ELSE;
            WNumero += 2000;
         ENDIF;
       ENDIF;
       IF (WNumero <> PARMSIN);
          WIsToChange = 'S';
       ENDIF;
       RETURN WNumero;
      /END-FREE
     P ConvN           E

     P*=======================================================
     P* Processo..: ConvA
     P* Descrição.: Converter Campos Alfanumericos
     P*=======================================================
     P ConvA           B                   EXPORT
     D                 PI           100A
     D    PARMPRO                   100A   Const
     D    PARMSIN                   100A   Const
     D*===========================================================
      /FREE
       IF (%TRIM(PARMPRO) <> %TRIM(PARMSIN));
          WIsToChange = 'S';
       ENDIF;
       RETURN PARMPRO;
      /END-FREE
     P ConvA           E
