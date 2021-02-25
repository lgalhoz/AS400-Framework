     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO200
     H* Descrição.: Manutenção de fuicheiros de Interfaces
     H*==================================================================
     FFINTP020  IF   E           K DISK    PREFIX(INT_)
     FFCOBE200  CF   E             WORKSTN SFILE(RCOBE20002:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================*
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SGRLO013

     D*==> Chamada Dinâmica a funções
     D CallDynamic     PR                  EXTPGM(WDynamic)
     D   FileCode                    10P 0 Const Options(*NOPASS)
     D WDynamic        S             10A
     D
     D*==> Campos de Trabalho
     D WNoFunction     S             10A
     D WSelected       S               N
     D WExtKeys        S             24A
     D
     D REC#            S              4S 0
     D KTE#INTER       S              5S 0
     D KTE#RECIB       S              5S 0
     D*==================================================================*
      /FREE

       //------------------------------------
       // Validar o acesso a esta função
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------------
       // Sessão de Interfaces
       //----------------------------------------------
       KTE#INTER = $SetFiles('FINTP020');
       $addCriteria('Data fich..:':'DTFILE':'GE':'D');
       $addCriteria('Nome.......:':'NOFILE':'LK');
       $addCriteria('Companhia..:':'CCOMP':'EQ');
       $addCriteria('Tipo.......:':'TPFILE':'EQ');
       $SetLstFld('NOFILE':'Interface':40:'E');
       $SetLstFld('CCOMP':'Comp.':6:'C');
       $SetLstFld('QTREGIS':'Quantid.':10:'D');
       $SetPage(12);

       //----------------------------------------------
       // Sessão de Recibos
       //----------------------------------------------
       KTE#RECIB = $SetFiles('FINTP020 I INNER JOIN (FINTP027 L INNER JOIN +
                              FINTP028 R on (L.CDREGIS = R.CDREGIS)) ON +
                              (I.CDFILE = L.CDFILE)');
       $SetFields('R.CDREGIS,R.CDRAMO,R.NUAPOL,R.NUMREC,R.NOMSEG');
       $addCriteria('Recibo.....:':'R.NUMREC':'GE':'A');
       $addCriteria('Apolice....:':'R.NUAPOL':'EQ':'D');
       $addCriteria('Segurado...:':'R.NOMSEG':'LK');
       $SetLstFld('R.CDRAMO':'Ramo':5:'D');
       $SetLstFld('R.NUAPOL':'Apólice':10:'D');
       $SetLstFld('R.NUMREC':'Recibo':10:'D');
       $SetLstFld('R.NOMSEG':'Segurado':40:'E');
       $SetPage(12);

       $SetDBSession(KTE#INTER);
       ECR_DSCRITER = $getCritText();
       EXSR ShowInter;

       $RmvDBSession(KTE#INTER);
       $RmvDBSession(KTE#RECIB);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ShowInter
       // Objectivo.: Apresentar as Interfaces
       //=========================================================
       BEGSR ShowInter;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             IF ($GetDBSession() = KTE#INTER);
                WNoFunction = 'RCOBE20003';
             ELSE;
                WNoFunction = 'RCOBE20007';
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RCOBE20000;
             WRITE RCOBE20099;
             WRITE RCOBE20098;
             EXFMT RCOBE20003;
             IF (Func#Key = KEY#F05);
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF (Func#Key = KEY#F12);
                IF ($GetDBSession() = KTE#RECIB);
                   $SetDBSession(KTE#INTER);
                   ECR_DSCRITER = $getCritText();
                   *IN12 = *OFF;
                ENDIF;
             ELSEIF (Func#Key = KEY#F22);
                IF ($GetDBSession() = KTE#RECIB);
                   // Anular Interface
                ENDIF;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR SelRec;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          //-------------------------------------------------------
          // Inicializar os ReEgistos
          //-------------------------------------------------------
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RCOBE20003;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã

          //-------------------------------------------------------
          // Obter os Registos
          //-------------------------------------------------------
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();
                ECR_LST#DET = $GetLstLine();
                ECR_CDFILE  = $GetFieldNum('CDFILE');
                REC# += 1;

                //---------------------------------------------------
                // Validar existencia de erros
                //---------------------------------------------------
                // *IN60 = *ON;
                // *IN80 = *OFF;

                ECR_RRN = 1;
                WRITE RCOBE20002;
                *IN52 = *ON;       // Encontrou registos
                $getNext();
             ENDDO;
          ENDIF;
          *IN54 = $IsLastPage();   // Indicação de Fim de Página
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          WSelected = *OFF;
          READC RCOBE20002;
          DOW (NOT %EOF());
             WSelected = *ON;
             IF ($ValidOption(ECR_OPTION:'2'));
                IF ($GetDBSession() = KTE#INTER);
                   EXSR ShowDetail;
                   IF (*IN12);
                      *IN12 = *OFF;
                      LEAVE;
                   ENDIF;
                ENDIF;
             ELSEIF (ECR_OPTION='6');
                IF ($GetDBSession() = KTE#INTER);
                   $SetDBSession(KTE#RECIB);
                   $SetCriteria('(I.CDFILE='''+%CHAR(ECR_CDFILE)+''')');
                   ECR_DSCRITER = $getCritText();
                ENDIF;
             ENDIF;
             READC RCOBE20002;
          ENDDO;
          IF (NOT WSelected);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ShowDetail
       // Objectivo.: Apresentar o detalhe do registo
       //========================================================
       BEGSR ShowDetail;
          *IN12 = *OFF;
          EXSR LoadData;
          DOW (NOT *IN03 AND NOT *IN12);
             WNoFunction = 'RCOBE20005'; // Mostrar o Detalhe
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WExtKeys);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WExtKeys);
             ECR_S#PGM   = S#PGM;
             WRITE RCOBE20000;
             WRITE RCOBE20099;
             EXFMT RCOBE20005;
             IF (Func#Key = KEY#Enter);
                LEAVE;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: LoadData
       // Objectivo.: Carregar a informação do Interface
       //========================================================
       BEGSR LoadData;
          CHAIN (ECR_CDFILE) RINTP020;
          IF (%FOUND());
             ECR_CDFILE   = INT_CDFILE;
             ECR_NOFILE   = INT_NOFILE;
             ECR_CCOMP    = INT_CCOMP;
             ECR_DSCOMP   = *BLANKS;
             ECR_ORNAME   = INT_ORNAME;
             ECR_DTFILE   = $CvtData(INT_DTFILE:'YYMD':'DMYY');
             ECR_QTDREGIS = INT_QTREGIS;
             ECR_CDCONTA  = INT_CDCONTA;
             ECR_TPFILE   = INT_TPFILE;
             ECR_DSTIPO   = *BLANKS;
          ENDIF;
       ENDSR;
      /END-FREE
