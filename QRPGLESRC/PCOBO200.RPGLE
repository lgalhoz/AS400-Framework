     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO200
     H* Descrição.: Manutenção de ficheiros de Interfaces
     H*==================================================================
     FFINTP02501IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP025:RINTP02501)
     FFINTP023  IF   E           K DISK    PREFIX(INT_)
     FFINTP024  IF   E           K DISK    PREFIX(INT_)
     FFINTP020  UF   E           K DISK    PREFIX(INT_)
     FFINTP028  UF   E           K DISK    PREFIX(INT_)
     FFINTP027  UF   E           K DISK    PREFIX(INT_)
     FFINTP029  UF A E           K DISK    PREFIX(INT_)
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

     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Chamada Dinâmica a funções
     D ShowReciboStd   PR                  EXTPGM('PCOBO201')
     D   NumRecibo                    9S 0 CONST
     D   Modo                          N   CONST
     D   Chamador                     5S 0 CONST
     D
     D*==> Rotina para o registo Chamador
     D CallApolice     PR                  EXTPGM('PCOBO203')
     D   NumOrdem                     7S 0 CONST
     D   ModoIO                       1N   CONST
     D   WChamador                    5S 0 CONST
     D
     D*==> Mapa de Recibos cobrados
     D PrintCobrados   PR                  EXTPGM('RCOBO005')
     D   NumFile                      9S 0 CONST
     D   CodErro                      7A   CONST
     D
     D*==> Lista de Pastas de interfaces
     D AddFolder       PR
     D   Pasta                      100A   CONST
     D Lst#Pastas      DS                  QUALIFIED DIM(25)
     D   Nompasta                   100A
     D   Isempty                       N
     D
     D*==> Campos de Trabalho
     D WQtdeRegistos   S              3S 0
     D WMsgErro        S              7A
     D WNoFunction     S             10A
     D WSelected       S               N
     D WExtKeys        S             24A
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WCdEstado       S              2S 0
     D WIdx            S              3S 0
     D WModo           S              1A
     D PModo           S              1A
     D
     D REC#            S              4S 0
     D KTE#INTER       S              5S 0
     D KTE#RECIB       S              5S 0
     D KTE#RECIB2      S              5S 0
     D KTE#COMIS       S              5S 0
     D KTE#CMDET       S              5S 0
     D
     D*==> Constantes
     D MOD#Recibo      C                   'R'
     D MOD#Anomalia    C                   'A'
     D MOD#Interface   C                   'I'
     D EST#Aberto      C                   'A'
     D*==================================================================*
     C     *ENTRY        PLIST
     C                   PARM                    PModo
      /FREE

       //------------------------------------
       // Validar o acesso a esta função
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------------
       // Validar o modo de execução
       //----------------------------------------------
       MONITOR;
          WModo = PModo;
       ON-ERROR;
          WModo = *BLANKS;
       ENDMON;

       //----------------------------------------------
       // Sessão de Interfaces
       //----------------------------------------------
       KTE#INTER = $SetFiles('FINTP020');
       $addCriteria('Data fich..:':'DTFILE':'GE':'D');
       $addCriteria('Nome.......:':'NOFILE':'LK');
       $addCriteria('Companhia..:':'CCOMP':'EQ');
       $addCriteria('Tipo.......:':'TPFILE':'EQ');
       $SetLstFld('DTFILE':'Entrada':10:'C':'D');
       $SetLstFld('NOFILE':'Interface':40:'E');
       $SetLstFld('CCOMP':'Comp.':6:'C');
       $SetLstFld('QTREGIS':'Quantid.':10:'D');
       $SetPage(12);

       //----------------------------------------------
       // Sessão de Excepções
       //----------------------------------------------
       KTE#COMIS = $SetFiles('FINTP023');
       $addCriteria('Dat.Registo:':'DTEXCEP':'GE':'D');
       $addCriteria('Companhia..:':'CCOMP':'EQ');
       $addCriteria('Tipo.......:':'TPEXCEPT':'EQ');
       $SetLstFld('DTEXCEP':'Registo':10:'E':'D');
       $SetLstFld('CCOMP':'Comp.':6:'C');
       $SetLstFld('TPEXCEPT':'Tipo Excepção':20:'E');
       $SetLstFld('TPEXCEPT':'Situação':20:'E');
       $SetLstFld('QTREGIS':'Quantid.':10:'D');
       $SetPage(12);

       //----------------------------------------------
       // Recibos com erros nas Comissões ou outros
       //----------------------------------------------
       KTE#CMDET = $SetFiles('FINTP023 I INNER JOIN (FINTP025 L INNER JOIN +
                              FINTP028 R on (L.CDREGIS = R.CDREGIS)) ON +
                              (I.CDEXCEPT = L.CDEXCEPT)');
       $SetFields('R.CDREGIS,R.CDRAMO,R.NUAPOLOR,R.NUMRECOR,R.NOMSEG,+
                   R.NUMORDRC,R.NUMORDAP,R.CDESTAD,L.CDEXCEPT');
       $addCriteria('Recibo.....:':'R.NUMRECOR':'LK':'A');
       $addCriteria('Apolice....:':'R.NUAPOLOR':'LK':'D');
       $addCriteria('Segurado...:':'R.NOMSEG':'LK');
       $addCriteria('Num.Ordem..:':'R.NUMORDRC':'GE':'D');
       $SetLstFld('R.CDRAMO':'Ramo':5:'D');
       $SetLstFld('R.NUAPOLOR':'Apól.Orig':10:'D');
       $SetLstFld('R.NUMRECOR':'Recib.Original':15:'D');
       $SetLstFld('R.NUMORDRC':'N.Ordem':8:'D');
       $SetLstFld('R.NOMSEG':'Segurado':33:'E');
       $SetPage(12);

       //----------------------------------------------
       // Sessão de Recibos
       //----------------------------------------------
       KTE#RECIB = $SetFiles('FINTP028 R');
       $SetFields('R.CDREGIS,R.CDRAMO,R.NUAPOLOR,R.NUMRECOR,R.NOMSEG,+
                   R.NUMORDRC,R.NUMORDAP,R.CDESTAD');
       $addCriteria('Recibo.....:':'R.NUMRECOR':'LK':'A');
       $addCriteria('Apolice....:':'R.NUAPOLOR':'LK':'D');
       $addCriteria('Segurado...:':'R.NOMSEG':'LK');
       $addCriteria('Num.Ordem..:':'R.NUMORDRC':'GE':'D');
       $SetLstFld('R.CDRAMO':'Ramo':5:'D');
       $SetLstFld('R.NUAPOLOR':'Apól.Orig':10:'D');
       $SetLstFld('R.NUMRECOR':'Recib.Original':15:'D');
       $SetLstFld('R.NUMORDRC':'N.Ordem':8:'D');
       $SetLstFld('R.NOMSEG':'Segurado':33:'E');
       $SetPage(12);

       //----------------------------------------------
       // Sessão de Recibos usados nos Interfaces
       //----------------------------------------------
       KTE#RECIB2 = $SetFiles('FINTP020 I INNER JOIN (FINTP027 L INNER JOIN +
                             FINTP028 R on (L.CDREGIS = R.CDREGIS)) ON +
                             (I.CDFILE = L.CDFILE)');
       $SetFields('R.CDREGIS,R.CDRAMO,R.NUAPOLOR,R.NUMRECOR,R.NOMSEG,+
                   R.NUMORDRC,R.NUMORDAP,R.CDESTAD,L.CDFILE');
       $addCriteria('Recibo.....:':'R.NUMRECOR':'LK':'A');
       $addCriteria('Apolice....:':'R.NUAPOLOR':'LK':'D');
       $addCriteria('Segurado...:':'R.NOMSEG':'LK');
       $addCriteria('Num.Ordem..:':'R.NUMORDRC':'GE':'D');
       $SetLstFld('R.CDRAMO':'Ramo':5:'D');
       $SetLstFld('R.NUAPOLOR':'Apól.Orig':10:'D');
       $SetLstFld('R.NUMRECOR':'Recib.Original':15:'D');
       $SetLstFld('R.NUMORDRC':'N.Ordem':8:'D');
       $SetLstFld('R.NOMSEG':'Segurado':33:'E');
       $SetPage(12);
       //--------------------------------------------------------
       // Formatar valores por defeito
       //--------------------------------------------------------
       Lst#Pastas(1).IsEmpty  = *ON;
       WCdEstado = *ZEROS;

       IF (WModo = MOD#Recibo) OR (WModo = *BLANKS);
          $SetDBSession(KTE#RECIB);
       ELSEIF (WModo = MOD#Anomalia);
          $SetDBSession(KTE#COMIS);
       ELSEIF (WModo = MOD#Interface);
          $SetDBSession(KTE#INTER);
       ENDIF;

       ECR_DSCRITER = $getCritText();
       EXSR ShowInter;

       $RmvDBSession(KTE#INTER);
       $RmvDBSession(KTE#RECIB);
       $RmvDBSession(KTE#RECIB2);
       $RmvDBSession(KTE#COMIS);
       $RmvDBSession(KTE#CMDET);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ShowInter
       // Objectivo.: Apresentar as Interfaces
       //=========================================================
       BEGSR ShowInter;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             IF ($GetDBSession() = KTE#RECIB);
                DOW (WQtdeRegistos = *ZEROS);
                   IF (WCdEstado < 9);
                       WCdEstado += 1;
                       $SetCriteria('R.CDESTAD = ''' + %CHAR(WCdEstado) + '''');
                   ELSE;
                       $SetCriteria(*BLANKS);
                       WCdEstado = *ZEROS;
                       LEAVE;
                   ENDIF;
                   EXSR getRecords;
                ENDDO;
             ENDIF;

             IF ($GetDBSession() = KTE#INTER);
                WNoFunction = 'RCOBE20003';
             ELSEIF ($GetDBSession() = KTE#RECIB);
                WNoFunction = 'RCOBE20009';
             ELSEIF ($GetDBSession() = KTE#RECIB2);
                WNoFunction = 'RCOBE20007';
             ELSEIF ($GetDBSession() = KTE#COMIS);
                WNoFunction = 'RCOBE20011';
             ELSEIF ($GetDBSession() = KTE#CMDET);
                WNoFunction = 'RCOBE20013';
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             IF ($GetDBSession() = KTE#RECIB);
                IF (WCdEstado > 0);
                   ECR_DS#FNC = %TRIM(ECR_DS#FNC) + ' - ' +
                                $GetDescricao(88:%CHAR(WCdEstado));
                ELSE;
                   ECR_DS#FNC = %TRIM(ECR_DS#FNC) + ' - Todos';
                ENDIF;
                ECR_DS#FNC = $AlignText(ECR_DS#FNC:57:'C');
             ENDIF;
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RCOBE20000;
             WRITE RCOBE20099;
             WRITE RCOBE20098;
             EXFMT RCOBE20003;
             IF ($ValidKey(Func#Key:KEY#F05));
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
                IF ($GetDBSession() = KTE#RECIB) OR
                   ($GetDBSession() = KTE#RECIB2);
                   EXSR VerOriginal;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F07));
                IF ($GetDBSession() = KTE#RECIB);
                   IF (WCdEstado < 9);
                       WCdEstado += 1;
                       $SetCriteria('R.CDESTAD = ''' + %CHAR(WCdEstado) + '''');
                   ELSE;
                       $SetCriteria(*BLANKS);
                       WCdEstado = *ZEROS;
                   ENDIF;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F09));
                IF ($GetDBSession() = KTE#RECIB);
                   $SetDBSession(KTE#COMIS);
                   ECR_DSCRITER = $getCritText();
                ELSEIF ($GetDBSession() = KTE#COMIS);
                   $SetDBSession(KTE#INTER);
                   $SetCriteria(*BLANKS);
                   ECR_DSCRITER = $getCritText();
                ELSEIF ($GetDBSession() = KTE#INTER);
                   $SetDBSession(KTE#RECIB);
                   WCdEstado = *ZEROS;
                   $SetCriteria(*BLANKS);
                   ECR_DSCRITER = $getCritText();
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F10));
                PrintCobrados(ECR_CDFILE:WMsgErro);
                IF (WMsgErro = *BLANKS);
                   WmsgErro = 'COB0061';
                ENDIF;
                $ShowDialog(WMsgErro);
             ELSEIF ($ValidKey(Func#Key:KEY#F12));
                IF ($GetDBSession() = KTE#RECIB2);
                   $SetDBSession(KTE#INTER);
                   ECR_DSCRITER = $getCritText();
                   *IN12 = *OFF;
                ELSEIF ($GetDBSession() = KTE#CMDET);
                   $SetDBSession(KTE#COMIS);
                   ECR_DSCRITER = $getCritText();
                   *IN12 = *OFF;
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
          WQtdeRegistos = *ZEROS;

          //-------------------------------------------------------
          // Obter os Registos
          //-------------------------------------------------------
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                WQtdeRegistos += 1;
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();
                ECR_LST#DET = $GetLstLine();
                REC# += 1;

                //---------------------------------------------------
                // Assinalar Interfaces Anuladas
                //---------------------------------------------------
                *IN60 = *OFF; // Vermelho
                *IN62 = *OFF; // Branco
                *IN64 = *OFF; // Amarelo
                IF ($GetDBSession() = KTE#INTER);
                   ECR_CDFILE  = $GetFieldNum('CDFILE');
                   IF ($GetField('CDESTAD') = 'A');
                      *IN60 = *ON; // Vermelho
                   ELSEIF ($GetField('CDESTAD') = 'F');
                      *IN62 = *ON; // Branco
                   ENDIF;
                ELSEIF ($GetDBSession() = KTE#COMIS);
                   ECR_CDEXCEPT = $GetFieldNum('CDEXCEPT');
                   %SUBST(ECR_LST#DET:19:20) = $GetDescAbrev(99:
                                                       $GetField('TPEXCEPT'));
                   %SUBST(ECR_LST#DET:40:10) = $GetDescAbrev(100:
                                                       $GetField('CDESTADO'));
                ELSEIF ($GetDBSession() = KTE#RECIB) OR
                  ($GetDBSession() = KTE#CMDET);
                   ECR_CDFILE  = $GetFieldNum('L.CDFILE');
                   ECR_CDREGIS = $GetFieldNum('R.CDREGIS');
                   ECR_NUMORDAP= $GetFieldNum('R.NUMORDAP');
                   IF ($GetField('R.CDESTAD') <> '1');
                      *IN62 = *ON; // Branco
                   ENDIF;
                   IF ($GetField('R.CDESTAD') = '8') OR
                     ($GetField('R.CDESTAD') = '9') OR
                     ($GetField('R.CDESTAD') = '7');
                      *IN62 = *OFF; // Branco
                      *IN64 = *ON;  // Vermelho
                   ENDIF;
                   SETLL (ECR_CDREGIS) RINTP02501;
                   READE (ECR_CDREGIS) RINTP02501;
                   DOW (NOT %EOF(FINTP02501));
                      CHAIN (INT_CDEXCEPT) RINTP023;
                      IF (INT_CDESTAD = EST#Aberto);
                         *IN62 = *OFF; // Branco
                         *IN60 = *ON;  // Cobrado
                         LEAVE;
                      ENDIF;
                      READE (ECR_CDREGIS) RINTP02501;
                   ENDDO;
                ELSEIF ($GetDBSession() = KTE#RECIB2);
                   ECR_CDFILE  = $GetFieldNum('L.CDFILE');
                   ECR_CDREGIS = $GetFieldNum('R.CDREGIS');
                   ECR_NUMORDAP= $GetFieldNum('R.NUMORDAP');
                   IF ($GetField('R.CDESTAD') <> '1');
                      *IN62 = *ON; // Branco
                   ENDIF;
                   IF ($GetField('R.CDESTAD') = '8') OR
                     ($GetField('R.CDESTAD') = '9') OR
                     ($GetField('R.CDESTAD') = '7');
                      *IN62 = *OFF; // Branco
                      *IN64 = *ON;  // Vermelho
                   ENDIF;
                ENDIF;
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
             IF ($ValidOption(ECR_OPTION:'2':S#PGM:WNoFunction));
                IF ($GetDBSession() = KTE#INTER);
                   $SetDBSession(KTE#RECIB2);
                   $SetCriteria('(I.CDFILE='''+%CHAR(ECR_CDFILE)+''')');
                   ECR_DSCRITER = $getCritText();
                ELSEIF ($GetDBSession() = KTE#COMIS);
                   $SetDBSession(KTE#CMDET);
                   $SetCriteria('(I.CDEXCEPT='''+%CHAR(ECR_CDEXCEPT)+''') +
                                AND (L.CDESTAD = ''A'')');
                   ECR_DSCRITER = $getCritText();
                ELSEIF ($GetDBSession() = KTE#CMDET);
                   ShowReciboStd(ECR_CDREGIS:*OFF:12345);
                ELSEIF ($GetDBSession() = KTE#RECIB);
                   ShowReciboStd(ECR_CDREGIS:*OFF:12346);
                ELSEIF ($GetDBSession() = KTE#RECIB2);
                   ShowReciboStd(ECR_CDREGIS:*OFF:12346);
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'4':S#PGM:WNoFunction));
                IF (ECR_NUMORDAP > *ZEROS);
                   CallApolice(ECR_NUMORDAP:*ON:11111);
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'6':S#PGM:WNoFunction));
                EXSR ShowDetail; //
                IF (*IN12);      //
                   *IN12 = *OFF; //
                   LEAVE;        //
                ENDIF;           //
             ELSEIF ($ValidOption(ECR_OPTION:'8':S#PGM:WNoFunction));
                EXSR VerOriginal;
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
       // Subrotina.: VerOriginal
       // Objectivo.: Apresentar o Interface Original
       //
       // DSPF STMF('/interfaces/recebido/REC_EMITIDOS_05703_20140328.csv')
       //
       //========================================================
       BEGSR VerOriginal;
          Addfolder('/interfaces/tratado/done/');
          AddFolder('/interfaces/recebido/');
          AddFolder('/interfaces/JMClubeTAP/');
          Addfolder('/interfaces/JMSede/');

          //---------------------------------------------
          // Procurar o ficheiro nas Pastas
          //---------------------------------------------
          CHAIN(N) (ECR_CDFILE) RINTP020;
          IF (%FOUND(FINTP020));
             Widx = 1;
             DOW (NOT Lst#Pastas(Widx).IsEmpty);
                MONITOR;
                   WStdCmd = 'DSPF STMF('''+%TRIM(Lst#Pastas(Widx).NomPasta) +
                              %TRIM(INT_NOFILE)+''')';
                   WStdLen = %LEN(WStdCmd);
                   shell(WStdCmd:WStdLen);
                   LEAVESR;
                ON-ERROR;
                ENDMON;
                Widx += 1;
             ENDDO;
             // Afixar mensagem de erro a informar que não encontrou
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
             IF ($GetDBSession() = KTE#INTER);
                WNoFunction = 'RCOBE20005'; // Mostrar o Detalhe
             ELSEIF ($GetDBSession() = KTE#COMIS);
                WNoFunction = 'RCOBE20006'; // Mostrar o Detalhe
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WExtKeys);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WExtKeys);
             ECR_S#PGM   = S#PGM;
             WRITE RCOBE20000;
             WRITE RCOBE20099;
             IF ($GetDBSession() = KTE#INTER);
                EXFMT RCOBE20005;
             ELSEIF ($GetDBSession() = KTE#COMIS);
                EXFMT RCOBE20006;
             ENDIF;
             IF ($ValidKey(Func#Key:KEY#F22));
                IF ($ShowDialog('INT0007':*BLANKS:'NS') = 'S');
                   EXSR RmvInterface;
                   LEAVESR;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F21));
                IF ($ShowDialog('INT0009':*BLANKS:'NS') = 'S');
                   EXSR Cobrar;
                   LEAVESR;
                ENDIF;
             ELSEIF (Func#Key = KEY#Enter);
                LEAVE;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: LoadData
       // Objectivo.: Carregar a informação do Interface
       //========================================================
       BEGSR LoadData;
          WExtKeys = *ZEROS;
          IF ($GetDBSession() = KTE#INTER);
             //-------------------------------------------------------
             // Carrega informação sobre o Interface
             //-------------------------------------------------------
             CHAIN(N) (ECR_CDFILE) RINTP020;
             IF (%FOUND());
                ECR_DSESTAD  = $GetDescricao(92:INT_CDESTAD);
                %SUBST(WExtKeys:21:1) = '1';
                %SUBST(WExtKeys:22:1) = '1';
                IF (INT_CDESTAD = 'P');
                   %SUBST(WExtKeys:22:1) = '0';
                ELSEIF (INT_CDESTAD = 'F') AND (INT_TPFILE = 'PAG');
                   %SUBST(WExtKeys:21:1) = '0';
                ENDIF;
                ECR_CDFILE   = INT_CDFILE;
                ECR_NOFILE   = INT_NOFILE;
                ECR_CCOMP    = INT_CCOMP;
                ECR_DSCOMP  = $GetDescricao(33:%TRIM(%EDITW(ECR_CCOMP:'0   ')));
                ECR_ORNAME   = INT_ORNAME;
                ECR_DTFILE   = $CvtData(INT_DTFILE:'YYMD':'DMYY');
                ECR_QTDREGIS = INT_QTREGIS;
                ECR_CDCONTA  = INT_CDCONTA;
                ECR_TPFILE   = INT_TPFILE;
                ECR_DSTIPO   = $GetDescricao(90:ECR_TPFILE);
             ENDIF;
          ELSEIF ($GetDBSession() = KTE#COMIS);
             //-------------------------------------------------------
             // Carrega informação sobre os erros de Comissão
             //-------------------------------------------------------
             *IN65 = *OFF;
             CHAIN (ECR_CDEXCEPT) RINTP023;
             IF (%FOUND());
                %SUBST(WExtKeys:21:1) = '1';
                %SUBST(WExtKeys:22:1) = '1';
                IF (INT_CDESTAD = 'P');
                   %SUBST(WExtKeys:22:1) = '0';
                ELSEIF (INT_CDESTAD = 'F') AND (INT_TPFILE = 'PAG');
                   %SUBST(WExtKeys:21:1) = '0';
                ENDIF;
                ECR_CCOMP    = INT_CCOMP;
                ECR_DSCOMP  = $GetDescricao(33:%TRIM(%EDITW(ECR_CCOMP:'0   ')));
                ECR_DSTIPO   = $GetDescricao(99:INT_TPEXCEPT);
                ECR_DTEXCEPT = $CvtData(INT_DTEXCEP:'YYMD':'DMYY');
                ECR_QTREGIS  = INT_QTREGIS;
                ECR_DSESTAD  = $GetDescricao(100:INT_CDESTADO);

                CHAIN (ECR_CDEXCEPT) RINTP024;
                IF (%FOUND(FINTP024));
                   *IN65 = *ON;
                   ECR_DFTCOM   = INT_DFTOCOM;
                   ECR_NURECIBO = INT_NURECIBO;
                   ECR_DTREGULA = $CvtData($DateToInt(INT_DTREGULA):
                                           'YYMD':'DMYY');
                   ECR_USRREGUL = INT_USRREGUL;
                ENDIF;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: RmvInterface
       // Objectivo.: Eliminar o ficheiro de Interface
       //========================================================
       BEGSR RmvInterface;
          //---------------------------------------------------
          // Validar se existe pelo menos um recibos gerado
          //---------------------------------------------------
          WMsgErro = *BLANKS;
          SETLL (ECR_CDFILE) RINTP027;
          READE (ECR_CDFILE) RINTP027;
          DOW NOT (%EOF(FINTP027));
             CHAIN(N) (INT_CDREGIS) RINTP028;
             IF (%FOUND(FINTP028));
                IF (INT_CDESTAD <> '1');
                   WMsgErro = 'INT0008';
                   LEAVE;
                ENDIF;
             ENDIF;
             READE (ECR_CDFILE) RINTP027;
          ENDDO;

          //---------------------------------------------------
          // Eliminar ficheiros Standard e Ligações
          //---------------------------------------------------
          IF (WMsgErro = *BLANKS);
             SETLL (ECR_CDFILE) RINTP027;
             READE (ECR_CDFILE) RINTP027;
             DOW NOT (%EOF(FINTP027));
                CHAIN (INT_CDREGIS) RINTP028;
                IF (%FOUND(FINTP028));
                   SETLL (INT_CDREGIS) RINTP029;
                   READE (INT_CDREGIS) RINTP029;
                   DOW  (NOT %EOF(FINTP029));
                      DELETE RINTP029; // Operações do Recibos
                      READE (INT_CDREGIS) RINTP029;
                   ENDDO;
                   DELETE RINTP028; // Recibo
                ENDIF;
                DELETE RINTP027; // Relação com o ficheiro
                READE (ECR_CDFILE) RINTP027;
             ENDDO;
          ENDIF;

          //----------------------------------------------
          // Anular ficheiro de Interface
          //----------------------------------------------
          CHAIN (ECR_CDFILE) RINTP020;
          IF (%FOUND(FINTP020));
             IF (WMsgErro = *BLANKS);
                INT_CDESTAD = 'A'; // Anulado
             ELSE;
                $ShowDialog(WMsgErro);
                INT_CDESTAD = 'F'; // C/Recibos Gerados
             ENDIF;
             UPDATE RINTP020 %FIELDS(INT_CDESTAD);
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: Cobrar
       // Objectivo.: Eliminar o ficheiro de Interface
       //========================================================
       BEGSR Cobrar;
          //---------------------------------------------------
          // Validar se os recibos tem todos número de ordem
          //---------------------------------------------------
          WMsgErro = *BLANKS;
          SETLL (ECR_CDFILE) RINTP027;
          READE (ECR_CDFILE) RINTP027;
          DOW NOT (%EOF(FINTP027));
             CHAIN(N) (INT_CDREGIS) RINTP028;
             IF (%FOUND(FINTP028));
                IF (INT_CDESTAD = '1') OR (INT_CDESTAD = '2')
                  OR (INT_CDESTAD = '3');
                   WMsgErro = 'INT0010';
                   LEAVE;
                ELSEIF (INT_CDESTAD = '7');
                   WMsgErro = 'INT0011';
                   LEAVE;
                ELSEIF (INT_CDESTAD = '8');
                   WMsgErro = 'INT0012';
                   LEAVE;
                ENDIF;
             ENDIF;
             READE (ECR_CDFILE) RINTP027;
          ENDDO;

          //---------------------------------------------------
          // Classificar Recibos para poderem ser Cobrados
          //---------------------------------------------------
          IF (WMsgErro = *BLANKS);
             SETLL (ECR_CDFILE) RINTP027;
             READE (ECR_CDFILE) RINTP027;
             DOW NOT (%EOF(FINTP027));
                CHAIN (INT_CDREGIS) RINTP028;
                IF (%FOUND(FINTP028));
                   IF (INT_CDESTAD <> '6');
                      INT_CDESTAD = '5'; // Marcar como Cobrado
                      UPDATE RINTP028 %FIELDS(INT_CDESTAD);

                      //-----------------------------------------------
                      // Actualizar Operaçao
                      //-----------------------------------------------
                      INT_NUOPERA  = $GetNextKey('FINTP029':'NUOPERA':
                                    '(CDREGIS = '''+%CHAR(INT_CDREGIS)+''')');
                      INT_CDERRO  = WMsgErro;
                      INT_DTOPERA = %DATE();
                      INT_HROPERA = %TIME();
                      INT_USOPERA = S#USR;
                      WRITE RINTP029;
                   ENDIF;
                ENDIF;
                READE (ECR_CDFILE) RINTP027;
             ENDDO;
             CHAIN (ECR_CDFILE) RINTP020;
             INT_CDESTAD = 'I'; // Intrução de Cobrança
             UPDATE RINTP020 %FIELDS(INT_CDESTAD);
          ELSE;
            $ShowDialog(WMsgErro);
          ENDIF;
       ENDSR;
      /END-FREE

     P*==================================================================
     P* Processo..: AddFolder
     P* Descrição.: Adicionar uma nova Pasta á lista
     P*==================================================================
     P AddFolder       B                   EXPORT
     D AddFolder       PI
     D   Pasta                      100A   CONST
     D
     D*==> Campos de Trabalho
     D Widx            S              3S 0
     D*=================================================================
      /FREE
         //--------------------------------------
         // Procurar a existência da pasta
         //--------------------------------------
         Widx = 1;
         DOW (NOT Lst#Pastas(Widx).IsEmpty);
            Widx += 1;
         ENDDO;
         //--------------------------------------
         // Actualiar elemento na listagem
         //--------------------------------------
          Lst#Pastas(Widx).Nompasta = Pasta;
          Lst#Pastas(Widx).IsEmpty  = *OFF;
          WIdx += 1;
          Lst#Pastas(Widx).IsEmpty  = *ON;
      /END-FREE
     P AddFolder       E
