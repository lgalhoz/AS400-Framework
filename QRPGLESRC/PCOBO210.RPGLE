     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO210
     H* Descri��o.: Gest�o de contactos
     H*
     H*--> Antes de Compilar, fazer:
     H* CRTDUPOBJ OBJ("fcobp010") FROMLIB("DemoComp") OBJTYPE(*FILE) TOLIB(QTEMP) NEWOBJ(fcobp010)
     H* CRTDUPOBJ OBJ("fcobp011") FROMLIB("DemoComp") OBJTYPE(*FILE) TOLIB(QTEMP) NEWOBJ(fcobp011)
     H* CRTDUPOBJ OBJ("fcobp012") FROMLIB("DemoComp") OBJTYPE(*FILE) TOLIB(QTEMP) NEWOBJ(fcobp012)
     H*==================================================================
     FFCOBP010  UF   E           K DISK    PREFIX(COB_)
     F                                     EXTFILE('"DemoComp"/"fcobp010"')
     F                                     RENAME(FCOBP010:RCOBP010)
     FFCOBP011  UF   E           K DISK    PREFIX(COB_)
     F                                     EXTFILE('"DemoComp"/"fcobp011"')
     F                                     RENAME(FCOBP011:RCOBP011)
     FFCOBP012  UF   E           K DISK    PREFIX(COB_)
     F                                     EXTFILE('"DemoComp"/"fcobp012"')
     F                                     RENAME(FCOBP012:RCOBP012)
     F
     FFINTP028  IF   E           K DISK    PREFIX(INT_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FMEDIADOR  IF   F  128     3AIDISK    KEYLOC(4) EXTFILE('SD.1.CR')
     FRECIBO    IF   F  326     7AIDISK    KEYLOC(270) EXTFILE('SD.1.REC')
     FAPOL_ACT  IF   F  320     7AIDISK    KEYLOC(1) EXTFILE('SD.1.AP')
     FAPOL_HST  IF   F  320     7AIDISK    KEYLOC(1) EXTFILE('SD.1.APM')
     FFCATP205  IF   E           K DISK    PREFIX(CAT_)
     F
     FFCOBE210  CF   E             WORKSTN SFILE(RCOBE21002:REC#)
     F                                     SFILE(RCOBE21004:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================*
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SGRLO013

     D*==> Chamada Din�mica a fun��es
     D ShowReciboStd   PR                  EXTPGM('PCOBO201')
     D   NumRecibo                    9S 0 CONST
     D   Modo                          N   CONST
     D   Chamador                     5S 0 CONST
     D
     D*==> Formatos de Registo
     D R#MEDIADOR    E DS                  EXTNAME('SD.1.CR') PREFIX('MED_')
     D R#RECIBO      E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D R#APOLICE     E DS                  EXTNAME('SD.1.AP') PREFIX('AP_')
     D
     D*==> Campos de Trabalho
     D WMsgErro        S              7A
     D WNoFunction     S             10A
     D WSelected       S               N
     D WExtKeys        S             24A
     D WSelActivas     S               N
     D WNumOrdemAP     S              7S 0
     D WNumOrdemRC     S              7S 0
     D WModoIO         S              1A
     D WCdRamo         S              3S 0
     D WTipLista       S              1A
     D
     D REC#            S              4S 0
     D
     D KTE#AGEND       S              5S 0
     D KTE#OPERA       S              5S 0
     D
     D MOD#Consulta    C                   'R'
     D MOD#Altera      C                   'C'
     D
     D ENT#Recibo      C                   'R'
     D ENT#Apolice     C                   'A'
     D ENT#Interface   C                   'I'
     D ENT#CMPSeguro   C                   'L'
     D
     D LST#Geral       C                   'G'
     D LST#Contactos   C                   'C'
     D*==================================================================*
      /FREE

       //------------------------------------
       // Validar o acesso a esta fun��o
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;
       WTipLista = LST#Geral;

       //----------------------------------------------
       // Opera��es Agendadas
       //----------------------------------------------
       WModoIO = MOD#Consulta;
       WSelActivas = *ON;
       KTE#AGEND = $SetFiles('"DemoComp"/"fcobp010"');
       $addCriteria('Agenda.....:':'DTPXCNT':'GE');
       $addCriteria('Responsavel:':'CDUSER':'GE');
       $SetCriteria('ICESTAD = ''A''');
       $SetLstFld('DTPXCNT':'Agenda':10:'D':'D');
       $SetLstFld('NUORDEM':'Chave':10:'D');
       $SetLstFld('TPENTID':'Entidade':18:'E');
       $SetLstFld('MTCONT':'Motivo':20:'E');
       $SetPage(12);

       //----------------------------------------------
       // Hist�rico de Ac��es Realizadas
       //----------------------------------------------
       KTE#OPERA = $SetFiles('"DemoComp"/"fcobp010" A INNER +
                              JOIN "DemoComp"/"fcobp011" O +
                              ON (A.CDCONT = O.CDCONT)');

       $SetFields('O.TPCONT,O.DTCONT,O.USCONT,O.TPPXCNT,O.DTPXCNT,+
                   O.NUOPERA');
       $addCriteria('Data.......:':'O.DTPXCNT':'GE':'A');
       $SetLstFld('O.TPPXCNT':'Forma':20:'E');
       $SetLstFld('O.DTPXCNT':'Agendado':10:'D':'D');
       $SetLstFld('O.TPCONT':'Forma':20:'E');
       $SetLstFld('O.DTCONT':'Realizado':10:'D':'D');
       $SetLstFld('O.USCONT':'Utilizador':10:'E');
       $SetPage(12);

       $SetDBSession(KTE#AGEND);
       ECR_DSCRITER = $getCritText();
       EXSR ShowInter;

       $RmvDBSession(KTE#AGEND);
       $RmvDBSession(KTE#OPERA);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ShowInter
       // Objectivo.: Apresentar as Interfaces
       //=========================================================
       BEGSR ShowInter;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             IF ($GetDBSession() = KTE#AGEND);
                IF (WSelActivas);
                   WNoFunction = 'RCOBE21003';
                ELSE;
                   WNoFunction = 'RCOBE21009';
                ENDIF;
             ELSE;
                WNoFunction = 'RCOBE21007';
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RCOBE21000;
             WRITE RCOBE21099;
             WRITE RCOBE21098;
             EXFMT RCOBE21003;
             IF ($ValidKey(Func#Key:KEY#F05));
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF ($ValidKey(Func#Key:KEY#F09));
                IF ($GetDBSession() = KTE#AGEND);
                   IF (WSelActivas);
                      WSelActivas = *OFF;
                      $SetCriteria('ICESTAD = ''F''');
                   ELSE;
                      WSelActivas = *ON;
                      $SetCriteria('ICESTAD = ''A''');
                   ENDIF;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F12));
                IF ($GetDBSession() = KTE#OPERA);
                   $SetDBSession(KTE#AGEND);
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
       // Objectivo.: Obter todos os registos de tabelas dispon�v
       //========================================================
       BEGSR getRecords;
          //-------------------------------------------------------
          // Inicializar os ReEgistos
          //-------------------------------------------------------
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecr�
          *In52     = *On;      // Mostrar a Lista de Ecr�
          IF (WTipLista = LST#Geral);
             WRITE RCOBE21003;     // Limpar Lista de Fun��es
          ELSEIF (WTipLista = LST#Contactos);
             WRITE RCOBE21005;     // Limpar Lista de Fun��es
          ENDIF;
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecr�

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
                ECR_CDCONT  = $GetFieldNum('CDCONT');
                ECR_NUOPERA = $GetFieldNum('O.NUOPERA');
                REC# += 1;

                //---------------------------------------------------
                // Assinalar Interfaces Anuladas
                //---------------------------------------------------
                *IN60 = *OFF; // Vermelho
                *IN62 = *OFF; // Branco
                *IN64 = *OFF; // Amarelo
                IF (WTipLista = LST#Geral);
                   IF ($GetDBSession() = KTE#AGEND);
                      %SUBST(ECR_LST#DET:23:18) = $Getdescricao(97:
                                                  %TRIM($GetField('TPENTID')));
                      %SUBST(ECR_LST#DET:42:20) = $Getdescricao(95:
                                                  %TRIM($GetField('MTCONT')));
                   ELSE;
                      %SUBST(ECR_LST#DET:1:20) = $Getdescricao(96:
                                                 %TRIM($GetField('O.TPPXCNT')));
                      %SUBST(ECR_LST#DET:33:20) = $Getdescricao(96:
                                                  %TRIM($GetField('O.TPCONT')));
                   ENDIF;
                ENDIF;
                ECR_RRN = 1;
                IF (WTipLista = LST#Geral);
                   WRITE RCOBE21002;
                ELSE;
                   WRITE RCOBE21004;
                ENDIF;
                *IN52 = *ON;       // Encontrou registos
                $getNext();
             ENDDO;
          ENDIF;
          *IN54 = $IsLastPage();   // Indica��o de Fim de P�gina
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          WSelected = *OFF;
          READC RCOBE21002;
          DOW (NOT %EOF());
             WSelected = *ON;
             IF ($ValidOption(ECR_OPTION:'2':S#PGM:WNoFunction));
                IF ($GetDBSession() = KTE#AGEND);
                   EXSR ShowDetail;
                   IF (*IN12);
                      *IN12 = *OFF;
                      LEAVE;
                   ENDIF;
                ELSE;
                // ShowReciboStd(ECR_CDREGIS:*OFF:12345);
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'6':S#PGM:WNoFunction));
             // IF ($GetDBSession() = KTE#AGEND);
                   $SetDBSession(KTE#OPERA);
                   $SetCriteria('(O.CDCONT='''+%CHAR(ECR_CDCONT)+''')');
                   ECR_DSCRITER = $getCritText();
             // ENDIF;
             ENDIF;
             READC RCOBE21002;
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
          EXSR getRecords;
          DOW (NOT *IN03 AND NOT *IN12);
             IF (WModoIO = MOD#Consulta);
             // ECR_GRPHEADER = 'Registo do �ltimo contacto:';
                WNoFunction = 'RCOBE21005'; // Mostrar o Detalhe
                *IN50 = *OFF;
             ELSE;
             // ECR_GRPHEADER = 'Registo do contacto:';
                WNoFunction = 'RCOBE21006'; // Mostrar o Detalhe
                *IN50 = *ON;
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WExtKeys);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WExtKeys);
             ECR_S#PGM   = S#PGM;
             WRITE RCOBE21000;
             WRITE RCOBE21099;
             WRITE RCOBE21098;
             EXFMT RCOBE21005;
             IF ($ValidKey(Func#Key:KEY#F21));
                WModoIO = MOD#Altera;
                *IN50 = *ON;
             ELSEIF (Func#Key = KEY#F12);
                IF (WModoIO = MOD#Altera);
                   *IN12 = *OFF;
                   WModoIO = MOD#Consulta;
                  *IN50 = *ON;
                ENDIF;
             ELSEIF (Func#Key = KEY#Enter);
                IF (WModoIO = MOD#Consulta);
                   LEAVE;
                ELSE;
                   // Criar o registo
                ENDIF;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: LoadData
       // Objectivo.: Carregar a informa��o do Interface
       //========================================================
       BEGSR LoadData;
          WExtKeys = *ZEROS;
          CHAIN(N) (ECR_CDCONT) RCOBP010;
          IF (%FOUND(FCOBP010));
             WNumOrdemAP = *ZEROS;
             WNumOrdemRC = *ZEROS;
             WCdRamo     = *ZEROS;
             IF (COB_TPENTID = ENT#Interface);
                 CHAIN (COB_NUORDEM) RINTP028;
                 IF (%FOUND(FINTP028));
                    WNumOrdemAP   = INT_NUMORDAP;
                    IF (WNumOrdemRC > *ZEROS);
                       WNumOrdemRC   = INT_NUMORDRC - 1000000;
                    ENDIF;
                    ECR_CDCOMP    = INT_CCOMP;
                    ECR_DSTIPRECIB = $GetDescricao(91:INT_TPRECIB);
                    IF (INT_CDRAMO = *ZEROS);
                       ECR_CDRAMO = INT_CDRMORG;
                       ECR_DSRAMO = *BLANKS;
                    ELSE;
                       ECR_CDRAMO = %CHAR(INT_CDRAMO);
                    ENDIF;
                    ECR_NUAPOL    = INT_NUAPOL;
                    ECR_NUMREC    = INT_NUMREC;
                    ECR_DTINICIO  = $CvtData($DateToInt(INT_DTINICIO):
                                                            'YYMD':'DMYY');
                    ECR_DTTERMO   = $CvtData($DateToInt(INT_DTTERMO):
                                                            'YYMD':'DMYY');
                    ECR_DTENTRADA = $CvtData($DateToInt(INT_DTENTRAD):
                                                            'YYMD':'DMYY');
                    ECR_DTLIMITE  = $CvtData($DateToInt(INT_DTLIMITE):
                                                            'YYMD':'DMYY');
                    ECR_NOSEG     = INT_NOMSEG;
                    ECR_NUMORDEM  = INT_NUMORDRC;
                 ENDIF;
             ELSEIF (COB_TPENTID=ENT#Recibo) OR (COB_TPENTID=ENT#CMPSeguro);
                WNumOrdemRC = COB_NUORDEM;
                CHAIN WNumOrdemRC RECIBO R#RECIBO;
                IF (%FOUND(RECIBO));
                    ECR_CDCOMP    = REC_CCOMP;
                    ECR_DSTIPRECIB = $GetDescricao(91:%CHAR(REC_CODMOV));
                    ECR_CDRAMO    = %CHAR(REC_CRAMO);
                    ECR_NUAPOL    = REC_APOLIC;
                    ECR_NUMREC    = REC_NREC2;
                    ECR_DTINICIO  = $CvtData(REC_DTINIC:'DMY':'DMYY');
                    ECR_DTTERMO   = $CvtData(REC_DTFIM:'DMY':'DMYY');
                    ECR_DTENTRADA = $CvtData(REC_DTREC:'DMY':'DMYY');
                    MONITOR;
                       ECR_DTLIMITE = %DEC(REC_DTLIM:6:0);
                    ON-ERROR;
                       ECR_DTLIMITE = *ZEROS;
                    ENDMON;
                    ECR_DTLIMITE  = $CvtData(ECR_DTLIMITE:'DMY':'DMYY');
                    ECR_NOSEG     = REC_DVAR;
                    ECR_NUMORDEM  = REC_NORD2;
                ENDIF;
             ELSEIF (COB_TPENTID = ENT#Apolice);
                WNumOrdemAP = COB_NUORDEM;
             ENDIF;

             ECR_DSCOMP = $GetDescricao(33:%TRIM(%EDITW(ECR_CDCOMP:'0   ')));
             //----------------------------------------------------
             // Obter a informa��o adicional
             //----------------------------------------------------
             IF (WNumOrdemAP > *ZEROS);
                CHAIN WNumOrdemAP APOL_ACT R#APOLICE;
                IF (NOT %FOUND(APOL_ACT));
                   CHAIN WNumOrdemAP APOL_HST R#APOLICE;
                   IF (NOT %FOUND(APOL_HST));
                      LEAVESR;
                   ENDIF;
                ENDIF;

                ECR_NOMED = *BLANKS;
                IF (AP_RI1 > *ZEROS);
                   CHAIN AP_RI1 MEDIADOR R#MEDIADOR;
                   IF (%FOUND(MEDIADOR));
                      ECR_NOMED = MED_NOMCR;
                   ENDIF;
                ENDIF;

                ECR_NUTELEF = *BLANKS;
                CHAIN (AP_CSEG) GFSEG;
                IF (%FOUND(GFSEG));
                   IF (SEG_TELEF = *BLANKS);
                      ECR_NUTELEF = '      ' + %EDITW(SEG_TLM:'         ');
                   ELSE;
                      ECR_NUTELEF = SEG_TELEF;
                   ENDIF;
                ENDIF;

                WCdRamo = AP_CRAMO;
                CHAIN (ECR_CDCOMP:WCdRamo) RCATP205;
                IF (%FOUND(FCATP205));
                   ECR_DSRAMO = CAT_DSRAMO;
                ELSE;
                   ECR_DSRAMO = *BLANKS;      // Obter a descri��o para o Ramo
                ENDIF;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: RmvInterface
       // Objectivo.: Eliminar o ficheiro de Interface
       //========================================================
       BEGSR RmvInterface;
       ENDSR;
      /END-FREE
