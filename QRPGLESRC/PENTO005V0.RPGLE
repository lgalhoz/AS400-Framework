     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PENTO005
     H* Descri��o.: Manuten��o de NIF's de Penhoras e Indesej�veis
     H* Observa��o: Este programa permite a consulta de NIF's bem como
     H* a manuten��o dos mesmos. Faz a distin��o entre NIF's carregados
     H* de forma autom�tica, partindo da informa��o da Lusit�nia, e NIF's
     H* actualizados manualmente pelo utilzador.
     H*==================================================================
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FFINTP010  IF   E           K DISK    PREFIX(INT_)
     FFENTP01001IF   E           K DISK    PREFIX(ENT_)
     F                                     RENAME(RENTP010:RENTP01001)
     FFENTP01002IF   E           K DISK    PREFIX(ENT_)
     F                                     RENAME(RENTP010:RENTP01002)
     FFENTP010  UF A E           K DISK    PREFIX(ENT_)
     FFENTE005  CF   E             WORKSTN SFILE(RENTE00502:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descri��o
     F* -----------  --------------------------------------------
     F*      45      Condicionar campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Validar NIF
     D  ChekNIF        PR                  EXTPGM('CHKD')
     D   NumNIF                       9S 0 CONST
     D   Resultado                    1A   CONST
     D
     D*==> Apresentar Listas
     D ShowList        PR
     D   PARM                         2S 0 CONST
     D
     D*==> Campos de Trabalho
     D WIcErro         S               N   INZ(*OFF)
     D WMsgErro        S              7A   INZ(*BLANKS)
     D
     D REC#            S              4S 0
     D SES#NIFS        S              5S 0
     D SES#Files       S              5S 0
     D SES#Detail      S              5S 0
     D SES#Screen      S              5S 0
     C*========================================================
      /FREE

       //------------------------------------
       // Validar o acesso a esta fun��o
       //------------------------------------
       S#PGM = 'PENTO005';
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------------
       // Sess�o de NIF's
       //----------------------------------------------
       SES#NIFS    = $SetFiles('FENTP010');
       $SetCriteria('(TPREGIS= ''PH'')');
       $addCriteria('NIF........:':'NUNIF':'GE');
       $addCriteria('Nome.......:':'NOME':'LK');
       ECR_DSCRITER = $getCritText();
       $SetLstFld('NUNIF':'NIF':9:'D');
       $SetLstFld('NOME':'Nome':40:'E');
       $SetLstFld('DTINICIO':'Dat.Inicio':10:'D':'D');
       $SetLstFld('DTTERMO':'Dat.Termo':10:'D':'D');
       $SetPage(12);

       //--------------------------------------------------
       // Transferir controlo do Ecr� para a API
       //--------------------------------------------------
       SES#Screen = $AddScreen('FENTE005':'RENTE00505');
       $AddScPFld(%ADDR(ECR_F#00):'DTINICIO'); // Data de Inicio da Penhora
       $AddScPFld(%ADDR(ECR_F#01):'DTTERMO');  // Data de Termo
       $AddScPFld(%ADDR(ECR_F#02):'NOME');     // Nome
       $AddScPFld(%ADDR(ECR_F#03):'NUNIF');    // NIF
       $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
       $AddScMsg(%ADDR(ECR_ERR#MSG));

       ShowList(*ZEROS);

       $RmvDBSession(SES#NIFS);
       $RmvScreen(SES#Screen);
       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: ShowList
     P* Descri��o.: Apresentar Ecr� de Consulta em Lista
     P*===================================================================
     P ShowList        B                   EXPORT
     D ShowList        PI
     D   PARMS                        2S 0 CONST
     D
     D*==> Campos de Trabalho
     D WKeysTohide     S             24A   INZ(*ZEROS)
     D WOptTohide      S             24A   INZ(*ZEROS)
     D WMsgTexto       S            163A   INZ(*BLANKS)
     D WNomeEntid      S             40A   INZ(*BLANKS)
     D WDatTermo       S              8S 0 INZ(*ZEROS)
     D WDatInicio      S              8S 0 INZ(*ZEROS)
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WSelected       S               N
     D WCdResult       S              1A
     D WIcModo         S              1A   INZ(*BLANKS)
     D*===================================================================
      /FREE

       WKeysTohide = *ZEROS;
       WOptTohide  = *ZEROS;
       DOW (NOT *IN03 AND NOT *IN12);
          EXSR getRecords;
          WNoFunction = 'RENTE00503';
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WKeysTohide);
          DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WOptTohide);
          ECR_S#PGM   = S#PGM;
          ECR_ERR#MSG = *BLANKS;
          WRITE     RENTE00500;
          WRITE     RENTE00599;
          WRITE     RENTE00598;
          EXFMT RENTE00503;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             WIcModo = 'I';
             EXSR ShowDetail;
          ELSEIF ($ValidKey(Func#Key:KEY#F08));
             EXSR ShowSegurado;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             *IN12 = *OFF;
          ENDIF;
       ENDDO;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas dispon�v
       //========================================================
       BEGSR getRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecr�
          *In52     = *On;      // Mostrar a Lista de Ecr�
          WRITE RENTE00503;     // Limpar Lista de Fun��es
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecr�
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                *IN70 = *OFF;
                ECR_OPTION  = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();
                ECR_LST#DET = $GetLstLine();
                ECR_CDREGIS = $GetFieldNum('CDREGIS');
                IF ($GetFieldNum('DTTERMO') <> 99999999);
                   *IN70 = *ON;
                ENDIF;
                REC# += 1;
                ECR_RRN = 1;
                WRITE RENTE00502;
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
          READC RENTE00502;
          DOW (NOT %EOF());
             WSelected = *ON;
             IF ($ValidOption(ECR_OPTION:'5'));
                WIcModo = 'R';
                EXSR ShowDetail;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));

             ENDIF;
             READC RENTE00502;
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
       // Objectivo.: Apresentar o detalhe do Registo
       //========================================================
       BEGSR ShowDetail;
          $InzScStatus();
          WKeysTohide = *ZEROS;
          WOptTohide  = *ZEROS;
          ECR_NUNIF     = *ZEROS;
          ECR_NOME      = *BLANKS;
          ECR_DTINICIO  = *ZEROS;
          ECR_DTTERMO   = *ZEROS;
          ECR_TPREGIS   = *BLANKS;
          ECR_DSTPREGIS = *BLANKS;
          ECR_CDORIGEM  = *BLANKS;
          ECR_DSORIGEM  = *BLANKS;
          ECR_NOFILE    = *BLANKS;
          ECR_ENT01001  = *BLANKS;
          ECR_ENT01003  = *BLANKS;

          DOW (NOT *IN03 AND NOT *IN12);
             *IN52 = *OFF;
             IF (WIcModo = 'I');
                *IN51 = *ON;
                *IN52 = *ON;
                ECR_TPREGIS   = 'PH';
                ECR_DSTPREGIS = 'Penhoras';
                ECR_CDORIGEM  = 'M';
                ECR_DSORIGEM  = 'Registo Manual';
                ECR_ENT01001  = %CHAR(%DATE());
                ECR_ENT01003  = %CHAR(%DATE());
                WNoFunction = 'RENTE00506';
             ELSE;
                EXSR LoadRecord;
                IF (WIcModo = 'R');
                   *IN51 = *OFF;
                   WNoFunction = 'RENTE00505';
                   IF (ECR_CDORIGEM = 'A');
                      %SUBST(WKeysTohide:21:1) = '1';
                      %SUBST(WKeysTohide:22:1) = '1';
                   IF (ECR_CDORIGEM = 'M');
                      IF (ECR_ENT01001 <> ECR_ENT01003);
                         %SUBST(WKeysTohide:22:1) = '1';
                         *IN52 = *ON;
                         *IN51 = *OFF;
                      ELSE;
                         *IN52 = *ON;
                         *IN51 = *ON;
                      ENDIF;
                   ENDIF;
                ELSE;
                   *IN51 = *ON;
                   WNoFunction = 'RENTE00521';
                ENDIF;
             ENDIF;
             $KeysToOmit(S#PGM:WNoFunction:WKeysToHide);
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WKeysTohide);
             ECR_S#PGM   = S#PGM;
             WRITE     RENTE00500;
             WRITE     RENTE00599;
             EXFMT RENTE00505;
             IF ($ValidKey(Func#Key:KEY#F21));
                WIcModo = 'U';
             ELSEIF ($ValidKey(Func#Key:KEY#F22));
                IF ($ShowDialog('ENT0021':*BLANKS:'NS') = 'S');
                   WIcModo = 'D';
                   EXSR UpdRecord;
                   LEAVESR;
                ENDIF;
             ELSEIF (Func#Key=KEY#Enter);
                IF (WIcModo <> 'R');
                   EXSR ValRecord;
                   IF ($GetScStatus = 'O');
                      EXSR UpdRecord;
                      WIcModo = 'R';
                   ENDIF;
                ELSE;
                   LEAVE;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //========================================================
       // Subrotina.: UpdRecord
       // Objectivo.: Actualizar o Registo
       //========================================================
       BEGSR UpdRecord;
          IF (WIcModo = 'I');
             ENT_CDREGIS  = $GetNextKey('FENTP010':'CDREGIS');
             ECR_CDREGIS  = ENT_CDREGIS;
             ENT_NUNIF    = ECR_NUNIF;
             ENT_NOME     = ECR_NOME;
             ENT_DTINICIO = ECR_DTINICIO;
             ENT_DTTERMO  = 99999999;
             ENT_TPREGIS  = 'PH';
             ENT_CDORIGEM = 'M';
             ENT_CDFILE   = *ZEROS;
             ENT_ENT01001 = %DATE();
             ENT_ENT01002 = %DATE('9999-12-31');
             ENT_ENT01003 = %DATE();
             WRITE RENTP010;
          ELSEIF (WIcModo = 'U');
             CHAIN (ECR_CDREGIS) RENTP010;
             ENT_NUNIF    = ECR_NUNIF;
             ENT_NOME     = ECR_NOME;
             ENT_DTINICIO = ECR_DTINICIO;
             ENT_DTTERMO  = ECR_DTTERMO;
             UPDATE RENTP010 %FIELDS(ENT_NOME:ENT_DTINICIO:ECR_DTTERMO);
          ELSEIF (WIcModo = 'D');
             CHAIN (ECR_CDREGIS) RENTP010;
             DELETE RENTP010;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ValRecord
       // Objectivo.: Validar Preenchimento do Registo
       //========================================================
       BEGSR ValRecord;
          $InzScStatus();
          IF (WIcModo = 'I');
             CHAIN (ECR_NUNIF:99999999) RENTP01002;
             IF (%FOUND());
                $SetSCError('NUNIF':'ENT0022'); // J� existe um registo activo
             ENDIF;
          ENDIF;
          IF (ECR_NOME = *BLANKS);
             $SetScError('NOME':'ENT0001'); // Nome Obrigat�rio
          ENDIF;

          IF (ECR_NUNIF = *ZEROS);
             $SetScError('NUNIF':'ENT0002'); // NIF Obrigat�rio
          ELSE;
             ChekNIF(ECR_NUNIF:WCdResult);
             IF (WCdResult = '1');
                $SetScError('NUNIF':'ENT0003'); // NIF Inv�lido
             ENDIF;
          ENDIF;

          IF (ECR_DTINICIO = *ZEROS);
             $SetScError('DTINICIO':'ENT0023');
          ELSE;
             TEST(DE) *ISO ECR_DTINICIO;
             IF (%ERROR());
                $SetScError('DTINICIO':'ENT0024');
             ENDIF;
          ENDIF;

          IF (ECR_DTTERMO = *ZEROS);
             $SetScError('DTTERMO':'ENT0025');
          ELSEIF (ECR_DTTERMO <> 99999999);
             TEST(DE) *ISO ECR_DTTERMO;
             IF (%ERROR());
                $SetScError('DTINICIO':'ENT0026');
             ENDIF;
          ENDIF;

          IF (ECR_DTTERMO <= ECR_DTINICIO);
             $SetScError('DTINICIO':'ENT0027');
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: LoadRecord
       // Objectivo.: Carregar o registo
       //========================================================
       BEGSR LoadRecord;
          CHAIN(N) (ECR_CDREGIS) RENTP010;
          IF (%FOUND());
             ECR_NUNIF    = ENT_NUNIF;
             ECR_NOME     = ENT_NOME;
             ECR_DTINICIO = ENT_DTINICIO;
             ECR_DTTERMO  = ENT_DTTERMO;
             ECR_TPREGIS  = ENT_TPREGIS;
             IF (ECR_TPREGIS = 'PH');
                ECR_DSTPREGIS = 'Penhoras';
             ELSE;
                ECR_DSTPREGIS = 'NIF''s Inv�lidos';
             ENDIF;
             ECR_CDORIGEM = ENT_CDORIGEM;
             IF (ECR_CDORIGEM = 'A');
                ECR_DSORIGEM  = 'Registo criado Autom�ticamente';
             ELSE;
                ECR_DSORIGEM  = 'Registo criado Manualmente';
             ENDIF;
             ECR_ENT01001 = %CHAR(ENT_ENT01001);
             ECR_ENT01003 = %CHAR(ENT_ENT01003);
             CHAIN (ENT_CDFILE) RINTP010;
             IF (%FOUND());
                ECR_NOFILE = INT_NOMFILE;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ShowSegurado
       // Objectivo.: Apresentar valores para o Segurado
       //========================================================
       BEGSR ShowSegurado;
          ECR_NUSEGU = *ZEROS;
          ECR_NOSEGU = *BLANKS;
          ECR_MSG001 = *BLANKS; // Str(61)
          ECR_MSG002 = *BLANKS; // Str(61)
          DOW (NOT *IN03 AND NOT *IN12);
             EXFMT RENTE00510;
             ECR_NOSEGU = *BLANKS;
             ECR_MSG001 = *BLANKS;
             ECR_MSG002 = *BLANKS;
             WMsgTexto  = *BLANKS;
             CHAIN (ECR_NUSEGU) GFSEG;
             IF (%FOUND());
                ECR_NOSEGU = SEG_NOM;
                IF (SEG_NIF = *ZEROS);
                   WMsgTexto = $GetMessage('ENT0016':*BLANKS:'T');
                ELSE;
                   WNomeEntid = *BLANKS;
                   WDatTermo  = *ZEROS;
                   WDatInicio = *ZEROS;
                   SETLL (SEG_NIF) RENTP01001;
                   READE (SEG_NIF) RENTP01001;
                   DOW (NOT %EOF(FENTP01001));
                      IF (ENT_TPREGIS = 'PH');
                         WNomeEntid = ENT_NOME;
                         WDatTermo  = ENT_DTTERMO;
                         WDatInicio = ENT_DTINICIO;
                         IF (ENT_DTTERMO = 99999999);
                            LEAVE;
                         ENDIF;
                         READE (SEG_NIF) RENTP01001;
                      ENDIF;
                   ENDDO;

                   IF (WDatTermo = 99999999);
                      WMsgTexto = $GetMessage('ENT0017':*BLANKS:'T');
                      WMsgTexto = $StrReplace('%VAR(NIF)':%CHAR(SEG_NIF):
                                               WMsgTexto);
                      WMsgTexto = $StrReplace('%VAR(NOME)':%TRIM(WNomeEntid):
                                               WMsgTexto);
                      WMsgTexto = $StrReplace('%VAR(DATA)':
                                         $ApplyDatFmt(WDatInicio:'YYMD':'-'):
                                            WMsgTexto);
                   ELSEIF (WDatTermo > *ZEROS);
                      WMsgTexto = $GetMessage('ENT0018':*BLANKS:'T');
                      WMsgTexto = $StrReplace('%VAR(NIF)':%CHAR(SEG_NIF):
                                               WMsgTexto);
                      WMsgTexto = $StrReplace('%VAR(NOME)':%TRIM(WNomeEntid):
                                               WMsgTexto);
                      WMsgTexto = $StrReplace('%VAR(DATA)':
                                         $ApplyDatFmt(WdatTermo:'YYMD':'-'):
                                            WMsgTexto);
                   ELSEIF (WDatTermo = *ZEROS);
                      WMsgTexto = $GetMessage('ENT0019':*BLANKS:'T');
                      WMsgTexto = $StrReplace('%VAR(NIF)':%CHAR(SEG_NIF):
                                              WMsgTexto);
                      WMsgTexto = $StrReplace('%VAR(NOME)':%TRIM(SEG_NOM):
                                                WMsgTexto);
                   ENDIF;
                ENDIF;
             ELSE;
                WMsgTexto = $GetMessage('ENT0020':*BLANKS:'T');
                ECR_MSG001 = 'Segurado Inexistente.+
                              Por corrija o C�digo do Segurado.';
             ENDIF;

             IF (WMsgTexto <> *BLANKS);
                $SetTexto(WmsgTexto:61:'E');
                ECR_MSG001 = $GetFirstLine();
                ECR_MSG002 = $GetNExtLine();
                ECR_MSG003 = $GetNExtLine();
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;
      /END-FREE
     P ShowList        E
