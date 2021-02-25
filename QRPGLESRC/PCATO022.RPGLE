     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO022
     H* Descrição.: Manutenção de planos de comissionamento
     H*==================================================================
     FFCATP205  UF A E           K DISK    PREFIX(CAT_)
     FFCATE022  CF   E             WORKSTN SFILE(RCATE02202:REC#)
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
     D WModo           S              1A
     D WMsgErro        S              7A
     D
     D REC#            S              4S 0
     D SES#COMIS       S              5S 0
     D SES#SCREEN      S              5S 0
     D
     D*==> Constantes
     D KTE#Consultar   C                   'C'
     D KTE#Alterar     C                   'A'
     D KTE#Inserir     C                   'I'
     D KTE#Eliminar    C                   'E'
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
       // Sessão de Parameterização de Comissões
       //----------------------------------------------
       SES#COMIS = $SetFiles('FCATP205');
       $addCriteria('Companhia..:':'CDCOMP':'EQ');
       $addCriteria('Ramo.......:':'DSRAMO':'LK');
       $SetLstFld('CDCOMP':'Comp.':6:'D');
       $SetLstFld('CDRAMO':'Ramo':6:'D');
       $SetLstFld('DSRAMO':'Descrição':50:'E');
       $SetLstFld('DTINICO':'Inicio.':10:'D':'E');
       $SetPage(12);

       //------------------------------------------------
       // Controlo das Variáveis do ecrã
       //------------------------------------------------
       SES#SCREEN = $AddScreen('FCATE022':'RCATE02205');
       $AddScPFld(%ADDR(ECR_F#01):'DSRAMO');
       $AddScPFld(%ADDR(ECR_F#02):'CDRAMO');
       $AddScPFld(%ADDR(ECR_F#02):'CDCOMP');
       $AddScPFld(%ADDR(ECR_F#03):'CDCOMP');
       $AddScPFld(%ADDR(ECR_F#04):'DTINICIO');
       $AddScPFld(%ADDR(ECR_F#05):'PRANGA');
       $AddScPFld(%ADDR(ECR_F#06):'PRCOBR');
       $AddScPFld(%ADDR(ECR_F#07):'PRCORR');
       $AddScPFld(%ADDR(ECR_F#09):'ENFIXO');
       $AddScPFld(%ADDR(ECR_F#10):'IMANPC');
       $AddScPFld(%ADDR(ECR_F#11):'IMFGA');
       $AddScPFld(%ADDR(ECR_F#12):'IMSELO');
       $AddScPFld(%ADDR(ECR_F#13):'IMINEM');
       $AddScPFld(%ADDR(ECR_F#14):'COEFIC');
       $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
       $AddScMsg(%ADDR(ECR_ERR#MSG));

       ECR_DSCRITER = $getCritText();
       WModo = KTE#Consultar;
       EXSR ShowInter;

       $RmvDBSession(SES#COMIS);
       $RmvScreen(SES#SCREEN);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ShowInter
       // Objectivo.: Apresentar as Interfaces
       //=========================================================
       BEGSR ShowInter;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WNoFunction = 'RCATE02203';
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RCATE02200;
             WRITE RCATE02299;
             WRITE RCATE02298;
             EXFMT RCATE02203;
             IF (Func#Key = KEY#F05);
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF (Func#Key = KEY#F06);
                WModo = KTE#Inserir;
                *IN50 = *ON;
                EXSR ShowDetail;
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
          WRITE RCATE02203;     // Limpar Lista de Funções
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
                ECR_CDCOMP  = $GetFieldNum('CDCOMP');
                ECR_CDRAMO  = $GetFieldNum('CDRAMO');
                ECR_NUSEQU  = $GetFieldNum('NUSEQU');
                REC# += 1;

                //---------------------------------------------------
                // Validar existencia de erros
                //---------------------------------------------------
                // *IN60 = *ON;
                // *IN80 = *OFF;

                ECR_RRN = 1;
                WRITE RCATE02202;
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
          READC RCATE02202;
          DOW (NOT %EOF());
             WSelected = *ON;
             IF ($ValidOption(ECR_OPTION:'2'));
                EXSR ShowDetail;
                IF (*IN12);
                   *IN12 = *OFF;
                   LEAVE;
                ENDIF;
             ELSEIF (ECR_OPTION='6');
             ENDIF;
             READC RCATE02202;
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
             IF (WModo = KTE#Consultar);
                WNoFunction = 'RCATE02205'; // Mostrar o Detalhe
             ELSEIF (WModo = KTE#Inserir);
                WNoFunction = 'RCATE02206'; // Mostrar o Detalhe
             ELSEIF (WModo = KTE#Alterar);
                WNoFunction = 'RCATE02221'; // Mostrar o Detalhe
                $SetScFldProt('CDRAMO');
                $SetScFldProt('CDCOMP');
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WExtKeys);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WExtKeys);
             ECR_S#PGM   = S#PGM;
             WRITE RCATE02200;
             WRITE RCATE02299;
             EXFMT RCATE02205;
             IF (Func#Key = KEY#F12);
                IF (WModo = KTE#Alterar);
                   WModo       = KTE#Consultar;
                   *IN50       = *OFF;
                   *IN12       = *OFF;
                ENDIF;
             ELSEIF (Func#Key = KEY#F21);
                WModo       = KTE#Alterar;
                *IN50       = *ON;
             ELSEIF (Func#Key = KEY#F22);
                IF ($ShowDialog('PRDXXXX':*BLANKS:'NS') = 'S');
                   WModo = KTE#Eliminar;
                   LEAVESR;
                ENDIF;
             ELSEIF (Func#Key = KEY#Enter);
                IF (WModo = KTE#Consultar);
                   LEAVE;
                ELSE;
                   EXSR SaveData;
                   IF (WMsgErro = *BLANKS);
                      WModo = KTE#Consultar;
                      *IN50 = *OFF;
                      EXSR LoadData;
                   ENDIF;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //========================================================
       // Subrotina.: LoadData
       // Objectivo.: Carregar a informação do Interface
       //========================================================
       BEGSR LoadData;
          IF (WModo <> KTE#Inserir);
             CHAIN (ECR_CDCOMP:ECR_CDRAMO:ECR_NUSEQU) RCATP205;
             IF (%FOUND());
                ECR_DSRAMO   = CAT_DSRAMO;
                ECR_DSCOMP   = *BLANKS;
                ECR_DTINICIO = $CvtData(CAT_DTINICIO:'YYMD':'DMYY');
                ECR_DTTERMO  = $CvtData(CAT_DTTERMO:'YYMD':'DMYY');
                ECR_PRANGA   = CAT_PRANGA;
                ECR_PRCOBR   = CAT_PRCOBR;
                ECR_PRCORR   = CAT_PRCORR;
                ECR_PRTOT    = CAT_PRANGA + CAT_PRCOBR + CAT_PRCORR;
                ECR_ENFIXO   = CAT_ENFIXO;
                ECR_IMANPC   = CAT_IMANPC;
                ECR_IMFGA    = CAT_IMFGA;
                ECR_IMSELO   = CAT_IMSELO;
                ECR_IMINEM   = CAT_IMINEM;
                ECR_COEFIC   = CAT_COEFIC;
             ENDIF;
          ELSE;
             ECR_CDCOMP   = *ZEROS;
             ECR_CDRAMO   = *ZEROS;
             ECR_DSRAMO   = *BLANKS;
             ECR_DSCOMP   = *BLANKS;
             ECR_DTINICIO = $CvtData($DateToInt(%DATE()):'YYMD':'DMYY');
             ECR_DTTERMO  = 29991231;
             ECR_PRANGA   = *ZEROS;
             ECR_PRCOBR   = *ZEROS;
             ECR_PRCORR   = *ZEROS;
             ECR_PRTOT    = *ZEROS;
             ECR_ENFIXO   = *ZEROS;
             ECR_IMANPC   = *ZEROS;
             ECR_IMFGA    = *ZEROS;
             ECR_IMSELO   = *ZEROS;
             ECR_IMINEM   = *ZEROS;
             ECR_COEFIC   = *ZEROS;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SaveData
       // Objectivo.: Guardar os registos
       //========================================================
       BEGSR SaveData;
          EXSR Validar;
          IF (WMsgErro = *BLANKS);
             IF (WModo = KTE#Inserir);
             ELSEIF (WModo = KTE#Alterar);
                CHAIN (ECR_CDCOMP:ECR_CDRAMO:ECR_NUSEQU) RCATP205;
                IF (%FOUND(FCATP205));
                CAT_DSRAMO   = ECR_DSRAMO;
                CAT_PRANGA   = ECR_PRANGA;
                CAT_PRCOBR   = ECR_PRCOBR;
                CAT_PRCORR   = ECR_PRCORR;
                CAT_ENFIXO   = ECR_ENFIXO;
                CAT_IMANPC   = ECR_IMANPC;
                CAT_IMFGA    = ECR_IMFGA;
                CAT_IMSELO   = ECR_IMSELO;
                CAT_IMINEM   = ECR_IMINEM;
                CAT_COEFIC   = ECR_COEFIC;
                UPDATE RCATP205 %FIELDS(CAT_DSRAMO:CAT_PRANGA:CAT_PRCOBR:
                                        CAT_PRCORR:CAT_ENFIXO:CAT_IMANPC:
                                        CAT_IMFGA:CAT_IMSELO:CAT_IMINEM:
                                        CAT_COEFIC);
                ENDIF;
             ELSEIF (WModo = KTE#Eliminar);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: Validar
       // Objectivo.: Validar os campos inseridos
       //========================================================
       BEGSR Validar;
          WMsgErro = *BLANKS;
          $InzScStatus();

          //----------------------------------------------
          // Validar a companhia
          //----------------------------------------------
          IF (ECR_CDCOMP = *ZEROS);
             WMsgErro = 'XXXXXXX';
             $SetScError('CDCOMP':WMsgErro);
          ELSE;
             IF (ECR_CDCOMP = *ZEROS); // A companhia existe
                WMsgErro = 'XXXXXXX';
                $SetScError('CDCOMP':WMsgErro);
             ENDIF;
          ENDIF;

          //----------------------------------------------
          // Validar o ramo
          //----------------------------------------------
          IF (ECR_CDRAMO = *ZEROS);
             WMsgErro = 'XXXXXXX';
             $SetScError('CDRAMO':WMsgErro);
          ELSE;
             IF (ECR_CDRAMO = *ZEROS); // Está parameterizado para a companhia ?
                WMsgErro = 'XXXXXXX';
                $SetScError('CDRAMO':WMsgErro);
             ENDIF;
          ENDIF;

          //----------------------------------------------
          // Validar se o Ramo já existe para a Companhia
          //----------------------------------------------
          IF (ECR_CDRAMO = *ZEROS);
             WMsgErro = 'XXXXXXX';
             $SetScError('CDRAMO':WMsgErro);
          ENDIF;
       ENDSR;
      /END-FREE
