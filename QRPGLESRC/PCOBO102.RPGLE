     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO102
     H* Descrição.: Manutenção de IDD's
     H* Observação: Programa reescrito na nova framework.
     H*==================================================================
     FFCOBP22002IF   E           K DISK    PREFIX(ADC_)
     F                                     RENAME(RCOBP220:RCOBP22002)
     FTBIDDPL3  IF   E           K DISK    RENAME(TBIDDP:TBIDDL)
     FGBASEGRL  IF   E           K DISK    PREFIX(BAS_)
     FSD1RE2    IF   E           K DISK    PREFIX(REC_)
     FFCOBE102  CF   E             WORKSTN SFILE(RCOBE10202:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================*
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

     D*==> Chamada Dinâmica a funções
     D CallDynamic     PR                  EXTPGM(WDynamic)
     D   FileCode                    10P 0 Const Options(*NOPASS)
     D   MsgError                     7A   Const Options(*NOPASS)
     D   Modo                         1A   Const Options(*NOPASS)
     D ShowDetail      PR                  EXTPGM('TB2120R')
     D   FileCode                    10P 0 Const Options(*NOPASS)
     D   Origem                       1A   Const Options(*NOPASS)
     D WDynamic        S             10A
     D
     D*==> Campos de Trabalho
     D REC#            S              4S 0
     D KTE#IDD         S              5S 0
     D WNoFunction     S             10A
     D WIsAll          S               N
     D WSelected       S               N
     D WIdSit          S              1A
     D WIdUser         S              8A
     D WIddTap         S              8S 0
     D WDatInt         S              8S 0
     D WValInt         S              8S 2
     D WIdVal          S             13S 2
     D WMsgErro        S              7A
     D WModoExec       S              1A
     D WIDDSit         S              1A
     D WNumRecJM       S              9A
     D WRecID          S            100A
     D
     D*==> Constantes
     D MOD#Pendentes   C                   'P'
     D MOD#Tratados    C                   'T'
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
       // Sessão de IDD's
       //----------------------------------------------
       WDatInt = %SUBDT(%DATE():*D)*1000000+%SUBDT(%DATE():*M)*10000+
                          %SUBDT(%DATE():*Y);
       KTE#IDD = $SetFiles('TBIDDEF');
       $addCriteria('Código.....:':'TEFENV':'GE':'D');
       $addCriteria('Data envio.:':'TEDATE':'GE');
       $addCriteria('Data retor.:':'TEDATR':'GE':'D');
       $addCriteria('Extensão...:':'TETIPF':'EQ':'D');
       $addCriteria('Valor......:':'TEVALP':'EQ':'A');
       $SetSort('TEFENV #DESC');
       ECR_DSCRITER = $getCritText();
       $SetLstFld('TEFENV':'Código' :10:'D');
       $SetLstFld('TETIPF':'Ext' :3:'E');
       $SetLstFld('TEDATE':'Data env.':10:'D':'L');
       $SetLstFld('TEDATR':'Data ret.':10:'D':'L');
       // $SetLstFld('TEDATP':'Dt Cobr.' :10:'D':'L');
       $SetLstFld('TETREG':'Env'      : 3:'D');
       $SetLstFld('TEREGA':'Ace'      : 3:'D');
       $SetLstFld('TEVALR':'V.Recib.' :10:'D':'M');
       $SetLstFld('IDCRET':'Observação':19:'E');
       //$SetLstFld('IDCRET':'Cód.Ret.' : 8:'D');
       //$SetLstFld('TEVALR':'V.Premios':10:'D':'M');
       $SetPage(12);

       EXSR ShowIDD;
       $RmvDBSession(KTE#IDD);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ShowIDD
       // Objectivo.: Apresentar Lista de IDD's
       //=========================================================
       BEGSR ShowIDD;
          $SetCriteria('(TEDATR = 0)');
          WModoExec = MOD#Pendentes;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             IF (WModoExec =MOD#Pendentes);
                WNoFunction = 'RCOBE10203'; // Listar IDDs
             ELSE;
                WNoFunction = 'RCOBE10207'; // Listar IDDs
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RCOBE10200;
             WRITE RCOBE10299;
             WRITE RCOBE10298;
             EXFMT RCOBE10203;
             IF (Func#Key = KEY#F05);
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF (Func#Key = KEY#F09);
                IF (WModoExec = MOD#Pendentes);
                   WModoExec = MOD#Tratados;
                   $SetCriteria('(TEDATR <> 0)');
                ELSE;
                   WModoExec = MOD#Pendentes;
                   $SetCriteria('(TEDATR = 0)');
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
          WRITE RCOBE10203;     // Limpar Lista de Funções
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
                ECR_CODERRO = *BLANKS;
                ECR_OPTION  = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();
                ECR_LST#DET = $GetLstLine();
                ECR_TEFENV  = $GetFieldNum('TEFENV'); // Código do Ficheiro
                ECR_TEDATR  = $GetFieldNum('TEDATR'); // Data de Transferencia
                ECR_TEDATP  = $GetFieldNum('TEDATP'); // Data de Pagamento

                IF ($GetFieldNum('TETREG') <> $GetFieldNum('TEREGA'));
                   IF ($GetFieldNum('TEDATP') > *ZEROS);
                   // *IN81 = *ON;
                   ELSEIF ($GetFieldNum('TEDATR') = *ZEROS);
                   // *IN80 = *ON;
                   ENDIF;

                   IF (WModoExec = MOD#Pendentes);
                      EXSR ValidarPendente;
                   ELSE;
                      EXSR ValidarTratado;
                   ENDIF;
                ENDIF;

                IF (ECR_CODERRO = *BLANKS);
                   %SUBST(ECR_LST#DET:57:19) = *BLANKS;
                   IF (WModoExec = MOD#Pendentes);
                      *IN80 = *ON; // Branco
                   ELSE;
                      *IN80 = *OFF; // Branco
                   ENDIF;
                   *IN81 = *OFF; // Vermelho
                   *IN82 = *OFF; // Amarelo
                ELSE;
                   *IN80 = *OFF; // Branco
                   *IN82 = *OFF; // Amarelo
                   *IN81 = *ON; // Vermelho
                   %SUBST(ECR_LST#DET:57:19) = $GetMessage(ECR_CODERRO);
                ENDIF;

                REC# += 1;
                ECR_RRN = 1;
                WRITE RCOBE10202;
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
          READC RCOBE10202;
          DOW (NOT %EOF());
             WMsgErro = *BLANKS;
             IF ($ValidOption(ECR_OPTION:'5')); // IDD's por ficheiro
                WSelected = *ON;
                WDynamic = 'TB2110R';
                CallDynamic(ECR_TEFENV:WMsgErro);

             ELSEIF ($ValidOption(ECR_OPTION:'6')); // Detalhe
                WSelected = *ON;
                ShowDetail(ECR_TEFENV:'E');

             ELSEIF ($ValidOption(ECR_OPTION:'7')); // Recebido

             ELSEIF ($ValidOption(ECR_OPTION:'9')); // Cobrar
                WRecID = '('+%CHAR(ECR_TEFENV)+')';
                IF ($ShowDialog('COB0082':WRecID:'NS') = 'S');
                   WDynamic = 'RCOBO130';
                   CallDynamic(ECR_TEFENV:WMsgErro:'C');
                   IF (WMsgErro <> *BLANKS);
                      $ShowDialog(WMsgErro);
                   ENDIF;
                ENDIF;

             ELSEIF ($ValidOption(ECR_OPTION:'10')); // Reemitir Documentos
                IF (ECR_TEDATR <> *ZEROS); // AND (ECR_TEDATP <> ECR_TEDATR);
                   WSelected = *ON;
                   WDynamic = 'RCOBO130';
                   CallDynamic(ECR_TEFENV:WMsgErro:'I');
                   IF (WMsgErro <> *BLANKS);
                      $ShowDialog(WMsgErro);
                   ELSE;
                      $ShowDialog('COB0098':%CHAR(ECR_TEFENV));
                   ENDIF;
                ELSE;
                   WMsgErro = 'COB0030'; // Ainda não foi processado
                ENDIF;

             ELSEIF ($ValidOption(ECR_OPTION:'11')); // Anular geração
                IF (ECR_TEDATR = *ZEROS); //AND (ECR_TEDATP <> ECR_TEDATR);
                   IF ($ShowDialog('COB0100':%CHAR(ECR_TEFENV):'NS') = 'S') ;
                      WSelected = *ON;
                      WDynamic = 'RCOBO132';
                      CallDynamic(ECR_TEFENV:WMsgErro);
                   ENDIF;
                ELSE;
                   WMsgErro = 'COB0031'; // Já foi processado o ficheiro
                ENDIF;
             ENDIF;
             IF (WMsgErro <> *BLANKS);
                $ShowDialog(WMsgErro);
             ENDIF;
             READC RCOBE10202;
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
       // Subrotina.: SavData
       // Objectivo.: Salvaguardar o registo
       //========================================================
       BEGSR SavData;
       ENDSR;

       //========================================================
       // Subrotina.: ValidarPendente
       // Objectivo.: Validar os registos pendentes
       //========================================================
       BEGSR ValidarPendente;
          SETLL (ECR_TEFENV) TBIDDL;
          READE (ECR_TEFENV) TBIDDL;
          DOW (NOT %EOF(TBIDDPL3));
             EXSR ValidarRecibo;  // Validar se existe recibos anulados ou cobrados
             IF (ECR_CODERRO <> *BLANKS);
                LEAVE;
             ENDIF;

             EXSR ValidarApolice;  // Validar se existe Apólices anuladas
             IF (ECR_CODERRO <> *BLANKS);
                LEAVE;
             ENDIF;
             READE (ECR_TEFENV) TBIDDL;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: ValidarTratado
       // Objectivo.: Valdiar os registos tratados
       //========================================================
       BEGSR ValidarTratado;
          SETLL (ECR_TEFENV) TBIDDL;
          READE (ECR_TEFENV) TBIDDL;
          DOW (NOT %EOF(TBIDDPL3));
             EXSR ValidarRecibo;  // Validar se existe recibos anulados ou cobrados
             IF (ECR_CODERRO <> *BLANKS);
                LEAVE;
             ENDIF;
             READE (ECR_TEFENV) TBIDDL;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: ValidarRecibo
       // Objectivo.: Validiar o estado dos recibos no Corretor
       //             em Demo Comp.
       //========================================================
       BEGSR ValidarRecibo;
          WNumRecJM = %TRIM(%EDITW(IDREC:'0         '));
          CHAIN (054:WNumRecJM) SD1RE2;
          IF (%FOUND(SD1RE2));
             IF (REC_CODSIT = 2) AND (WModoExec = MOD#Pendentes);
                ECR_CODERRO = 'COB0113';
                LEAVESR;
             ELSEIF (REC_CODSIT = 1) OR (REC_CODSIT = 3)
                AND (WModoExec = MOD#Pendentes);
                ECR_CODERRO = 'COB0114';
                LEAVESR;
             ELSEIF (REC_CODSIT = 0) AND (WModoExec = MOD#Tratados)
               AND (IDSIT = '4');
                ECR_CODERRO = 'COB0115';
                LEAVESR;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ValidarApolice
       // Objectivo.: Validar a situação das Apólices
       //========================================================
       BEGSR ValidarApolice;
          CHAIN (IDNADC) RCOBP22002;
          IF (%FOUND(FCOBP22002));
             CHAIN (ADC_CDRAMO:ADC_NUAPOL) GBASEGRL;
             IF (%FOUND(GBASEGRL));
                IF (BAS_SIT <> 1);
                   ECR_CODERRO = 'COB0111';
                   LEAVESR;
                ENDIF;
             ELSE;
                ECR_CODERRO = 'COB0112';
                LEAVESR;
             ENDIF;
          ELSE;
             ECR_CODERRO = 'COB0106';
             LEAVESR;
          ENDIF;
       ENDSR;
      /END-FREE
