     H*ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: TB2002R
     H* Descrição.: Manutenção de IDD's
     H* Observação: Programa reescrito na nova framework.
     H*==================================================================
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
     D WMsgError       S              7A
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
       $addCriteria('Ficheiro...:':'TEFENV':'GE':'D');
       $addCriteria('Data envio.:':'TEDATE':'EQ');
       $SetSort('TEFENV #DESC');
       ECR_DSCRITER = $getCritText();
       $SetLstFld('TEFENV':'Ficheiro' :10:'D');
       $SetLstFld('TEDATE':'Data env.':10:'D':'L');
       $SetLstFld('TEDATR':'Data ret.':10:'D':'L');
       $SetLstFld('TEDATP':'Dt Cobr.' :10:'D':'L');
       $SetLstFld('TETREG':'Env'      : 3:'D');
       $SetLstFld('TEREGA':'Ace'      : 3:'D');
       $SetLstFld('TEVALR':'V.Premios':10:'D':'M');
       $SetLstFld('TEVALP':'V.Pagam.' :10:'D':'M');
       $SetPage(12);

       EXSR ShowIDD;
       $RmvDBSession(KTE#IDD);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ShowIDD
       // Objectivo.: Apresentar Lista de IDD's
       //=========================================================
       BEGSR ShowIDD;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WNoFunction = 'RCOBE10203'; // Listar IDDs
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
                *IN80 = *OFF;
                *IN81 = *OFF;
                *IN82 = *OFF;
                ECR_OPTION   = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();
                ECR_LST#DET = $GetLstLine();
                ECR_TEFENV = $GetFieldNum('TEFENV'); // Código do Ficheiro
                ECR_TEDATR = $GetFieldNum('TEDATR'); // Data de Transferencia
                ECR_TEDATP = $GetFieldNum('TEDATP'); // Data de Pagamento
                IF ($GetFieldNum('TETREG') <> $GetFieldNum('TEREGA'));
                   IF ($GetFieldNum('TEDATP') > *ZEROS);
                      *IN81 = *ON;
                   ELSEIF ($GetFieldNum('TEDATR') = *ZEROS);
                      *IN81 = *ON;
                   ENDIF;
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
             IF ($ValidOption(ECR_OPTION:'5')); // IDD's por ficheiro
                WSelected = *ON;
                WDynamic = 'TB2110R';
                CallDynamic(ECR_TEFENV);
             ELSEIF ($ValidOption(ECR_OPTION:'6')); // Detalhe do ficheiro IDD
                WSelected = *ON;
                WDynamic = 'TB2120R';
                CallDynamic(ECR_TEFENV);
             ELSEIF ($ValidOption(ECR_OPTION:'8')); // Recepcionar o Ficheiro IDD
                IF (ECR_TEDATR = *ZEROS) AND (ECR_TEDATP = ECR_TEDATR);
                   WSelected = *ON;
                   WDynamic = 'RCOBO120';
                   CallDynamic(ECR_TEFENV);
                ELSE;
                   $ShowDialog(WMsgError);
                   // WTxtErro = 'Não é possível recepcionar ficheiros +
                   //            retornados do Banco. Contacte a +
                   //            Informática, por favor.';
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'9')); // Cobrar registos IDD
                IF (ECR_TEDATR <> *ZEROS) AND (ECR_TEDATP <> ECR_TEDATR);
                   WSelected = *ON;
                   WDynamic = 'TB2300J';
                   CallDynamic(ECR_TEFENV);
                ELSE;
                   $ShowDialog(WMsgError);
                   // WTxtErro = 'Não é possível Cobrar registos +
                   //            que ainda não foram tratados. +
                   //            Contacte a Informática, por favor.';
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'10')); // Reemitir Documentos
                IF (ECR_TEDATR <> *ZEROS) AND (ECR_TEDATP <> ECR_TEDATR);
                   WSelected = *ON;
                   WDynamic = 'TB2300JR';
                   CallDynamic(ECR_TEFENV);
                ELSE;
                   $ShowDialog(WMsgError);
                   // WTxtErro = 'Não é possível Cobrar registos +
                   //            que ainda não foram tratados. +
                   //            Contacte a Informática, por favor.';
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'11')); // Anular geração
                WSelected = *ON;
                WDynamic = 'RCOBO105';
                CallDynamic(ECR_TEFENV);
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
      /END-FREE
