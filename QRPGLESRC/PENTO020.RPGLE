     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PENTO020
     H* Descrição.: Manutenção de Entidades
     H*             Este programa recebe como o parâmetro o tipo de
     H*             Entidade a Trabalhar. Por exemplo 20 para o Credores
     H*==================================================================
     FGSINENT   UF A F  400     7AIDISK    KEYLOC(1) EXTFILE('G.SINENT')
     FFENTE020  CF   E             WORKSTN SFILE(RENTE02002:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================*
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SGRLO013

     D*===> Formato de Registo
     D R#ENT         E DS                  EXTNAME('G.SINENT') PREFIX('ENT_')
     D
     D*==> Definição dos Campos de Trabalho
     D WSelected       S               N
     D WNoFunction     S             10A
     D WExtKeys        S             24A
     D WMsgErro        S              7A
     D WIO#Mode        S              1A
     D WTipEntidad     S              2A
     D PTipEntidad     S              2A
     D
     D REC#            S              4S 0
     D KTE#ENTID       S              5S 0
     D KEY#ENTID       S              7A
     D
     D*==> Constantes
     D MOD#Criar       C                   'I'
     D MOD#Alterar     C                   'C'
     D MOD#Consulta    C                   'L'
     D*==================================================================*
     C     *ENTRY        PLIST
     C                   PARM                    PTipEntidad
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
          WTipEntidad = PTipEntidad;
       ON-ERROR;
          WTipEntidad = '20';
       ENDMON;

       //----------------------------------------------
       // Sessão de Interfaces
       //----------------------------------------------
       KTE#ENTID = $SETFILES('"G.SINENT"');
       $SetFields('TPENT,NRENT,NOME,NCONTR,CDELIM');
       $SETCRITERIA('TPENT = ' + WTipEntidad);

       $addCriteria('Código.....:':'NRENT':'GE');
       $addCriteria('Nome.......:':'NOME':'LK');
       $addCriteria('NIF........:':'NCONTR':'EQ');
       $SetLstFld('NRENT':'Código':10:'D');
       $SetLstFld('NOME':'Nome':40:'E');
       $SetLstFld('NCONTR':'NIF':9:'D');
       $SetPage(12);

       //--------------------------------------------------------
       // ForCompr valores por defeito
       //--------------------------------------------------------
       ECR_DSCRITER = $getCritText();
       EXSR ShowEntidade;

       $RmvDBSession(KTE#ENTID);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ShowEntidade
       // Objectivo.: Apresentar a lista das Entidades
       //=========================================================
       BEGSR ShowEntidade;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WNoFunction = 'RENTE02003';
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RENTE02000;
             WRITE RENTE02099;
             WRITE RENTE02098;
             EXFMT RENTE02003;
             IF ($ValidKey(Func#Key:KEY#F05));
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF ($ValidKey(Func#Key:KEY#F06));
                WIO#Mode = MOD#Criar;
                EXSR ShowDetail;
                WIO#Mode = MOD#Consulta;
             ELSEIF ($ValidKey(Func#Key:KEY#F07));
             ELSEIF ($ValidKey(Func#Key:KEY#F09));
             ELSEIF ($ValidKey(Func#Key:KEY#F10));
             // PrintCobrados(ECR_CDFILE:WMsgErro);
             // IF (WMsgErro = *BLANKS);
             //    WmsgErro = 'COB0061';
             // ENDIF;
             // $ShowDialog(WMsgErro);
             ELSEIF ($ValidKey(Func#Key:KEY#F12));
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
          WRITE RENTE02003;     // Limpar Lista de Funções
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
                REC# += 1;

                //---------------------------------------------------
                // Assinalar Interfaces Anuladas
                //---------------------------------------------------
                ECR_NRENT = $GetFieldNum('NRENT');
                IF ($GetField('CDELIM') = '*');
                   *IN60 = *ON;
                ELSE;
                   *IN60 = *OFF;
                ENDIF;
                // *IN60 = *OFF; // Vermelho
                // *IN62 = *OFF; // Branco
                // *IN64 = *OFF; // Amarelo
                // IF ($GetDBSession() = KTE#ENTID);
                //    ECR_CDFILE  = $GetFieldNum('CDFILE');
                //    IF ($GetField('CDESTAD') = 'A');
                //       *IN60 = *ON; // Vermelho
                //    ELSEIF ($GetField('CDESTAD') = 'F');
                //       *IN62 = *ON; // Branco
                //    ENDIF;
                // ENDIF;
                ECR_RRN = 1;
                WRITE RENTE02002;
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
          READC RENTE02002;
          DOW (NOT %EOF());
             WSelected = *ON;
             IF ($ValidOption(ECR_OPTION:'5':S#PGM:WNoFunction));
                EXSR ShowDetail;
                IF (*IN12);
                   *IN12 = *OFF;
                   LEAVE;
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'4':S#PGM:WNoFunction));
             ELSEIF ($ValidOption(ECR_OPTION:'6':S#PGM:WNoFunction));
             ELSEIF ($ValidOption(ECR_OPTION:'8':S#PGM:WNoFunction));
             ENDIF;
             READC RENTE02002;
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
          IF (WIO#Mode = MOD#Criar);
             *IN50 = *ON;
             WNoFunction = 'RENTE02006';
          ELSE;
             *IN50 = *OFF;
             WNoFunction = 'RENTE02005';
             EXSR LoadData;
          ENDIF;

          DOW (NOT *IN03 AND NOT *IN12);
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WExtKeys);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WExtKeys);
             ECR_S#PGM   = S#PGM;
             WRITE RENTE02000;
             WRITE RENTE02099;
             EXFMT RENTE02005;
             IF ($ValidKey(Func#Key:KEY#F04));
             ELSEIF ($ValidKey(Func#Key:KEY#F12));
                IF (WIO#Mode = MOD#Alterar);
                   WIO#Mode = MOD#Consulta;
                   WNoFunction = 'RENTE02005';
                   *IN50 = *OFF;
                   *IN12 = *OFF;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F21));
                WIO#Mode = MOD#Alterar;
                WNoFunction = 'RENTE02021';
                *IN50 = *ON;
             ELSEIF ($ValidKey(Func#Key:KEY#F22));
                IF ($ShowDialog('INT0007':*BLANKS:'NS') = 'S');
                   EXSR RmvEntidade;
                   LEAVESR;
                ENDIF;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR ChgEntidade;
                LEAVESR;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //========================================================
       // Subrotina.: LoadData
       // Objectivo.: Carregar a informação do Interface
       //========================================================
       BEGSR LoadData;
          WExtKeys = *ZEROS;
          KEY#ENTID = '20' + %EDITW(ECR_NRENT:'     ');
          CHAIN(N) KEY#ENTID GSINENT R#ENT;
          IF (%FOUND(GSINENT));
             IF (ENT_CDELIM = '*');
                %SUBST(WExtKeys:21:1) = '1';
                %SUBST(WExtKeys:22:1) = '1';
             ENDIF;
             ECR_NRENT  = ENT_NRENT;
             ECR_EMPIND = ENT_EMPIND;
             ECR_NOME   = ENT_NOME;
             ECR_RESPON = ENT_RESPON;
             ECR_MORADA = ENT_MORADA;
             ECR_CDPOST = ENT_CDPOST;
             ECR_LOCAL  = ENT_LOCAL;
             ECR_TELEF  = ENT_TELEF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: RmvInterface
       // Objectivo.: Eliminar o Registo da Entidade
       //========================================================
       BEGSR RmvEntidade;
          WMsgErro = *BLANKS;
       ENDSR;

       //========================================================
       // Subrotina.: ChgEntidade
       // Objectivo.: Alterar o Registo da Entidade
       //========================================================
       BEGSR ChgEntidade;
       ENDSR;
      /END-FREE
