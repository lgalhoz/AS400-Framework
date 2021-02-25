     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO015
     H* Descrição.: Manutenção de Tabelas e Códigos
     H*==================================================================
     FCSF02     UF A E           K DISK    PREFIX(COS_)
     FGBASEGRO  IF   E           K DISK    PREFIX(OR_)
     FGBASEGRA  IF   E           K DISK    PREFIX(AUT_)
     FFPRDE015  CF   E             WORKSTN SFILE(RPRDE01502:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      45      Condicionar campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Base Auto (Original)
     D AppAuto         PR                  EXTPGM('GP010X')
     D   Option                       1A   CONST
     D   RamApol                      9A   CONST
     D   Data                         8S 0 CONST Options(*NOPASS)
     D
     D*==> Base Outros Ramos (Original)
     D AppOutros       PR                  EXTPGM('GP015X')
     D   Option                       1A   CONST
     D   RamApol                      9A   CONST
     D   Data                         8S 0 CONST Options(*NOPASS)
     D
     D*==> Estrutura de Tabelas ou Códigos
     D DspRecibos      PR
     D   PCdRamo                      2S 0 CONST
     D   PNuApolice                   7S 0 CONST
     D
     D*==> Listar Planos Co-Seguro Aceite
     D PLANOCA         PR                  EXTPGM('LP002CSCL')
     D
     D*==> Listar Planos Co-Seguro Cedido
     D PLANOCC         PR                  EXTPGM('LP003CSCL')
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WCdMessage      S              7A   INZ(*BLANKS)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D KTE#RecLus      S              5S 0
     D KTE#RecCor      S              5S 0
     D
     D WCdRamo         S              2S 0
     D WNuApol         S              7S 0
     D WCvtRamo        S              3A
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdRamo
     C                   PARM                    WNuApol
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;
       EXSR CvtRamo;

       //----------------------------------------------
       // Lista de Recibos - Lusitania Azul
       //----------------------------------------------
       KTE#RecLus = $SetFiles('GCOBGR');
       $addCriteria('Número.....:':'GCNREC':'GE');
       $addCriteria('Situação...:':'GCSITR':'EQ');
       $addCriteria('Tipo.......:':'GCTIPR':'EQ');
       $SetSort('GCNREC #DESC');
       $SetLstFld('GCRAMO':'Ramo':4:'D');
       $SetLstFld('GCAPOL':'Apólice':8:'D');
       $SetLstFld('GCNREC':'Recibo':10:'D');
       $SetLstFld('GCSITR':'Situação':8:'C');
       $SetLstFld('GCTIPR':'Tipo':4:'C');
       $SetLstFld('GCDEMI':'Emissão':10:'C');
       $SetLstFld('GCTREC':'Valor':15:'C');
       $SetFields('GCNREC, GCRAMO, GCAPOL, GCNREC, GCSITR, GCTIPR,+
                   GCDEMI,GCTREC');
       $SetCriteria('(GCRAMO='''+%CHAR(WCdRamo)+''') AND +
                     (GCAPOL='''+%CHAR(WNuApol)+''')');
       $SetPage(12);

       //----------------------------------------------
       // Lista de Recibos - Corretor
       //----------------------------------------------
       KTE#RecCor = $SetFiles('FRECT010');
       $addCriteria('Número.....:':'NNUMREC':'GE':*ON);
       $addCriteria('Situação...:':'CSIT':'EQ');
       $addCriteria('Emissão....:':'DTEMISSAO':'GE');
       $SetLstFld('CRAMO':'Ramo':4:'D');
       $SetLstFld('NUMAPOL':'Apólice':10:'D');
       $SetLstFld('NNUMREC':'Recibo':10:'D');
       $SetLstFld('CSIT':'Situação':8:'C');
       $SetLstFld('DTEMISSAO':'Emissão':10:'C');
       $SetLstFld('VALREC':'Valor':15:'C');
       $SetSort('NNUMREC #DESC');
       $SetFields('CRAMO,NNUMREC,NUMAPOL,CSIT,DTEMISSAO,VALREC');
       IF (WCvtRamo <> *BLANKS);
          $SetCriteria('(CRAMO='''+%TRIM(WCvtRamo)+''') AND +
                        (NUMAPOL=''   '+%CHAR(WNuApol)+''')');
       ELSE;
          $SetCriteria('(NUMAPOL=''   '+%CHAR(WNuApol)+''')');
       ENDIF;
       $SetPage(12);

       //----------------------------------------------
       // Iniciar Apresentação dos Dados
       //----------------------------------------------
       $SetDBSession(KTE#RecLus); // Repor Sessão das Funções
       ECR_DSCRITER = $getCritText();
       DspRecibos(WCdRamo:WNuApol);

       $RmvDBSession(KTE#RecLus);
       $RmvDBSession(KTE#RecCor);
       *IN12 = *OFF;
       *IN03 = *OFF;
       RETURN;

       //===============================================================
       // Subrotina..: CvtRamo
       // Objectivo..: Converter Ramo para a Lusitania Azul
       //===============================================================
       BEGSR CvtRamo;
          WCvtRamo = *BLANKS;
          IF (WCdRamo = 1);
             WCvtRamo = ' 80';
          ELSEIF (WCdRamo = 2);
             WCvtRamo = ' 14';
          ELSEIF (WCdRamo = 3);
             WCvtRamo = '130';
          ELSEIF (WCdRamo = 4);
             WCvtRamo = '133';
          ELSEIF (WCdRamo = 5);
             WCvtRamo = '342';
          ELSEIF (WCdRamo = 6);
             WCvtRamo = '160';
          ELSEIF (WCdRamo = 7);
             WCvtRamo = ' 92';
          ELSEIF (WCdRamo = 8);
             WCvtRamo = ' 93';
          ELSEIF (WCdRamo = 9);
             WCvtRamo = ' 94';
          ELSEIF (WCdRamo = 10);
             WCvtRamo = ' 16';
          ELSEIF (WCdRamo = 15);
             WCvtRamo = ' 12';
          ELSEIF (WCdRamo = 17);
             WCvtRamo = '346';
          ELSEIF (WCdRamo = 18);
             WCvtRamo = ' 96';
          ELSEIF (WCdRamo = 65);
             WCvtRamo = ' 95';
          ELSEIF (WCdRamo = 19);
             WCvtRamo = '345';
          ELSEIF (WCdRamo = 51);
             WCvtRamo = ' 10';
          ELSEIF (WCdRamo = 55);
             WCvtRamo = '343';
          ELSEIF (WCdRamo = 69);
             WCvtRamo = ' 73';
          ELSEIF (WCdRamo = 50);
             WCvtRamo = '310';
          ENDIF;
       ENDSR;
      /END-FREE

     P*===================================================================
     P* Processo..: DspRecibos
     P* Descrição.: Apresenta os Recibos da Lusitania Azul e Corretor
     P*===================================================================
     P DspRecibos      B                   EXPORT
     D DspRecibos      PI
     D   PCdRamo                      2S 0 CONST
     D   PNuapolice                   7S 0 CONST
     D
     D WIOMode         S              1S 0 INZ(*ZEROS)
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WIsEmpty        S               N
     D WSelected       S               N
     D WRamoApolice    S              9A
     D
     D*==> Definição das constantes
     D KTE_Display     C                   0
     D KTE_Insert      C                   1
     D KTE_Change      C                   2
     D KTE_Delete      C                   3
     D*===================================================================
      /FREE

       WIOMode    = KTE_Display;

       EXSR getRecords;
       DOW (NOT *IN03 AND NOT *IN12);
          IF ($GetDBSession() = KTE#RecLus);
             WNoFunction = 'RPRDE01603'; // Listar Recibos Lusitania Azul
          ELSE;
             WNoFunction = 'RPRDE01604'; // Listar Recibos Corretor
          ENDIF;
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          ECR_ERR#MSG = *BLANKS;
          WRITE     RPRDE01500;
          WRITE     RPRDE01599;
          WRITE     RPRDE01598;
          EXFMT RPRDE01503;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
          ELSEIF ($ValidKey(Func#Key:KEY#F09));
             IF ($GetDBSession() = KTE#RecLus);
                $SetDBSession(KTE#RecCor);
             ELSE;
                $SetDBSession(KTE#RecLus);
             ENDIF;
             EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             EXSR getRecords;
             *IN12 = *OFF;
          ENDIF;
       ENDDO;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE01503;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                ECR_LSTHDR = $GetLstHdr();
                ECR_LSTDET = $GetLstLine();

                //-------------------------------------------------
                // Situação do Recibo
                //-------------------------------------------------
                IF ($GetfieldNum('GCSITR') = 0);
                   %SUBST(ECR_LSTDET:26) = 'Por Cobrar';
                ELSEIF ($GetfieldNum('GCSITR') = 1);
                   %SUBST(ECR_LSTDET:26) = 'Cobrado';
                ELSEIF ($GetfieldNum('GCSITR') = 3);
                   %SUBST(ECR_LSTDET:26) = 'Anulado';
                ELSEIF ($GetfieldNum('GCSITR') = 4);
                   %SUBST(ECR_LSTDET:26) = 'Pré-Anulação';
                ELSEIF ($GetfieldNum('GCSITR') = 6);
                   %SUBST(ECR_LSTDET:26) = 'Pré-Contencioso';
                ENDIF;

                //-------------------------------------------------
                // Tipo de Recibo
                //-------------------------------------------------
                IF ($GetfieldNum('GCTIPR') = 0);
                   %SUBST(ECR_LSTDET:45) = 'Novo';
                ELSEIF ($GetfieldNum('GCTIPR') = 1);
                   %SUBST(ECR_LSTDET:45) = 'Adicional';
                ELSEIF ($GetfieldNum('GCTIPR') = 2);
                   %SUBST(ECR_LSTDET:45) = 'Continuado';
                ELSEIF ($GetfieldNum('GCTIPR') = 5);
                   %SUBST(ECR_LSTDET:45) = 'Estorno';
                ENDIF;

                //---------------------------------------------
                // Formatar as Chaves das Tabelas
                //---------------------------------------------
                // ECR_APLDR    = $GetField('APLDR');
                // MONITOR;
                //    ECR_RAMO     = %DEC($GetField('RAMO'):2:0);
                //    ECR_APOL     = %DEC($GetField('APOL'):7:0);
                //    ECR_DATA     = %DEC($GetField('DATA'):8:0);
                //    ECR_NUMSEG   = %DEC($GetField('NUMSEG'):4:0);
                //    ECR_NUMSEGLS = %DEC($GetField('NUMSEGLS'):5:0);
                // ON-ERROR;
                //    ECR_RAMO     = *ZEROS;
                //    ECR_APOL     = *ZEROS;
                //    ECR_DATA     = *ZEROS;
                //    ECR_NUMSEG   = *ZEROS;
                //    ECR_NUMSEGLS = *ZEROS;
                // ENDMON;

                REC# += 1;
                WRITE RPRDE01502;
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
          READC RPRDE01502;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5')); // Intervenientes
                EXSR ShowRecibos;
                LEAVESR;
                //-----------------------------------------
                // Consultar os dados da Apólice
                //-----------------------------------------
             ELSEIF ($ValidOption(ECR_OPTION:'2'));
                WRamoApolice = %TRIM(%EDITW(ECR_RAMO:'0  ')) +
                               %CHAR(ECR_APOL);
                IF (ECR_RAMO = 50) OR (ECR_RAMO = 51);
                   AppAuto('2':WRamoApolice);
                ELSE;
                   AppOutros('2':WRamoApolice);
                ENDIF;
             ENDIF;
             READC RPRDE01502;
          ENDDO;
          IF (NOT WSelected AND WIOMode = KTE_Display);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: ShowRecibos
       // Objectivo.: Apresentar a Informação dos Recibos LA ou JM
       //==================================================================
       BEGSR ShowRecibos;
       ENDSR;


       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
       BEGSR RunPopUp;
          MONITOR;
          // IF (ECR_CM#NMFLD = 'CS');
          //    ECR_CS = %DEC($runPopUp(69:%CHAR(ECR_CS)):1:0);
          //    ECR_DSCS = $GetDescricao(69:%CHAR(ECR_CS));
          // ELSEIF (ECR_CM#NMFLD = 'NUMSEGLS');
          //    ECR_NUMSEGLS = %DEC($runPopUp(70:%CHAR(ECR_NUMSEGLS)):5:0);
          //    ECR_DSNUMSEG = $GetDescricao(70:%CHAR(ECR_NUMSEGLS));
          // ENDIF;
          ON-ERROR;
          ENDMON;
          ECR_CM#ROW = CM#CROW;
          ECR_CM#COL = CM#CCOL;
       ENDSR;
      /END-FREE
     PDspRecibos       E
