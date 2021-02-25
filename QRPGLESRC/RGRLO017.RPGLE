     H NOMAIN
     H*=======================================================
     FSGRLE017  CF   E             WORKSTN PREFIX(ECR_) USROPN
     F                                     INFDS(INFD#KEYS)
     F*=======================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Campos de Trabalho
     D WLibFile        S             10A   INZ('*LIBL')
     D WNomFile        S             10A   INZ('MSG01POR')
     P*=======================================================
     P* Processo..: $showDialog
     P* Objectivo.: Apresentar uma janela com uma mensagem ou
     P*             um pedido de decisão.
     P*
     P* Observação: Passar a tratar as variáveis &VAR(n), de
     P*             momento só está preparado para tratar a
     P*             uma única variável &VAR(1).
     P*=======================================================
     P $showDialog     B                   EXPORT
     D                 PI             1A
     D   CodMessage                   7A   CONST
     D   Vars                       100A   Const Options(*NOPASS)
     D   Options                      8A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WIcSair         S               N
     D WVars           S            100A
     D WOptions        S              8A
     D WTextOption     S             10A
     D WIdx            S              2S 0
     D WLenOption      S              2S 0
     D WTextMessage    S           1000A
     D WHeadMessage    S             50A
     D*=======================================================
      /FREE
        OPEN SGRLE017;
        WIcSair     = *ON;
        *IN50       = *OFF;
        WVars       = *BLANKS;
        WOptions    = *BLANKS;
        WTextOption = *BLANKS;
        IF (%PARMS() > 1);
           WVars = Vars;
           IF (%PARMS() > 2);
              WIcSair = *OFF;
              WOptions = Options;
              EXSR MakeOptLabel;
           ENDIF;
        ENDIF;

        IF (WOptions <> *BLANKS);
           *IN50 = *ON;
        ENDIF;
        ECR_MSGERRO  = CodMessage;
        WHeadMessage = $GetMessage(CodMessage);
        WTextMessage = $GetMessage(CodMessage:*BLANKS:'T');
        EXSR ExtractVars;
        ECR_OPTION = *BLANKS;

        $SetTexto(WHeadMessage:50:'C');
        ECR_DSHEAD   = $GetFirstLine();

        $SetTexto(WTextMessage:50:'J');
        ECR_DSLINE01 = $GetFirstLine();
        ECR_DSLINE02 = $GetNextLine();
        ECR_DSLINE03 = $GetNextLine();
        ECR_DSLINE04 = $GetNextLine();
        ECR_DSLINE05 = $GetNextLine();
        ECR_DSOPT = WTextOption;
        DOU (WIcSair OR *IN03 OR *IN12);
           ECR_OPTION = %SUBST(WOptions:1:1);
           EXFMT RGRLE01705;
           IF (ECR_OPTION <> *BLANKS);
              EXSR ValOption;
           ENDIF;
        ENDDO;

        CLOSE SGRLE017;
        RETURN ECR_OPTION;

        //====================================================
        // Subrotina..: ExtractVars
        // Objectivo..: Extrair variáveis e escrever no texto
        //====================================================
        BEGSR ExtractVars;
           IF (WVars <> *BLANKS);
              WTextMessage = $StrReplace('&VAR(1)':%TRIM(WVars):WTextMessage);
           ENDIF;
        ENDSR;

        //====================================================
        // Subrotina..: MakeOptLabel
        // Objectivo..: Construir a label das opções
        //====================================================
        BEGSR MakeOptLabel;
           WLenOption = %LEN(%TRIM(WOptions));
           WIdx = 1;
           WTextOption= '(';
           DOW (WIdx <= WLenOption);
              IF (WIdx > 1);
                 WtextOption = %TRIM(WTextOption)+',';
              ENDIF;
              WTextoption = %TRIM(WTextOption)+%SUBST(WOptions:WIdx:1);
              WIdx += 1;
           ENDDO;
           WTextOption= %TRIM(WTextOption)+')';
        ENDSR;

        //====================================================
        // Subrotina..: ValOption
        // Objectivo..: Validar se a resposta é válida
        //====================================================
        BEGSR ValOption;
           WLenOption = %LEN(%TRIM(WOptions));
           WIdx = 1;
           DOW (WIdx <= WLenOption);
              IF (%SUBST(WOptions:Widx:1) = ECR_OPTION);
                 WIcSair = *ON;
              ENDIF;
              WIdx += 1;
           ENDDO;
        ENDSR;
      /END-FREE
     P $showDialog     E

     P*=======================================================
     P* Processo..: $SendMessage
     P* Objectivo.: Enviar uma mensagem de erro
     P*=======================================================
     P $sendMessage    B                   EXPORT
     D                 PI
     D   PRM_CodMsg                   7A   CONST
     D
     D SndPgmMsg       PR                  ExtPgm('QMHSNDPM')
     D   MessageID                    7A   Const
     D   QualMsgF                    20A   Const
     D   MsgData                    256A   Const options(*varsize)
     D   MsgDtaLen                   10I 0 Const
     D   MsgType                     10A   Const
     D   CallStkEnt                  10A   Const
     D   CallStkCnt                  10I 0 Const
     D   MessageKey                   4A
     D   ErrorCode                32766A   options(*varsize)
     D
     DErrorCode        DS
     D  ErrCBytePrv                  10I 0
     D  ErrCByteAvl                  10I 0
     D
     D WMsg            S            200A   INZ('This is a test')
     D WMsgKey         S              4A
     D*=============================================================
      /FREE

        SndPgmMsg(PRM_CodMsg: WNomFile+WLibFile:
          WMsg  : %Len(%TrimR(WMsg)) :'*INFO':'*EXT':0:
          WMsgKey:ErrorCode);


       RETURN;
      /END-FREE
     P $sendMessage    E

     P*=======================================================
     P* Processo..: $getMessage
     P* Objectivo.: Obter Mensagem de erro do ficheiro de
     P*             mensagens.
     P*=======================================================
     P $getMessage     B                   EXPORT
     D                 PI          1000A
     D   PRM_CodMsg                   7A   CONST
     D   PRM_Vars                   100A   Const Options(*NOPASS)
     D   PRM_Tipo                     1A   Const Options(*NOPASS)
     D
     D
     DRetMessage       PR                  EXTPGM('QMHRTVM')
     D  pMsgInfo                  32767a   options(*varsize)
     D  pMsgInfoLen                  10i 0 const
     D  pReturnFmt                    8a   const
     D  pMsgId                        7a   const
     D  pMsgFile                     20a   const
     D  pMsgData                  32767a   const options(*varsize)
     D  pMsgDataLen                  10i 0 const
     D  pReplSubst                   10a   const
     D  pRtnCtrl                     10a   const
     D  pApiErrorDS               32767a   options(*nopass:*varsize)
     D  pRtvOption                   10a   CONST options(*nopass)
     D  pCCSID                       10i 0 const options(*nopass)
     D  pReplCCSID                   10i 0 const options(*nopass)
     D
     DRTVM0300         DS
     D  RtvMByteRtn                  10I 0
     D  RtvMByteAvl                  10I 0
     D  RtvMMsgSvrty                 10I 0
     D  RtvMAltIdx                   10I 0
     D  RtvMAltOpt                    9A
     D  RtvMLogInd                    1A
     D  RtvMMsgId                     7A
     D  RtvMRsrvd                     3A
     D  RtvMNSVF                     10I 0
     D  RtvMCCSID1                   10I 0
     D  RtvMCCSID2                   10I 0
     D  RtvMCCSID3                   10I 0
     D  RtvMOffDR                    10I 0
     D  RtvMLenDRRtn                 10I 0
     D  RtvMLenDRAvl                 10I 0
     D  RtvMOffMsg                   10I 0
     D  RtvMLenMsgRtn                10I 0
     D  RtvMLenMsgAvl                10I 0
     D  RtvMOffMsgH                  10I 0
     D RtvMLenMsgHRtn                10I 0
     D RtvMLenMsgHAvl                10I 0
     D  RtvMOffSVF                   10I 0
     D  RtvMLenSVFRtn                10I 0
     D  RtvMLenSVFAvl                10I 0
     D  RtvMLenSVFElm                10I 0
     D  RtvMVarAra                 4096A
     D
     DERRC0100         DS
     D  ErrCBytePrv                  10I 0 Inz(272)
     D  ErrCByteAvl                  10I 0
     D  ErrCExcId                     7A
     D  ErrCRsrvd                     1A
     D  ErrCExcDta                  256A
     D
     D WITMsgFL        S            256A
     D WITMsgSL        S           2048A
     D WGravidade      S              3S 0
     D WResultado      S           1000A
     D*=============================================================
      /FREE

        RetMessage(RTVM0300:%SIZE(RTVM0300):'RTVM0300':PRM_CodMsg:
                   WNomFile+WLibFile:'*BLANKS':*ZEROS:'*NO':'*YES':
                   ERRC0100:'*MSGID':0:0);
        IF (ErrCExcId = *Blanks);
           WITMsgFL = %SubSt(RTVM0300:RTVMOffMsg+1:RTVMLenMsgRtn);
           WITMsgSL = %SubSt(RTVM0300:RTVMOffMsgH+1:RTVMLenMsgHRtn);
           WGravidade = RtvMMsgSvrty;
           WResultado = %SubSt(WITMsgFL:1:100);
           IF (%Parms() > 2);
             IF (PRM_Tipo = 'C');
                WResultado = %CHAR(WGravidade);
             ELSEIF (PRM_TIPO = 'T');
                WResultado = %SubSt(WITMsgSL:1:1000);
             ELSEIF (PRM_TIPO = 'M');
                WResultado = %SubSt(WITMsgFL:1:100);
             ENDIF;
           ENDIF;
        ELSE;
           WResultado = 'Mensagem "'+PRM_CodMsg+'" Inexistente.';
        ENDIF;

        RETURN WResultado;
      /END-FREE
     P $getMessage     E
