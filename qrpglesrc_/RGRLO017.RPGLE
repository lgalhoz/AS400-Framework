     H NOMAIN
     H*=======================================================
      /Copy QRPGLESRC,SGRLO010
     D WLibFile        S             10A   INZ('*LIBL')
     D WNomFile        S             10A   INZ('MSG01POR')
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
      /FREE

        SndPgmMsg(PRM_CodMsg: WNomFile+WLibFile:
          WMsg  : %Len(%TrimR(WMsg)) : '*INFO':'*':0:
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
     D                 PI           100A
     D   PRM_CodMsg                   7A   CONST
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
     D WResultado      S            100A
      /FREE

        RetMessage(RTVM0300:%SIZE(RTVM0300):'RTVM0300':PRM_CodMsg:
                   WNomFile+WLibFile:'*BLANKS':*ZEROS:'*NO':'*YES':
                   ERRC0100:'*MSGID':0:0);
        IF (ErrCExcId = *Blanks);
           WITMsgFL = %SubSt(RTVM0300:RTVMOffMsg+1:RTVMLenMsgRtn);
           WITMsgSL = %SubSt(RTVM0300:RTVMOffMsgH+1:RTVMLenMsgHRtn);
           WGravidade = RtvMMsgSvrty;
           WResultado = %SubSt(WITMsgFL:1:100);
           IF (%Parms() > 1);
             IF (PRM_Tipo = 'C');
                WResultado = %CHAR(WGravidade);
             ELSEIF (PRM_TIPO = 'T');
                WResultado = %SubSt(WITMsgSL:1:100);
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
