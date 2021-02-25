     H NOMAIN
     H*=======================================================
     H* Autor......: Luis Galhoz
     H* Processo...: $ConfAltera
     H* Objectivo..: Validar se as Alterações são para continuar
     H*              ou para remover.
     H*=======================================================
     FSDBSE022  CF   E             WORKSTN INFDS(INFD#KEYS)
     F                                     USROPN PREFIX(ECR_)
     F*===========================================================
     F* Indicadores  Descrição
     F* -----------  ---------------------------------------------
     F*      03      Tecla F3
     F*      06      Tecla F6
     F*      12      Tecla F12
     F*      30      FTBLP010
     F*      50      Tecla PagDn
     F*      51      Tecla PagUp
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     F*=====================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SDBSO012

     D  WRetCode       S             15A
     P*=======================================================
     P* Processo..: $ConfAltera
     P* Descrição.: Confirmar se as Alterações são para
     P*             Continuar.
     C*=======================================================
     P $ConfAltera     B                   EXPORT
     D                 PI             1A
     D
     D*==> Campos de Trabalho
     D WRetCode        S              1A
     D*===========================================================
      /FREE
       OPEN  SDBSE022;
       //-------------------------------------------
       // Validar se Existem Alterações Pendentes
       //-------------------------------------------
       WRetCode = *BLANKS;
+----  IF  $IsCmtPending();
          ExSr DspMensagem;
       ENDIF;
       CLOSE SDBSE022;
       RETURN WRetCode;

       //==============================================================
       // Subrotina.: dspMensagem
       // Objectivo.: Mostrar a informação sobre o Código
       //==============================================================
       BEGSR dspMensagem;
          DS#HEADER   = $getHeader(S#PGM:'RDBSO02205');
          ECR_DS#FNC = $ALIGNTEXT('Pedido para abortar uma alteração':60:'C');
          ECR_DS#PGMR = ECR_DS#PGM;
          ECR_DS#FNCR = ECR_DS#FNC;
          WRITE RDBSO02200;
          DS#FOOTER     = $getFooter(S#PGM:'RDBSO02208':*BLANKS);
          ECR_DS#KEY01R = ECR_DS#KEY01;
          ECR_DS#KEY02R = ECR_DS#KEY02;
          WRITE RDBSO02299;
          ECR_Opcao = *BLANKS;
          DOW (ECR_OPCAO<>'N') AND (ECR_OPCAO<>'S');
             EXFMT RDBSO02205;
             IF (Func#Key = KEY#F03) OR (Func#Key = KEY#F12);
                WRetCode = 'N';
                LEAVESR;
             ENDIF;
             WRetCode = ECR_Opcao;
          ENDDO;
       ENDSR;
      /END-FREE
     P $ConfAltera     E
