     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDPO010
     H* Descrição.: Manuenção de Apólices
     H*==================================================================
     FSGRLE017  CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Apresentar uma janela
     D showDialog      PR             1A
     D   CodMessage                   7A   CONST
     D   TipMessage                   1A   Const Options(*NOPASS)
     D   Options                      8A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WLibFile        S             10A   INZ('*LIBL')
     D WNomFile        S             10A   INZ('MSG01POR')
     D WOption         S              1A
     D*=======================================================
      /FREE
         ShowDialog('PRD0100');
         WOption = ShowDialog('PRD0100':*BLANKS:'SN');
         WOption = ShowDialog('PRD0100':*BLANKS:'S');
         ShowDialog('PRD0100');
         WOption = ShowDialog('PRD0100':*BLANKS:'SNTY');
         DSPLY WOption;
         RETURN;
      /END-FREE

     P*=======================================================
     P* Processo..: showDialog
     P* Objectivo.: Apresentar uma janela com uma mensagem ou
     P*             um pedido de decisão.
     P*=======================================================
     P showDialog      B                   EXPORT
     D                 PI             1A
     D   CodMessage                   7A   CONST
     D   Vars                         1A   Const Options(*NOPASS)
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
        WIcSair     = *ON;
        *IN50       = *OFF;
        WVars       = *BLANKS;
        WOptions    = *BLANKS;
        WTextOption = *BLANKS;
        IF (%PARMS() > 1);
           WVars =Vars;
           IF (%PARMS() > 2);
              WIcSair = *OFF;
              WOptions = Options;
              EXSR MakeOptLabel;
           ENDIF;
        ENDIF;

        IF (WOptions <> *BLANKS);
           *IN50 = *ON;
        ENDIF;

        WHeadMessage = $GetMessage(CodMessage);
        WTextMessage = $GetMessage(CodMessage:'T');
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
           ECR_OPTION = *BLANKS;
           EXFMT RGRLE01705;
           IF (ECR_OPTION <> *BLANKS);
              EXSR ValOption;
           ENDIF;
        ENDDO;

        RETURN ECR_OPTION;

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
     P showDialog      E
