     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     FFSECP01001IF   E           K DISK    PREFIX(SEC_)
     FFGRLE010  CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Informação do Menu
     D MenuOption      PR              N
     D    WNumMenu                    5S 0 CONST
     D    WOriginal                   5S 0 CONST
     D
     D*==> Invocar um programa de forma dinâmica
     D CallOption      PR                  EXTPGM(DynamicCall)
     D
     D*=> Registo da opção a seleccionar
     D LstOption       DS                  QUALIFIED DIM(24)
     D   CdItem                       5S 0
     D   TipItem                      1A
     D   Ptr#Opt                       *
     D   Ptr#Des                       *
     D   Ptr#Cmd                       *
     D WIdx            S              5S 0
     D
     D WOption         S              2A   BASED(Ptr#Opt)
     D WDescr          S             27A   BASED(Ptr#Des)
     D WCommand        S              6A   BASED(Ptr#Cmd)
     D DynamicCall     S             10A   INZ(*BLANKS)
     D WNumMenu        S              5S 0 INZ(15)
     D PNumMenu        S              5A
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PNumMenu
      /FREE
         IF (%parms() > 2);
            DSPLY 'TESTE';
         ENDIF;


         MONITOR;
            WNumMenu = %DEC(PNumMenu:5:0);
            IF (WNumMenu = *ZEROS);
              WNumMenu = 15;
            ENDIF;
         ON-ERROR;
         ENDMON;

         //--------------------------------------------
         // Transformar os campos de Ecrã num Array
         //--------------------------------------------
         EXSR changeFields;
         MenuOption(WNumMenu:WNumMenu);

         *INLR = *ON;
         RETURN;

       //==============================================================
       // Subrotina.: changeFields
       // Objectivo.: Gravar endereços dos campos de ecrã, utilização
       //             de apontadores, para aceder aos endereços dos
       //             campos de ecrã.
       //==============================================================
       BEGSR changeFields;
          LstOption(01).Ptr#Opt = %ADDR(Ecr_Opt01);
          LstOption(01).Ptr#Des = %ADDR(Ecr_Des01);
          LstOption(01).Ptr#Cmd = %ADDR(Ecr_Cmd01);
          LstOption(02).Ptr#Opt = %ADDR(Ecr_Opt02);
          LstOption(02).Ptr#Des = %ADDR(Ecr_Des02);
          LstOption(02).Ptr#Cmd = %ADDR(Ecr_Cmd02);
          LstOption(03).Ptr#Opt = %ADDR(Ecr_Opt03);
          LstOption(03).Ptr#Des = %ADDR(Ecr_Des03);
          LstOption(03).Ptr#Cmd = %ADDR(Ecr_Cmd03);
          LstOption(04).Ptr#Opt = %ADDR(Ecr_Opt04);
          LstOption(04).Ptr#Des = %ADDR(Ecr_Des04);
          LstOption(04).Ptr#Cmd = %ADDR(Ecr_Cmd04);
          LstOption(05).Ptr#Opt = %ADDR(Ecr_Opt05);
          LstOption(05).Ptr#Des = %ADDR(Ecr_Des05);
          LstOption(05).Ptr#Cmd = %ADDR(Ecr_Cmd05);
          LstOption(06).Ptr#Opt = %ADDR(Ecr_Opt06);
          LstOption(06).Ptr#Des = %ADDR(Ecr_Des06);
          LstOption(06).Ptr#Cmd = %ADDR(Ecr_Cmd06);
          LstOption(07).Ptr#Opt = %ADDR(Ecr_Opt07);
          LstOption(07).Ptr#Des = %ADDR(Ecr_Des07);
          LstOption(07).Ptr#Cmd = %ADDR(Ecr_Cmd07);
          LstOption(08).Ptr#Opt = %ADDR(Ecr_Opt08);
          LstOption(08).Ptr#Des = %ADDR(Ecr_Des08);
          LstOption(08).Ptr#Cmd = %ADDR(Ecr_Cmd08);
          LstOption(09).Ptr#Opt = %ADDR(Ecr_Opt09);
          LstOption(09).Ptr#Des = %ADDR(Ecr_Des09);
          LstOption(09).Ptr#Cmd = %ADDR(Ecr_Cmd09);
          LstOption(10).Ptr#Opt = %ADDR(Ecr_Opt10);
          LstOption(10).Ptr#Des = %ADDR(Ecr_Des10);
          LstOption(10).Ptr#Cmd = %ADDR(Ecr_Cmd10);
          LstOption(11).Ptr#Opt = %ADDR(Ecr_Opt11);
          LstOption(11).Ptr#Des = %ADDR(Ecr_Des11);
          LstOption(11).Ptr#Cmd = %ADDR(Ecr_Cmd11);
          LstOption(12).Ptr#Opt = %ADDR(Ecr_Opt12);
          LstOption(12).Ptr#Des = %ADDR(Ecr_Des12);
          LstOption(12).Ptr#Cmd = %ADDR(Ecr_Cmd12);
          LstOption(13).Ptr#Opt = %ADDR(Ecr_Opt13);
          LstOption(13).Ptr#Des = %ADDR(Ecr_Des13);
          LstOption(13).Ptr#Cmd = %ADDR(Ecr_Cmd13);
          LstOption(14).Ptr#Opt = %ADDR(Ecr_Opt14);
          LstOption(14).Ptr#Des = %ADDR(Ecr_Des14);
          LstOption(14).Ptr#Cmd = %ADDR(Ecr_Cmd14);
          LstOption(15).Ptr#Opt = %ADDR(Ecr_Opt15);
          LstOption(15).Ptr#Des = %ADDR(Ecr_Des15);
          LstOption(15).Ptr#Cmd = %ADDR(Ecr_Cmd15);
          LstOption(16).Ptr#Opt = %ADDR(Ecr_Opt16);
          LstOption(16).Ptr#Des = %ADDR(Ecr_Des16);
          LstOption(16).Ptr#Cmd = %ADDR(Ecr_Cmd16);
          LstOption(17).Ptr#Opt = %ADDR(Ecr_Opt17);
          LstOption(17).Ptr#Des = %ADDR(Ecr_Des17);
          LstOption(17).Ptr#Cmd = %ADDR(Ecr_Cmd17);
          LstOption(18).Ptr#Opt = %ADDR(Ecr_Opt18);
          LstOption(18).Ptr#Des = %ADDR(Ecr_Des18);
          LstOption(18).Ptr#Cmd = %ADDR(Ecr_Cmd18);
          LstOption(19).Ptr#Opt = %ADDR(Ecr_Opt19);
          LstOption(19).Ptr#Des = %ADDR(Ecr_Des19);
          LstOption(19).Ptr#Cmd = %ADDR(Ecr_Cmd19);
          LstOption(20).Ptr#Opt = %ADDR(Ecr_Opt20);
          LstOption(20).Ptr#Des = %ADDR(Ecr_Des20);
          LstOption(20).Ptr#Cmd = %ADDR(Ecr_Cmd20);
          LstOption(21).Ptr#Opt = %ADDR(Ecr_Opt21);
          LstOption(21).Ptr#Des = %ADDR(Ecr_Des21);
          LstOption(21).Ptr#Cmd = %ADDR(Ecr_Cmd21);
          LstOption(22).Ptr#Opt = %ADDR(Ecr_Opt22);
          LstOption(22).Ptr#Des = %ADDR(Ecr_Des22);
          LstOption(22).Ptr#Cmd = %ADDR(Ecr_Cmd22);
          LstOption(23).Ptr#Opt = %ADDR(Ecr_Opt23);
          LstOption(23).Ptr#Des = %ADDR(Ecr_Des23);
          LstOption(23).Ptr#Cmd = %ADDR(Ecr_Cmd23);
          LstOption(24).Ptr#Opt = %ADDR(Ecr_Opt24);
          LstOption(24).Ptr#Des = %ADDR(Ecr_Des24);
          LstOption(24).Ptr#Cmd = %ADDR(Ecr_Cmd24);
       ENDSR;
      /END-FREE
     P*=======================================================
     P* Processo..: MenuOption
     P* Objectivo.: Gererir as Opções do Menu recursivamente
     P*=======================================================
     P MenuOption      B                   EXPORT
     D MenuOption      PI              N
     D    PRM_NumMenu                 5S 0 CONST
     D    PRM_Origin                  5S 0 CONST
     D
     D WModoSimples    S               N   INZ(*ON)
     D WNomMenu        S             10A
     D WResultado      S               N
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D*=======================================================
      /FREE
         WSessionID = $SETFILES('FSECP014 AS A INNER JOIN FSECP010 AS B ON +
                     (A.CDITEM = B.CDPROGR)');
         $SetFields('FSECP014':'A');
         $SetFields('FSECP010':'B':*OFF);
         EXSR SetParms;
         EXSR  inzRecords;
         EXSR  getRecords;

         //-----------------------------------
         // ForCompr as Opções de Menu
         //-----------------------------------
         WModoSimples = *ON;

         //-----------------------------------
         // Gerir as Opções do Menu
         //-----------------------------------
         DOW (NOT *IN03 AND NOT *IN12);
            DS#HEADER    = $getHeader(WNomMenu:WNomMenu);
            ECR_DS#PGM = $AlignText('Opções disponíveis no Menu':57:'C');
            ECR_S#PGM  = WNomMenu;
            WRITE RGRLE01000;
            DS#FOOTER    = $getFooter(S#PGM:'FGRLE010':*BLANKS);
            WRITE RGRLE01099;
            ECR_Option = *BLANKS;
            IF (WModoSimples);
               //$sendMessage('ENT0002');
               //$sendMessage('ENT0001');
               EXFMT RGRLE01005;
            ELSE;
               //$sendMessage('ENT0001');
               //$sendMessage('ENT0002');
               EXFMT RGRLE01007;
            ENDIF;
            IF (Func#Key = KEY#F05);
               WModoSimples = NOT WModoSimples;
               EXSR SetParms;
               EXSR inzRecords;
               EXSR getRecords;
            ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
              ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
               EXSR inzRecords;
               EXSR getRecords;
            ELSEIF (Func#Key = KEY#F12) AND (PRM_NumMenu = PRM_Origin);
               *IN12 = *OFF;
            ELSEIF (Func#Key = KEY#Enter);
               IF (ECR_Option <> *BLANKS);
                  EXSR SelOption;
                  EXSR inzRecords;
                  EXSR getRecords;
                  *IN12 = *OFF;
               ENDIF;
            ENDIF;
         ENDDO;
         $RmvDBSession(WSessionID);
         RETURN WResultado;

       //==============================================================
       // Subrotina.: setParms
       // Objectivo.: Colocar Parâmetros
       //==============================================================
       BEGSR setParms;
          //-----------------------------------
          // Obter registos para as opções
          //-----------------------------------
          // $SETDBSession(WSessionID);
          IF (WModoSimples);
             $SetPage(13);
          ELSE;
             $SetPage(25);
          ENDIF;
          $SETCRITERIA('CDMENU = '+ %CHAR(PRM_NumMenu));
          $SETSORT('NUOPTION');
       ENDSR;

       //==============================================================
       // Subrotina.: selOption
       // Objectivo.: Seleccionar o registo
       //==============================================================
       BEGSR selOption;
          //---------------------------------------
          // Converter Comando ou a Opção
          //---------------------------------------
          IF (%Check('0123456789': %trim(ECR_Option)) = *ZEROS);
             WIdx = *ZEROS;
             DOW (WIdx < 24);
                WIdx += 1;
                Ptr#Opt  = LstOption(WIdx).Ptr#Opt;
                IF (WOption <> *BLANKS);
                   IF (%DEC(WOption:2:0) = %DEC(ECR_OPTION:6:0));
                      Ptr#Cmd    = LstOption(WIdx).Ptr#Cmd;
                      ECR_Option = WCommand;
                      LEAVE;
                   ENDIF;
                ELSE;
                   LEAVE;
                ENDIF;
             ENDDO;
          ENDIF;

          //---------------------------------------
          // Converter a opção no comando
          //---------------------------------------
          IF (ECR_Option <> *BLANKS);
             EXSR runComando;
          ELSE;
             // Erro opção inexistente
             EXFMT RGRLE01030;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: runComando
       // Objectivo.: Obter o Item associado ao comando, e executar.
       //==============================================================
       BEGSR runComando;
          CHAIN (ECR_Option) RSECP010;
          IF (%FOUND());
             EXSR CallPGM;
          ELSE;
             // Erro comando inexistente
             EXFMT RGRLE01030;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: CallPGM
       // Objectivo.: chamar um programa externo
       //==============================================================
       BEGSR CallPgm;
          IF NOT $ValAccess(SEC_NMPROGR:'':99999);
             EXFMT RGRLE01032;
          ELSE;
             SELECT;
                WHEN (SEC_CDTIPO = 'M'); // Menu
                   IF (SEC_CDPROGR <> PRM_ORIGIN);
                      MenuOption(SEC_CDPROGR:PRM_Origin);
                   ENDIF;
                WHEN (SEC_CDTIPO = 'P'); // Programa
                   MONITOR;
                      DynamicCall = SEC_NMPROGR;
                      CallOption();
                   ON-ERROR;
                      EXFMT RGRLE01030;
                   ENDMON;

                WHEN (SEC_CDTIPO = 'C'); // Comando
                   $shellExec(Wcommand);

                WHEN (SEC_CDTIPO = 'O'); // Comando
                   $shellExec('STRS36PRC PRC('+%TRIM(SEC_NMPROGR)+
                               ') CURLIB(QS36F)');
             ENDSL;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: getRecords
       // Objectivo.: Obter as Opções do Menu ao que o Utilizador
       //             está autorizado.
       //==============================================================
       BEGSR getRecords;
          WIdx = *ZEROS;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                IF ($GetField('A.CDMENU') = $GetField('B.CDITEM'));
                   WNomMenu = $GetField('B.NMPROGR');
                ELSE;
                   WIdx += 1;
                   LstOption(WIdx).CdItem  = %DEC($GetField('A.CDITEM'):5:0);
                   LstOption(WIdx).TipItem  = $GetField('B.CDTIPO');
                   Ptr#Opt  = LstOption(WIdx).Ptr#Opt;
                   Ptr#Des  = LstOption(WIdx).Ptr#Des;
                   Ptr#Cmd  = LstOption(WIdx).Ptr#Cmd;
                   WOption  = $GetField('A.NUOPTION');
                   WDescr   = $GetField('B.DSPROGR');
                   WCommand = $GetField('A.CMITEM');
                ENDIF;
                IF ((WModoSimples) AND (WIdx = 12)) OR
                   ((NOT WModoSimples) AND (WIdx = 24));
                   LEAVE;
                ENDIF;
                $getNext();
             ENDDO;
             IF (NOT $IsLastPage());
                ECR_MORE = 'Mais...';
             ELSE;
                ECR_MORE = 'Fim';
             ENDIF;
          ELSE;
             ECR_MORE = 'Fim';
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: inzRecords
       // Objectivo.: Inicializar os registos
       //==============================================================
       BEGSR inzRecords;
          WIdx = *ZEROS;
          DOW (WIdx < 24);
             WIdx += 1;
             Ptr#Opt  = LstOption(WIdx).Ptr#Opt;
             Ptr#Des  = LstOption(WIdx).Ptr#Des;
             Ptr#Cmd  = LstOption(WIdx).Ptr#Cmd;
             WOption  = *BLANKS;
             WDescr   = *BLANKS;
             WCommand = *BLANKS;
          ENDDO;
       ENDSR;

      /END-FREE
     P MenuOption      E
