     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RCATO009
     H* Descrição.: Pré-Visualização dos Textos
     H*==================================================================
     FSCATE009  CF   E             WORKSTN SFILE(RCATE00902:REC#)
     F                                     INFDS(INFD#KEYS)
     F                                     PREFIX(ECR_)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO014
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
     D
     D RetTexto        PR                  EXTPGM('RCATO008')
     D   WCodPagina                   5S 0 CONST
     D   WTipItem                     2A   CONST
     D   WCodProduo                   5S 0 CONST
     D   WCodItem1                    9S 0 CONST
     D   WCodItem2                    9S 0 CONST
     D   #RetTexto                         CONST LIKE(#RETTEXTO) DIM(1000)
     D   WLargura                     3S 0 CONST
     D
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D WRecPage        S              5S 0
     D WCurPage        S              5S 0
     D WCursor         S              5S 0
     D WSalto          S              5S 0
     D
     D WCodPagina      S              5S 0
     D WCodProduto     S              5S 0
     D WTipItem        S              2A
     D WCodItem1       S              9S 0
     D WCodItem2       S              9S 0
     D
     D REC#            S              4  0
     D*=================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodPagina
     C                   PARM                    WTipItem
     C                   PARM                    WCodProduto
     C                   PARM                    WCodItem1
     C                   PARM                    WCodItem2
      /FREE

        RetTexto(WCodPagina:WTipItem:WCodProduto:
                   WCodItem1:WCodItem2:#RetTexto:73);
        WRITE RCATE00900;

           //---------------------------------------
           // Inicializar variáveis de trabalho
           //---------------------------------------
           WCurPage = *ZEROS;
           WCursor  = 1;
           WSalto   = 1;

           EXSR inzRecords;
           EXSR getRecords;
           DS#FOOTER     = $getFooter(S#PGM:'RCATE00901':*BLANKS);
           ECR_DS#KEY01R = ECR_DS#KEY01;
           ECR_DS#KEY02R = ECR_DS#KEY02;
           WRITE RCATE00998;
           WRITE RCATE00999;
           DOW (NOT *IN03 AND NOT *IN12);
              EXFMT RCATE00901;
              IF (Func#Key = KEY#PgDown) AND (WRecPage = *ZEROS);
                 WCurPage += 1;
                 EXSR inzRecords;
                 EXSR getRecords;
              ELSEIF (Func#Key = KEY#PgUp) AND (WCurPage > *ZEROS);
                 WCurPage -= 1;
                 EXSR inzRecords;
                 EXSR getRecords; // Carregar Lista com Registos
              ELSEIF (Func#Key = KEY#F19); // Deslocamento à Esquerda
                 WCursor = 1;
                 EXSR inzRecords;
                 EXSR getRecords;
              ELSEIF (Func#Key = KEY#F20); // Deslocamento à Direita
                 WCursor = 30;
                 EXSR inzRecords;
                 EXSR getRecords;
              ELSEIF (Func#Key = KEY#Enter);
                 WCursor = ECR_CURSOR;
                 IF (WCursor = *ZEROS);
                    WCursor = 1;
                 ENDIF;
                 IF (ECR_LINHA > *ZEROS);
                    WSalto = (ECR_LINHA - %INT(ECR_LINHA/14)*14);
                    WPosIni = ECR_LINHA;
                 ENDIF;
                 ECR_LINHA = *ZEROS;
                 EXSR inzRecords;
                 EXSR getRecords;
              ENDIF;
           ENDDO;

        *INLR = *ON;
        RETURN;

        //==============================================================
        // Subrotina.: InzRecords
        // Objectivo.: Inicializar a lista de registos
        //==============================================================
        BEGSR inzRecords;
           REC#  = *ZEROS;   // Controlo de Registos da Lista de Ecrã
           *In53 = *On;      // Limpar Lista de Ecrã
           *In52 = *On;      // Mostrar a Lista de Ecrã
           WRITE RCATE00901; // Limpar Lista
           *In53 = *Off;     // Repor Indicador
           *In54 = *Off;     // Repor Indicador
           REC# = *ZEROS;
        ENDSR;

       //==============================================================
       // Subrotina.: getRecords
       // Objectivo.: Obter os ergistos a apresentar ao utilizador
       //==============================================================
       BEGSR getRecords;
          WPosIni = (WCurPage * 14) + WSalto;
          WPosFim = (WPosIni + 14);
          MONITOR;
             DOW (NOT #RetTexto(WPosIni).IsEmpty) AND (WPosIni<WPosFim);
                REC# += 1;
                // IF  (CUR_LINE = 82);
                //     ECR_TEXTO = $AlignText('.../...':72:'R');
                // ENDIF;
                ECR_TEXTO = %SUBST(#RetTexto(WPosIni).Texto:WCursor);
                WRITE RCATE00902;
                WPosIni += 1;
                ECR_LINHA = WPosIni;
             ENDDO;
             WRecPage = (WPosFim - WPosIni);
             IF (WRecPage = *ZEROS);
                *IN54 = *OFF;
             ELSE;
                *IN54 = *ON;
             ENDIF;
             IF (REC# = *ZEROS);
                *IN52 = *OFF;
             ENDIF;
          ON-ERROR;
          ENDMON;
       ENDSR;
      /END-FREE
