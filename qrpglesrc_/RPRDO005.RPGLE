     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDO005
     H* Descrição.: Pré-Visualização do texto das Actas
     H*==================================================================
     FSPRDE005  CF   E             WORKSTN SFILE(RPRDE00502:REC#)
     F                                     INFDS(INFD#KEYS)
     F                                     PREFIX(ECR_)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO014
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SSECO010
     D
     D RetTexto        PR                  EXTPGM('RPRDO008')
     D   WGexap                        *   CONST
     D   WGBaseDio                     *   CONST
     D   #RetTexto                         CONST LIKE(#RETTEXTO) DIM(1000)
     D   CdGrupo                      5S 0 CONST
     D
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D WRecPage        S              5S 0
     D WCurPage        S              5S 0
     D WCursor         S              5S 0
     D
     D WPtrHistorico   S               *
     D WPtrDiario      S               *
     D WCdGrupo        S              5S 0
     D
     D REC#            S              4  0
     D*=================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WPtrDiario
     C                   PARM                    WCdGrupo
      /FREE

        RetTexto(WPtrHistorico:WPtrDiario:#RetTexto:WCdGrupo);
        WRITE RPRDE00500;

           //---------------------------------------
           // Inicializar variáveis de trabalho
           //---------------------------------------
           WCurPage = *ZEROS;
           WCursor  = 1;

           EXSR inzRecords;
           EXSR getRecords;
           DS#FOOTER     = $getFooter(S#PGM:'RPRDE00501':*BLANKS);
           ECR_DS#KEY01R = ECR_DS#KEY01;
           ECR_DS#KEY02R = ECR_DS#KEY02;
           WRITE RPRDE00598;
           WRITE RPRDE00599;
           DOW (NOT *IN03 AND NOT *IN12);
              EXFMT RPRDE00501;
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
                 WCursor = 18;
                 EXSR inzRecords;
                 EXSR getRecords;
              ELSEIF (Func#Key = KEY#Enter);
                 WCursor = ECR_CURSOR;
                 IF (WCursor = *ZEROS);
                    WCursor = 1;
                 ENDIF;
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
           WRITE RPRDE00501; // Limpar Lista
           *In53 = *Off;     // Repor Indicador
           *In54 = *Off;     // Repor Indicador
           REC# = *ZEROS;
        ENDSR;

       //==============================================================
       // Subrotina.: getRecords
       // Objectivo.: Obter os ergistos a apresentar ao utilizador
       //==============================================================
       BEGSR getRecords;
          WPosIni = (WCurPage * 14) + 1;
          WPosFim = (WPosIni + 14);
          MONITOR;
             DOW (NOT #RetTexto(WPosIni).IsEmpty) AND (WPosIni<WPosFim);
                REC# += 1;
                // IF  (CUR_LINE = 82);
                //     ECR_TEXTO = $AlignText('.../...':72:'R');
                // ENDIF;
                ECR_TEXTO = %SUBST(#RetTexto(WPosIni).Texto:WCursor);
                WRITE RPRDE00502;
                WPosIni += 1;
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
