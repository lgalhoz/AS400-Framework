     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO004
     H* Descrição.: Disponibilizar Histórico de Alterações das Apólices
     H* Observação: Esta rotina lista todos os ficheiros de Histórico
     H*             onde consta a apólice, e devolve o nome do ficheiro
     H*             para que o utilizador possa consultar os dados.
     H*==================================================================
     FGBASEGRA  IF   E           K DISK    PREFIX(AUT_)
     FGBASEGRO  IF   E           K DISK    PREFIX(OR_)
     FSPRDE004  CF   E             WORKSTN SFILE(RPRDE00402:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO020
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*================================================================
     D*--> Invocar um comando de AS/400 externo: -----------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D*==> Base Auto (Original)
     D AppAuto         PR                  EXTPGM('GP010X')
     D   Option                       1A   CONST
     D   RamApol                      9A   CONST
     D   Data                         8  0 CONST
     D
     D*==> Base Outros Ramos (Original)
     D AppOutros       PR                  EXTPGM('GP015X')
     D   Option                       1A   CONST
     D   RamApol                      9A   CONST
     D   Data                         8  0 CONST
     D
     D*--> Lista de Trabalho de Ficheiros
     D Lst#Hist        DS                  QUALIFIED DIM(50)
     D   Date                         8S 0 INZ(*ZEROS)
     D   File                        10A   INZ(*BLANKS)
     D   Lib                         10A   INZ(*BLANKS)
     D   CdMot                        2S 0 INZ(*ZEROS)
     D   Origem                       1A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D WIdx            S              5S 0 INZ(*ZEROS)
     D WIdx2           S              5S 0 INZ(*ZEROS)
     D
     D*--> Campos de Trabalho
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              5S 0
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WSelected       S               N
     D
     D WRamoApolice    S              9A
     D WNomLib         S             10A
     D WNomFile        S             10A
     D WPrefix         S              5A
     D WCdRamo         S              2S 0
     D WNuapol         S              7S 0
     D WDatCria        S              8S 0
     D WDatAltera      S              8S 0
     D WIcCarrega      S               N
     D WMesFich        S              2S 0
     D WMesIni         S              2S 0
     D WMesTmp         S              2S 0
     D PCdRamo         S              2S 0
     D PNuApol         S              7S 0
     D
     D WIcSelect       S               N
     D WTemporarios    S              5S 0
     D WMetadados      S              5S 0
     D WCdBatch        S              9S 0
     D WNuSequ         S              5S 0
     D WCdFich         S              5S 0
     D WDTCria         S             10A
     D WAnoMes         S              6S 0
     D WCdLetra        S              1A
     D
     D WLstParms       DS                  LIKEDS(LST#ValField) DIM(10)
     D WLstFiles       DS                  LIKEDS(LST#TmpFile) DIM(9999)
     D
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCDRAMO
     C                   PARM                    PNUAPOL
     C                   PARM                    WNomLib
     C                   PARM                    WNomFile
      /FREE

       MONITOR;
          WCdRamo  = PCdRamo;
          WNuapol  = PNuApol;
          ECR_RAMO = WCdRamo;
          ECR_APOLICE = WNuApol;
       ON-ERROR;
          RETURN;
       ENDMON;

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          RETURN;
       ENDIF;

       EXSR ResetVars;
       WRITE     RPRDE00400;
       EXSR GetVencimento;
       EXSR FillHistorico;
       EXSR inzRecords;
       EXSR getRecords;
       DOW (NOT *IN03 AND NOT *IN12);
          WNomLib     = *BLANKS;
          WNomFile    = *BLANKS;
          WNoFunction = 'RPRDE00403'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_DS#KEY01R = ECR_DS#KEY01;
          ECR_DS#KEY02R = ECR_DS#KEY02;
          WRITE     RPRDE00497; // Limpar o Ecra
          WRITE     RPRDE00400;
          WRITE     RPRDE00499;
          WRITE     RPRDE00498;
          EXFMT RPRDE00403;
          IF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             EXSR inzRecords;
             EXSR getRecords;
          ENDIF;
       ENDDO;
       RETURN;

       //========================================================
       // Subrotina.: ResetVars
       // Objectivo.: Limpar Campos de Trabalho
       //========================================================
       BEGSR ResetVars;
           Lst#Hist(1).IsEmpty = *ON;
          *IN12 = *OFF;
          *IN03 = *OFF;
       ENDSR;

       //========================================================
       // Subrotina.: GetVencimento
       // Objectivo.: Obter datas de Vencimento
       //========================================================
       BEGSR getVencimento;
           IF ((WCdRamo = 50) OR (WCdRamo = 51));
              CHAIN (WCdRamo:WNuApol) GBASEGRA;
               IF (%FOUND());
                  ECR_NOMSEG = AUT_SEG;
                  WMesIni    = %DEC(%SUBST(%EDITW(AUT_DTINI:'0      '):
                               4:2):2:0);
               ELSE;
                  RETURN;
               ENDIF;
           ELSE;
              CHAIN (WCdRamo:WNuApol) GBASEGRO;
               IF (%FOUND());
                  ECR_NOMSEG = OR_SEG;
                  WMesIni    = %DEC(%SUBST(%EDITW(OR_DTINI:'0      '):
                               4:2):2:0);
               ELSE;
                  RETURN;
               ENDIF;
           ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: FillHistorico
       // Objectivo.: Carregar lista com ficheiros de Histórico
       //========================================================
       BEGSR FillHistorico;
          WLstParms(1).ValField = %TRIM(%EDITW(PNuApol:'0       '));
          WLstParms(1).IsEmpty = *OFF;
          WLstParms(2).ValField = %TRIM(%EDITW(PCDRamo:'0  '));
          WLstParms(2).IsEmpty = *OFF;
          WLstParms(3).IsEmpty = *ON;
          WLstFiles = $GetTmpFiles(WLstParms);
          WIdx = 1;
          WIdx2 = 1;
          DOW (NOT WLstFiles(WIdx).ISEmpty);
             Lst#Hist(WIdx2).Origem = 'X';
             IF ((WLstFiles(WIdx).CodFile >= 21) AND
                 (WLstFiles(WIdx).CodFile <= 23));
                Lst#Hist(WIdx2).Origem = 'D';
             ELSEIF ((WLstFiles(WIdx).CodFile >= 5) AND
                 (WLstFiles(WIdx).CodFile <= 8));
                WMesFich = %DEC(%SUBST(%CHAR(WlstFiles(WIdx).DatCria):5:2):2:0);
                IF (WMesFich = WMesIni);
                   Lst#Hist(WIdx2).Origem = 'C';
                ENDIF;
             ENDIF;

             IF ((Lst#Hist(WIdx2).Origem = 'C') OR
                (Lst#Hist(WIdx2).Origem = 'D'));
                Lst#Hist(WIdx2).File  = WLstFiles(WIdx).NomFile;
                Lst#Hist(WIdx2).Lib   = WLstFiles(WIdx).NomLibl;
                Lst#Hist(WIdx2).Date  = WlstFiles(WIdx).DatCria;
                Lst#Hist(WIdx2).IsEmpty= *OFF;
          // Lst#Hist(WIdx2).CdMot = WLstFiles(WIdx).
          // Lst#Hist(WIdx2).      = WLstFiles(WIdx).CodFile;
          // Lst#Hist(WIdx2).      = WLstFiles(WIdx).AnoMes;
          // Lst#Hist(WIdx2).      = WLstFiles(WIdx).CodLetra;
          // Lst#Hist(WIdx2).      = WlstFiles(WIdx).HorCria;
             WIdx2 += 1;
             Lst#Hist(WIdx2).IsEmpty= *ON;
             ENDIF;
             WIdx += 1;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE00403;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
          *In54     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          REC# = *ZEROS;
          WIdx = 1;
          DOW (NOT Lst#Hist(WIdx).ISEmpty);
   77        ECR_FICHEIRO   = Lst#Hist(Widx).File;
   77        ECR_BIBLIOTECA = Lst#Hist(Widx).Lib;
   77        ECR_CDMOTIVO   = Lst#Hist(Widx).CdMot;
   77        ECR_OPTION     = *BLANKS;

             //--> Determinar o valor para o mês
             IF (Lst#Hist(Widx).Origem = 'D');
                WMesFich    = %DEC(%SUBST(Lst#Hist(Widx).File:6:2):2:0);
   77           ECR_CARREGA = %SUBST(Lst#Hist(Widx).File:8:1);
   78           ECR_MOTIVO  = $getDescricao(27:%CHAR(Lst#Hist(Widx).CdMot));
             ELSE;
                WMesFich    = %DEC(%SUBST(Lst#Hist(Widx).File:7:2):2:0);
   77           ECR_CARREGA = 'Cont.';
   78           ECR_MOTIVO  = 'Backup antes da Anuidade da Apólice';
             ENDIF;

             //--> ForCompr o texto para o mês
             IF (WMesFich = 1);
   78           ECR_MES = 'Janeiro';
             ELSEIF (WMesFich = 2);
   78           ECR_MES = 'Fevereiro';
             ELSEIF (WMesFich = 3);
   78           ECR_MES = 'Março';
             ELSEIF (WMesFich = 4);
   78           ECR_MES = 'Abril';
             ELSEIF (WMesFich = 5);
   78           ECR_MES = 'Maio';
             ELSEIF (WMesFich = 6);
   78           ECR_MES = 'Junho';
             ELSEIF (WMesFich = 7);
   78           ECR_MES = 'Julho';
             ELSEIF (WMesFich = 8);
   78           ECR_MES = 'Agosto';
             ELSEIF (WMesFich = 9);
   78           ECR_MES = 'Setembro';
             ELSEIF (WMesFich = 10);
   78           ECR_MES = 'Outubro';
             ELSEIF (WMesFich = 11);
   78           ECR_MES = 'Novembro';
             ELSEIF (WMesFich = 12);
   78           ECR_MES = 'Dezembro';
             ENDIF;

   78        ECR_DATA       = Lst#Hist(Widx).Date;
             REC# += 1;
             WRITE RPRDE00402;
             Widx += 1;
          ENDDO;

          //---------------------------------------
          // Validar se ainda existem mais registos
          //---------------------------------------
          IF (REC# = *ZEROS);
             *IN52 = *OFF;
          ELSE;
             IF (NOT $IsLastPage());
                *IN54    = *OFF;
             ELSE;
                *IN54    = *ON;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          IF (REC# = *ZEROS);
             LEAVESR;
          ENDIF;
          WSelected = *OFF;
          READC RPRDE00402;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                WSelected = *ON;
                WNomLib   = ECR_BIBLIOTECA;
                WNomFile  = ECR_FICHEIRO;
                EXSR LoadApolice;
             ENDIF;
             READC RPRDE00402;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: LoadApolice
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR LoadApolice;
          ECR_OPTION = '2';
          WRamoApolice = %TRIM(%EDITW(ECR_RAMO:'0  ')) +
                         %CHAR(ECR_APOLICE);
          IF (WNomFile <> *BLANKS);
             IF (ECR_RAMO = 50) OR (ECR_RAMO = 51);
               WStdCmd  = 'OVRDBF FILE(GBASEDIA) TOFILE('+
                           %TRIM(WNomLib)+'/'+%TRIM(WNomFile)+')';
   29          Shell(%TRIM(WStdCmd):%LEN(%TRIM(WStdCmd)));
               AppAuto(%TRIM(ECR_OPTION):WRamoApolice:ECR_DATA);
               WStdCmd = 'DLTOVR FILE(GBASEDIA)';
   29          Shell(%TRIM(WStdCmd):%LEN(%TRIM(WStdCmd)));
             ELSE;
               WStdCmd = 'OVRDBF FILE(GBASEDIO) TOFILE('+
                           %TRIM(WNomLib)+'/'+%TRIM(WNomFile)+')';
   29          Shell(%TRIM(WStdCmd):%LEN(%TRIM(WStdCmd)));
               AppOutros(%TRIM(ECR_OPTION):WRamoApolice:ECR_DATA);
               WStdCmd = 'DLTOVR FILE(GBASEDIO)';
   29          Shell(%TRIM(WStdCmd):%LEN(%TRIM(WStdCmd)));
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
