     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO017
     H* Descrição.: Actualiza Relação Ficheiro de Histórico/Apólices/Recibos
     H* Observação: Esta rotina varre todos os ficheiros de Histórico, e
     H*             por cada um extrai metadados que actualzia numa tabela
     H*             de relação.
     H*==================================================================
     FQAFDBASI  IF   E             DISK    USROPN EXTFILE('QTEMP/QAFDBASI')
     FGBASEGRA  IF   E           K DISK    PREFIX(AUT_)
     FGBASEGRO  IF   E           K DISK    PREFIX(OR_)
     FTMPFILE   IF   F  700     9AIDISK    KEYLOC(1) USROPN
     FSPRDE004  CF   E             WORKSTN SFILE(RPRDE00402:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      45      Condicionar Campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*================================================================
     D*--> Invocar um comando de AS/400 externo: -----------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*--> Formato de Registo dos Diários
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(HO_)
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
     D   Next                         5S 0 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D WFirst          S              5S 0 INZ(*ZEROS)
     D WLastRec        S              5S 0 INZ(*ZEROS)
     D WNextRec        S              5S 0 INZ(*ZEROS)
     D WHighDate       S              8S 0 INZ(*ZEROS)
     D WIdx            S              5S 0 INZ(*ZEROS)
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
     D WStdCmd         S            200A
     D WMesIni         S              2S 0
     D WStdLen         S             15P 5
     D PCdRamo         S              2S 0
     D PNuApol         S              7S 0
     I*=====================================================================
     IDIARIO    AA  11
     I                                  1  700  RBASEDIO
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
          WIdx = 1;
          DOW (WIdx < 50);
             Lst#Hist(WIdx).IsEmpty = *ON;
             Lst#Hist(WIdx).Date    = *ZEROS;
             Lst#Hist(WIdx).File    = *BLANKS;
             Lst#Hist(WIdx).Lib     = *BLANKS;
             Lst#Hist(WIdx).CdMot   = *ZEROS;
             Lst#Hist(WIdx).Origem  = *BLANKS;
             Lst#Hist(WIdx).Next    = *ZEROS;
             WIdx += 1;
          ENDDO;

          WFirst    = *ZEROS;
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
           WIdx = *ZEROS;
           IF ((WCdRamo = 50) OR (WCdRamo = 51));
              WPrefix = 'GBDIA';
           ELSE;
              WPrefix = 'GBDIO';
           ENDIF;

           //-----------------------------
           // Registos Correntes
           //-----------------------------
           WNomLib = 'QS36F';
           WIcCarrega = *ON;
           EXSR ListFiles;
           WIcCarrega = *OFF;
           EXSR ListFiles;

           //-----------------------------
           // Registos do Ano 2013
           //-----------------------------
           WNomLib = 'GUIAS2013';
           WIcCarrega = *ON;
           EXSR ListFiles;
           WIcCarrega = *OFF;
           EXSR ListFiles;

           //-----------------------------
           // Registos do Ano 2012
           //-----------------------------
           WNomLib = 'GUIAS2012';
           WIcCarrega = *ON;
           EXSR ListFiles;
           WIcCarrega = *OFF;
           EXSR ListFiles;

           //-----------------------------
           // Registos do Ano 2011
           //-----------------------------
           WNomLib = 'GUIAS2011';
           WIcCarrega = *ON;
           EXSR ListFiles;
           WIcCarrega = *OFF;
           EXSR ListFiles;

           //-----------------------------
           // Registos do Ano 2010
           //-----------------------------
           WNomLib = 'GUIAS2010';
           WIcCarrega = *ON;
           EXSR ListFiles;
           WIcCarrega = *OFF;
           EXSR ListFiles;

           //-----------------------------
           // Registos do Ano 2009
           //-----------------------------
           WNomLib = 'GUIAS2009';
           WIcCarrega = *ON;
           EXSR ListFiles;
           WIcCarrega = *OFF;
           EXSR ListFiles;

           //-----------------------------
           // Ordenar o Array
           //-----------------------------
           ExSr SortLista;
       ENDSR;

        //=================================================================
        // Subrotina..: sortLista
        // Objectivo..: Ordenar a lista Interna por data de Alteração
        //              decrescente.
        //=================================================================
        BEGSR sortLista;
           WFirst    = *ZEROS;
           WLastRec  = *ZEROS;
           DOU (WNextRec = *ZEROS);
              WHighDate = *ZEROS;
              WNextRec = *ZEROS;
              WIdx = 1;
              DOW (Lst#Hist(Widx).IsEmpty = *OFF);
                 IF ((Lst#Hist(Widx).Date >= WHighDate) AND (WIdx<>WLastRec)
                    AND (Lst#Hist(Widx).Next = *ZEROS));
                    WHighDate  = Lst#Hist(Widx).Date;
                    WNextRec = WIdx;
                 ENDIF;
                 WIdx += 1;
              ENDDO;
              IF (WFirst = *ZEROS);
                 WFirst  = WNextRec;
              ELSEIF (WNextRec<>WLastRec);
                 Lst#Hist(WLastRec).Next = WNextRec;
              ENDIF;
              WLastRec = WNextRec;
           ENDDO;
        ENDSR;

        //=================================================================
        // Subrotina.: ListFiles
        // Objectivo.: Listar os ficheiros existentes numa dada Biblioteca
        //=================================================================
        BEGSR ListFiles;
           MONITOR;
              IF ((WCdRamo = 50) OR (WCdRamo = 51));
                 IF (WIcCarrega);
                    WStdCmd = 'DSPFD FILE('+%TRIM(WNomLib)+'/GBDIA*) +
                               TYPE(*BASATR) +
                               OUTPUT(*OUTFILE) OUTFILE(QTEMP/QAFDBASI)';
                 ELSE;
                    WStdCmd = 'DSPFD FILE('+%TRIM(WNomLib)+'/BGRA*) +
                               TYPE(*BASATR) +
                               OUTPUT(*OUTFILE) OUTFILE(QTEMP/QAFDBASI)';
                 ENDIF;
              ELSE;
                 IF (WIcCarrega);
                    WStdCmd = 'DSPFD FILE('+%TRIM(WNomLib)+'/GBDIO*) +
                              TYPE(*BASATR) +
                              OUTPUT(*OUTFILE) OUTFILE(QTEMP/QAFDBASI)';
                 ELSE;
                    WStdCmd = 'DSPFD FILE('+%TRIM(WNomLib)+'/BGRO*) +
                              TYPE(*BASATR) +
                              OUTPUT(*OUTFILE) OUTFILE(QTEMP/QAFDBASI)';
                 ENDIF;
              ENDIF;
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
              OPEN QAFDBASI;
              READ QAFDBASI;
              DOW (NOT %EOF());
                MONITOR;
                   WMesFich = %DEC(%SUBST(ATFILE:7:2):2:0);
                ON-ERROR;
                ENDMON;
                IF (WIcCarrega) OR (NOT WIcCarrega AND WMesFich = WMesIni);
                   WNomLib  = ATLIB;
                   WNomFile = ATFILE;
                   EXSR OpnFile;
                ENDIF;
                READ QAFDBASI;
              ENDDO;
              CLOSE QAFDBASI;
           ON-ERROR;
           ENDMON;
        ENDSR;

        //=================================================================
        // Subrotina..: opnFile
        // Objectivo..: Abrir o ficheiro de Histórico
        //=================================================================
        BEGSR opnFile;
           MONITOR;
              WStdCmd = 'OVRDBF FILE(DIARIO) +
                         TOFILE('+%TRIM(WNomLib)+
                         '/' + %TRIM(WNomFile) + ') LVLCHK(*NO)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
              WRamoApolice = %TRIM(%EDITW(ECR_RAMO:'0  ')) +
                             %CHAR(ECR_APOLICE);
              OPEN DIARIO;
              CHAIN WRamoApolice DIARIO;
              IF (%FOUND);
                 WIdx += 1;
                 Lst#Hist(Widx).File = WNomFile;
                 Lst#Hist(Widx).Lib     = WNomLib;
                 Lst#Hist(Widx).Date    = %DEC(ATFCDT:6:0)+20000000;
                 Lst#Hist(Widx).CdMot   = HO_MALT; // Obter o Motivo
                 IF (WIcCarrega);
                    Lst#Hist(Widx).Origem  = 'D'; // Carregamentos
                 ELSE;
                    Lst#Hist(Widx).Origem  = 'C'; // Continuados
                 ENDIF;
                 Lst#Hist(Widx).IsEmpty = *OFF;
              ENDIF;
              CLOSE DIARIO;
           ON-ERROR;
           ENDMON;
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
          WIdx = WFirst;
          REC# = *ZEROS;
          DOW (WIdx > *ZEROS);
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

             //--> Formatar o texto para o mês
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
             WIdx = Lst#Hist(WIdx).Next;
             REC# += 1;
             WRITE RPRDE00402;
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
