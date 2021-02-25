     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO004
     H* Descrição.: Disponibilizar Histórico de Alterações das Apólices
     H* Observação: Esta rotina lista todos os ficheiros de Histórico
     H*             onde consta a apólice, e devolve o nome do ficheiro
     H*             para que o utilizador possa consultar os dados.
     H*==================================================================
     FQAFDBASI  IF   E             DISK    USROPN EXTFILE('QTEMP/QAFDBASI')
     FGBASEGRA  IF   E           K DISK    PREFIX(AUT_)
     FGBASEGRO  IF   E           K DISK    PREFIX(OR_)
     FDIARIO    IF   F  700     9AIDISK    KEYLOC(1) USROPN
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
     D PLst#Hist       DS                  LIKEDS(Lst#Hist) DIM(50)
     I*=====================================================================
     IDIARIO    AA  11
     I                                  1  700  RBASEDIO
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCDRAMO
     C                   PARM                    PNUAPOL
     C                   PARM                    PLst#Hist
      /FREE

       MONITOR;
          WCdRamo  = PCdRamo;
          WNuapol  = PNuApol;
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
       EXSR GetVencimento;
       EXSR FillHistorico;
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
                  WMesIni    = %DEC(%SUBST(%EDITW(AUT_DTINI:'0      '):
                               4:2):2:0);
               ELSE;
                  RETURN;
               ENDIF;
           ELSE;
              CHAIN (WCdRamo:WNuApol) GBASEGRO;
               IF (%FOUND());
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
              WRamoApolice = %TRIM(%EDITW(WCdRamo:'0  ')) +
                             %CHAR(WNuApol);
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
      /END-FREE
