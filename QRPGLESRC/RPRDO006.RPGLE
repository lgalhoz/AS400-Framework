     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO006
     H* Descrição.: Listar histórico dos Documentos gerados no AS/400
     H* Observação: Esta rotina lista todos os ficheiros de Histórico
     H*             onde consta a apólice, e devolve o nome do ficheiro
     H*             para que o utilizador possa consultar os dados.
     H*==================================================================
     FGBASEGRA  IF   E           K DISK    PREFIX(AUT_)
     FGBASEGRO  IF   E           K DISK    PREFIX(OR_)
     FFDOCP060  IF   E             DISK
     FSPRDE006  CF   E             WORKSTN SFILE(RPRDE00602:REC#)
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
      /Copy QRPGLESRC,SGRLO015
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*================================================================
     D*--> Invocar um comando de AS/400 externo: -----------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*--> Formato de Registo dos Diários
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(HO_)
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
     D WRamoApolice    S              9S 0
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

       WRITE     RPRDE00600;
       EXSR GetDocumentos;
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          WNomLib     = *BLANKS;
          WNomFile    = *BLANKS;
          WNoFunction = 'RPRDE00603'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_DS#KEY01R = ECR_DS#KEY01;
          ECR_DS#KEY02R = ECR_DS#KEY02;
       // ECR_S#PGM    = S#PGM;
          WRITE     RPRDE00600;
          WRITE     RPRDE00699;
          WRITE     RPRDE00698;
          EXFMT RPRDE00603;
          IF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             RETURN;
          ENDIF;
       ENDDO;

       RETURN;

       //========================================================
       // Subrotina.: GetDocumentos
       // Objectivo.: Obter lista de Documentos onde conta a apólice
       //========================================================
       BEGSR GetDocumentos;
         $InzListHS();
         $FillListHS('QS36F':'GGUIA*');
         DOW (NOT $EofHS());
            Fil#Info = $getNextHS();
            IF ($FindRec(%TRIM(Fil#Info.Lib):%TRIM(Fil#Info.File):
                         ''+%TRIM(%EDITW(WCdRamo:'0  '))+
                           %CHAR(WNuApol)+''
                         :10));
               DSPLY %TRIM(Fil#Info.File);
            ENDIF;
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
          WRITE RPRDE00603;     // Limpar Lista
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
   77        // ECR_CARREGA = %SUBST(Lst#Hist(Widx).File:8:1);
   78           ECR_MOTIVO  = $getDescricao(27:%CHAR(Lst#Hist(Widx).CdMot));
             ELSE;
                WMesFich    = %DEC(%SUBST(Lst#Hist(Widx).File:7:2):2:0);
   77        // ECR_CARREGA = 'Cont.';
   78           ECR_MOTIVO  = 'Backup antes da Anuidade da Apólice';
             ENDIF;

   78        ECR_DATA       = Lst#Hist(Widx).Date;
             WIdx = Lst#Hist(WIdx).Next;
             REC# += 1;
             WRITE RPRDE00602;
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
          READC RPRDE00602;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                WSelected = *ON;
                WNomLib   = ECR_BIBLIOTECA;
                WNomFile  = ECR_FICHEIRO;
             ENDIF;
             READC RPRDE00602;
          ENDDO;
       ENDSR;
      /END-FREE
