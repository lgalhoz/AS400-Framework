     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO102
     H* Descrição.: Este programa possibilita a gestão de textos ou Notas
     H*             Descritivas a diferentes níveis (n).
     H*
     H*==================================================================
     FFCATP105  IF   E           K DISK    PREFIX(CAT_)
     FFCATP106  IF   E           K DISK    PREFIX(CAT_)
     FFCATP10601IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP106:RCATP10601)
     FFPRDP010  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP012  UF A E           K DISK    PREFIX(PRD_)COMMIT
     FFPRDP01001IF   E           K DISK    PREFIX(PRD_)
     F                                     RENAME(RPRDP010:RPRDP01001)
     FFPRDP01002IF   E           K DISK    PREFIX(PRD_)
     F                                     RENAME(RPRDP010:RPRDP01002)
     FFPRDE102  CF   E             WORKSTN SFILE(RPRDE10202:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      03      Tecla F3
     F*      06      Tecla F6
     F*      12      Tecla F12
     F*      45      IO Condicional - Capital
     F*      46      IO Condicional - Texto
     F*      50      Tecla PagDn
     F*      51      Tecla PagUp
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SDBSO010
     D
     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Gerir Manutenção de textos (livres, Flexiveis) e conteudos
     D MntTextos       PR             5S 0
     D  WPProd                        5S 0 CONST
     D  WPRamo                        2S 0 CONST
     D  WPApolice                     7S 0 CONST
     D  WPCdSuper                     5S 0 CONST
     D
     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   5S 0 CONST
     D
     D*==> Completar Arvore se não existir
     D CheckPai        PR             5S 0
     D  CdProd                        5S 0 Const
     D  CdRamo                        2S 0 CONST
     D  NuApolice                     7S 0 CONST
     D  CdSuper                       5S 0 CONST
     D
     D*==> Actualizar Capitais ou Eliminar Itens Superiores
     D UpdatePai       PR            11S 2
     D  CdRamo                        2S 0 CONST
     D  NuApolice                     7S 0 CONST
     D  CdSuper                       5S 0 CONST
     D*==> Actualizar o Capital de Risco
     D UpdCapRisco     PR
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D
     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D CallOption      PR                  EXTPGM(DynamicCall)
     D   WCodProduto                  5S 0 CONST
     D   Superior                     5S 0 CONST
     D   PtrOR                         *   CONST
     D   WDatInicio                   8S 0 CONST
     D   ModeIO                        N   CONST
     D
     D*==> Lista de Riscos
     D LST#Risco       DS                  QUALIFIED DIM(10)
     D   CapRisco                    11S 2 INZ(*ZEROS)
     D   CDRISCO                      5S 0 INZ(*ZEROS)
     D   ISEMPTY                       N   INZ(*ON)
     D
     D*==> Parametros recebidos
     D WCodProduto     S              5S 0
     D
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WIdx            S              5S 0
     D WIcExtract      S               N
     D WIcDisplay      S               N
     D WModo           S               N
     D WExceptKeys     S             24A   INZ(*BLANKS)
     D DynamicCall     S             10A   INZ(*BLANKS)
     D WMoreText       S              4A   INZ(*BLANKS)
     D WDatInicio      S              8S 0
     D WptrOr          S               *
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrOR
     C                   PARM                    WModo
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       ptrOr       = WptrOR;
       ECR_NOMSEG  = OR_SEG;
       ECR_RAMO    = OR_RAMO;
       ECR_APOLICE = OR_APOL;
       WIcDisplay = WModo;
       MntTextos(WCodProduto:OR_RAMO:OR_APOL:*ZEROS);

       *INLR = *ON;
       RETURN;

      /END-FREE
     P*=======================================================
     P* Processo..: MntTextos
     P* Objectivo.: Realizar a Manutenção dos Textos
     P*=======================================================
     P MntTextos       B                   EXPORT
     D MntTextos       PI             5S 0
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D    PRM_CdSuper                 5S 0 CONST
     D
     D    WResult      S              5S 0
     D    WIcLoad      S               N   INZ(*OFF)
     D    WSelRec      S              4S 0
     D    WIcDetail    S              1A
     D    WCdTexto     S              5S 0
     D    WDsTexto     S             50A
     D    WNuSqSup     S              5S 0
     D    WNuSequ      S              5S 0
     D    WIcChanged   S              1A
     D    WNoFunction  S             10A
     D    WNoApp       S             10A
     D    WSessionID   S              5S 0 INZ(*ZEROS)
     D    WCdRisco     S              5S 0 INZ(*ZEROS)
     D*=================================================================
      /FREE
       WIcLoad  = *ON;
       DOW (NOT *IN03 AND NOT *IN12);
          EXSR ShowCapRisco;
          //-----------------------------------------------
          // Obter registos em função do Nível
          //-----------------------------------------------
          IF (WIcLoad);
             EXSR InzRecTxt; // Limpar a Lista de Ecrã
             EXSR  GetRecTexto; // Carregar Lista com Registos
             WIcLoad = *OFF;
          ENDIF;

          //-----------------------------------------------
          // Condicionar apresentação em função do Nível
          //-----------------------------------------------
          IF (PRM_CdSuper = *ZEROS);
             ECR_DSHDR = 'Notas descritivas';
             IF (WIcDisplay);
                WNoFunction = 'RPRDE10203';
                WExceptKeys = *BLANKS;
             ELSE;
                WNoFunction = 'RPRDE10221';
                WExceptKeys = *BLANKS;
             ENDIF;
          ELSE;
             CHAIN (PRM_CdProd:PRM_CdSuper) RCATP106;
             IF (%FOUND());
                ECR_DSHDR = CAT_DSTEXTO;
                WNoFunction = 'RPRDE10208'; // Manutenção de Itens
                IF ((CAT_ICRESERV = 'S') OR (WIcDisplay));
                   WExceptKeys = '000001'; // F06 Off
                ELSE;
                   WExceptKeys = *BLANKS;
                ENDIF;
             ENDIF;
          ENDIF;
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:WExceptKeys);
          ECR_S#PGM = S#PGM;
          WRITE     RPRDE10200;
          WRITE     RPRDE10299;
          WRITE     RPRDE10298;
          EXFMT RPRDE10203;

          IF ($ValidKey(Func#Key:KEY#F21));
             //-------------------------------------------------
             // Mudar Modo IO, Para alteração
             //-------------------------------------------------
             //IF (PRM_CdSuper = *ZEROS);
             //   WIcDisplay = *OFF;
             //   WIcLoad = *ON;
             //ENDIF;
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             //-------------------------------------------------
             // Adicionar mais linhas se estiver em modo de Alteração
             //-------------------------------------------------
             IF ((ECR_ICRESERV <> 'S') AND (PRM_CdSuper > *ZEROS)
                AND (NOT WIcDisplay));
                REC#        += 1;
                *IN46        = *ON;
                *IN45        = *OFF;
                *IN52        = *ON;
                ECR_ICDETAIL  = 'N';
                ECR_NUSEQU   = *ZEROS;
                ECR_CPSEGURO = *ZEROS;
                ECR_DSTEXTO  = *BLANKS;
                ECR_CDTEXTO  = *ZEROS;
                ECR_DSTEXTOBK= ECR_DSTEXTO;
                WRITE RPRDE10202;
             ENDIF;
          ELSEIF (Func#Key = KEY#F14);
             MONITOR;
                PrintPreview(WPtrOr:23);
             ON-ERROR;
             ENDMON;
          ELSEIF (Func#Key = KEY#Enter);
             //-----------------------------------------------
             // Validar se foi Seleccionado algum registo
             //-----------------------------------------------
             EXSR SelRec;
          ELSEIF (Func#Key = KEY#F12);
             IF (PRM_CdSuper > *ZEROS);
                *IN12 = *OFF;
                RETURN WResult;
             ENDIF;
          ENDIF;
       ENDDO;
       RETURN WResult;

       //============================================================
       // Subrotina..: ShowCapRisco
       // Objectivo..: repor Capitais de Risco
       //============================================================
       BEGSR ShowCapRisco;
          WIdx = *ZEROS;
          ECR_DSRISC001 = *BLANKS;
          ECR_CPRISC001 = *ZEROS;
          IF (OR_CR1 > *ZEROS);
             ECR_DSRISC001 = $getDescRisco(PRM_CdProd:OR_CR1);
             ECR_CPRISC001 = OR_CAP1;
             WIdx += 1;
             LST#Risco(WIdx).CdRisco = OR_CR1;
             LST#Risco(WIdx).IsEmpty = *OFF;
          ENDIF;

          ECR_DSRISC002 = *BLANKS;
          ECR_CPRISC002 = *ZEROS;
          IF (OR_CR2 > *ZEROS);
             ECR_DSRISC002 = $getDescRisco(PRM_CdProd:OR_CR2);
             ECR_CPRISC002 = OR_CAP2;
             WIdx += 1;
             LST#Risco(WIdx).CdRisco = OR_CR2;
             LST#Risco(WIdx).IsEmpty = *OFF;
          ENDIF;

          ECR_DSRISC003 = *BLANKS;
          ECR_CPRISC003 = *ZEROS;
          IF (OR_CR3 > *ZEROS);
             ECR_DSRISC003 = $getDescRisco(PRM_CdProd:OR_CR3);
             ECR_CPRISC003 = OR_CAP3;
             WIdx += 1;
             LST#Risco(WIdx).CdRisco = OR_CR3;
             LST#Risco(WIdx).IsEmpty = *OFF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecTxt;
          WCdTexto  = *ZEROS;   // Código do Texto
          WNuSqSup  = *ZEROS;   // Elemento Superior
          *IN46     = *OFF;
          REC#      = 0;        // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE10203; // Limpar Lista
          *In53     = *Off;     // Repor Indicador
          *In54     = *ON;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecTexto
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecTexto;
          WMoreText = '...';
          WNoApp      = *BLANKS;
          WSessionID = $AddDBSession();
          IF (PRM_CdSuper = *ZEROS);
             $SETFILES('FCATP106 A INNER JOIN +
                        FCATP105 B ON (A.CDPROD = B.CDPROD AND A.CDTEXTO +
                        = B.CDTEXTO)');
             $SETFIELDS('A.CDPROD, B.CDRISCO, A.CDTEXTO, A.DSTEXTO, +
                         A.CDSUPER, ICRESERV, ICDETAIL, ICOBRIGA');
             $SetSort('CDRISCO');
             $SetCriteria('A.CDPROD = ''' + %CHAR(PRM_CdProd) + '''');
          ELSE;
             $SETFILES('FCATP106 A');
             $SETFIELDS('A.CDPROD, A.CDTEXTO, A.DSTEXTO, A.CDSUPER, +
                         ICRESERV, ICDETAIL, ICOBRIGA');
             $SetSort('CDTEXTO');
             $SetCriteria('A.CDPROD = ''' + %CHAR(PRM_CdProd) + ''' +
                        AND A.CDSUPER = ''' + %CHAR(PRM_CdSuper) +
                       ''' AND A.CDSUPER <> A.CDTEXTO ');
          ENDIF;
          $SetPage(25);
          Func#Key = Key#Enter;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //----------------------------------------------
                // Validar se o texto está associado ao Risco
                //----------------------------------------------
                WIcExtract = *ON;
                IF (PRM_CDSUPER = *ZEROS);
                   WIcExtract = *OFF;
                   WIdx = 1;
                   MONITOR;
                   DOW (NOT LST#Risco(Widx).IsEmpty);
                      IF (%DEC($GetField('B.CDRISCO'):5:0)=
                                        Lst#Risco(WIdx).CdRisco);
                         WIcExtract = *ON;
                         LEAVE;
                      ENDIF;
                      WIdx += 1;
                   ENDDO;
                   ON-ERROR;
                      WIcExtract = *OFF;
                   ENDMON;
                ENDIF;

                //----------------------------------------------
                // Texto Relacionado com Risco
                //----------------------------------------------
                IF (WIcExtract);
                   *IN45 = WIcDisplay;     // Não pode alterar a descrição
                   ECR_NUSEQU    = *ZEROS;
                   ECR_CPSEGURO  = *ZEROS;
                   ECR_CPSEGUROBK= *ZEROS;
                   ECR_NUSQSUP   = *ZEROS;
                   ECR_ICRESERV  = 'N';     // Pode adicionar linhas
                   ECR_ICPROTEC  = 'N';     // Pode alterar o Capital Seguro
                   ECR_NUSQSUP   = *ZEROS; // Não existe nível Superior
                   ECR_DSTEXTO =  $GetField('A.DSTEXTO');
                   WCdTexto = %DEC($GetField('A.CDTEXTO'):5:0);
                   CHAIN(N) (PRM_CdRamo:PRM_APOLICE:WCdTexto) RPRDP01001;
                   IF (%FOUND());
                      ECR_CPSEGURO  = PRD_CPSEGURO; // Capital Seguro
                      ECR_CPSEGUROBK= PRD_CPSEGURO; // Backup do Cap.Seguro
                      ECR_NUSQSUP   = PRD_NUSQSUP;  // Sequencia Superior
                      ECR_NUSEQU    = PRD_NUSEQU;   // Numero de Sequencia

                      SETLL (PRM_CdRamo:PRM_APOLICE:PRD_NUSEQU) RPRDP01002;
                      READE (PRM_CdRamo:PRM_APOLICE:PRD_NUSEQU) RPRDP01002;
                      DOW (NOT %EOF());
                         IF (PRD_NUSEQU  <> PRD_NUSQSUP);
                            ECR_DSTEXTO = %TRIM(WMoreText) + ECR_DSTEXTO;
                            ECR_ICPROTEC = 'S';
                            *IN45 = *ON;
                            LEAVE;
                         ENDIF;
                         READE (PRM_CdRamo:PRM_APOLICE:PRD_NUSEQU) RPRDP01002;
                      ENDDO;
                   ENDIF;
                   REC# += 1;
                   ECR_CDTEXTO   = WCdTexto;
                   ECR_CDPROD    = PRM_CDPROD;
                   ECR_ICOBRIGA  = $GetField('ICOBRIGA');
                   ECR_ICRESERV  = $GetField('ICRESERV');
                   ECR_ICDETAIL  = $GetField('ICDETAIL');
                   ECR_DSTEXTOBK = ECR_DSTEXTO;
                   WRITE RPRDE10202;
                ENDIF;
                $getNext();
             ENDDO;
          ENDIF;

          EXSR GetFreeTexto;
          IF (REC# = *ZEROS);
             *IN52 = *OFF;
          ENDIF;
          $RmvDBSession(WSessionID);
       ENDSR;

       //========================================================
       // Subrotina.: GetFreeTexto
       // Objectivo.: Obter texto Livre
       //========================================================
       BEGSR getFreeTexto;
          WNoApp     = *BLANKS;
          WIcExtract = *OFF;
          *IN46        = NOT WIcDisplay;
          *IN45        = WIcDisplay;
          IF (PRM_CdSuper <> *ZEROS);
             CHAIN (PRM_CdRamo:PRM_Apolice:PRM_CdSuper) RPRDP01001;
             IF (%FOUND());
                WNuSqSup = PRD_NUSEQU;
                CHAIN (PRM_CdProd:PRM_CdSuper) RCATP106;
                IF (%FOUND());
                   WNoApp = $GetFunction(CAT_CDFUNCT);
                ENDIF;
                WIcExtract = *ON;
             ENDIF;
          ENDIF;

          IF (WIcExtract);
             SETLL (PRM_CdRamo:PRM_APOLICE:WNuSqSup) RPRDP01002;
             READE (PRM_CdRamo:PRM_APOLICE:WNuSqSup) RPRDP01002;
             DOW (NOT %EOF());
                IF (PRD_CDTEXTO = *ZEROS);
                   ECR_DSTEXTO = '<NÃO ESPECIFICADO>';
                   CHAIN(N) (PRM_CdRamo:PRM_Apolice:PRD_NuSeQu) RPRDP012;
                   IF (%FOUND());
                      ECR_DSTEXTO = PRD_DSDETAIL;
                      IF (PRD_ICDETAIL = 'S');
                         ECR_DSTEXTO = %TRIM(WMoreText) + ECR_DSTEXTO;
                         ECR_ICPROTEC = 'S';
                         *IN45 = *ON;
                         *IN46 = *OFF;
                      ELSE;
                         ECR_ICPROTEC = 'N';
                         *IN46 = NOT WIcDisplay;
                         *IN45 = WIcDisplay;
                      ENDIF;
                   ENDIF;

                   REC#         += 1;
                   ECR_CDTEXTO   = *ZEROS;
                   ECR_CPSEGURO  = PRD_CPSEGURO; // Capital Seguro
                   ECR_CPSEGUROBK= PRD_CPSEGURO; // Backup do Capital Seguro
                   ECR_NUSQSUP   = PRD_NUSQSUP;  // Sequencia Superior
                   ECR_NUSEQU    = PRD_NUSEQU;   // Numero de Sequencia
                   ECR_ICDETAIL  = 'N';
                   ECR_ICRESERV  = 'N';          // Pode adicionar linhas
                   ECR_ICPROTEC  = 'N';
                   ECR_NUSQSUP   = WNuSqSup;     // Não existe nível Superio
                   ECR_DSTEXTOBK = ECR_DSTEXTO;
                   WRITE RPRDE10202;
                ENDIF;
                READE (PRM_CdRamo:PRM_APOLICE:WNuSqSup) RPRDP01002;
             ENDDO;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR selRec;
          IF (REC# = *ZEROS);
             LEAVESR;
          ENDIF;
          WSelRec    = *ZEROS;
          WIcChanged = 'N';
          READC RPRDE10202;
          DOW (NOT %EOF());

             //------------------------------------------------
             // Salvar o Registo
             //------------------------------------------------
             IF (ECR_ICPROTEC = 'N');
                IF (ECR_CPSEGURO <> ECR_CPSEGUROBK) OR
                    (ECR_DSTEXTO <> ECR_DSTEXTOBK);
                   EXSR ChgRecord;
                   WIcChanged = 'S';
                ENDIF;
             ENDIF;

             //------------------------------------------------
             // Actualizar Capitais
             //------------------------------------------------
             IF (WIcChanged = 'S');
                IF (PRM_CdSuper > *ZEROS);
                   UpdatePai(PRM_CdRamo:PRM_APOLICE:WNuSqSup);
                   COMMIT;
                ENDIF;
                UpdCapRisco(PRM_CdRamo:PRM_APOLICE);
             ENDIF;

             //------------------------------------------------
             // Produto parameterizado para abrir outro nível
             //------------------------------------------------
             IF (ECR_OPTION <> *BLANKS);
                WSelRec    = REC#;
                WIcDetail  = CAT_ICDetail;
                ECR_OPTION = *BLANKS;
             ENDIF;
             READC RPRDE10202;
          ENDDO;

          //----------------------------------
          // Actualizar nivel Superior
          //----------------------------------
          IF (WIcChanged = 'S');
          // IF (PRM_CdSuper > *ZEROS);
          //    UpdatePai(PRM_CdRamo:PRM_APOLICE:WNuSqSup);
          //    COMMIT;
          // ENDIF;
          // UpdCapRisco(PRM_CdRamo:PRM_APOLICE);
             WIcLoad = *ON;
          ENDIF;

          //----------------------------------
          // Invocar nivel Seguinte
          //----------------------------------
          IF (WSelRec > *ZEROS);
             IF (ECR_IcDetail = 'S');
                MntTextos(PRM_CdProd:PRM_CdRamo:PRM_APOLICE:ECR_CDTEXTO);
                WIcLoad = *ON;
             ELSE;
                DynamicCall = WNoApp;
                MONITOR;
                   IF (DynamicCall = *BLANKS);
                      DynamicCall = 'RPRDO104'; // função por Defeito
                   ENDIF;
                   CallOption(WCodProduto:ECR_NUSEQU:WPtrOR:WDatInicio:
                              WIcDisplay);
                   DynamicCall = *BLANKS;
                   UpdatePai(PRM_CdRamo:PRM_APOLICE:WNuSqSup);
                   UpdCapRisco(PRM_CdRamo:PRM_APOLICE);
                   COMMIT;
                ON-ERROR;
                   ROLBK;
                ENDMON;
                WIcLoad = *ON;
                *IN12 = *OFF;
             ENDIF;
          ENDIF;
       ENDSR;


       //========================================================
       // Subrotina.: ChgRecord
       // Objectivo.: Realizar a operação IO sobre o registo
       //========================================================
       BEGSR ChgRecord;
          IF (ECR_NUSEQU > *ZEROS);
             IF (ECR_CPSEGURO = *ZEROS);
                //---------------------
                // Eliminar o registo
                //---------------------
                MONITOR;
                   DELETE (PRM_CdRamo:PRM_APOLICE:ECR_NUSEQU) RPRDP010;
                   COMMIT;
                ON-ERROR;
                   ROLBK;
                ENDMON;
             ELSE;
                //---------------------
                // Alterar o Registo
                //---------------------
                CHAIN (PRM_CdRamo:PRM_APOLICE:ECR_NUSEQU) RPRDP010;
                PRD_CPSEGURO = ECR_CPSEGURO;
                MONITOR;
                   UPDATE RPRDP010 %FIELDS(PRD_CPSEGURO);
                   IF (ECR_CDTEXTO = *ZEROS);
                      CHAIN (PRM_CdRamo:PRM_APOLICE:ECR_NUSEQU) RPRDP012;
                      IF (%FOUND());
                         PRD_DSDETAIL = ECR_DSTEXTO;
                         UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL);
                      ENDIF;
                   ENDIF;
                   COMMIT;
                ON-ERROR;
                   ROLBK;
                ENDMON;
             ENDIF;
          ELSE;
             //---------------------
             // Adicionar o Registo
             //---------------------
             IF (ECR_CPSEGURO > *ZEROS);
                IF (WNuSqSup = *ZEROS) AND (PRM_CdSuper > *ZEROS);
                   WNuSqSup = CheckPai(PRM_CdProd:PRM_CdRamo:
                                       PRM_APOLICE:PRM_CDSUPER);
                ENDIF;
                WCdTexto    = ECR_CDTEXTO;
                EXSR addrecord;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: addRecord
       // Objectivo.: Adicionar um novo Registo
       //========================================================
       BEGSR addRecord;
          SETGT (PRM_CdRamo:PRM_APOLICE) RPRDP010;
          READP(N) RPRDP010;
          PRD_CDRAMO   = PRM_CdRamo;
          PRD_NUAPOL   = PRM_Apolice;
          PRD_NUSEQU   = PRD_NUSEQU + 1;
          PRD_CDTEXTO  = WCdTexto;
          IF (WNuSqSup = *ZEROS);
             WNuSqSup = PRD_NUSEQU;
          ENDIF;
          PRD_NUSQSUP  = WNuSqSup;
          PRD_CPSEGURO = ECR_CPSEGURO;
          MONITOR;
             WRITE RPRDP010;
             IF (WCdTexto = *ZEROS);
                PRD_CDRAMO   = PRM_CdRamo;
                PRD_NUAPOL   = PRM_Apolice;
                PRD_NUSEQU   = PRD_NUSEQU;
                PRD_ICDETAIL = 'N';
                PRD_DSDETAIL = ECR_DSTEXTO;
                MONITOR;
                   WRITE RPRDP012;
                   COMMIT;
                ON-ERROR;
                   ROLBK;
                ENDMON;
             ELSE;
                COMMIT;
             ENDIF;
          ON-ERROR;
             ROLBK;
          ENDMON;
       ENDSR;
      /END-FREE
     P MntTextos       E
     P*=======================================================
     P* Processo..: CheckPai
     C* Objectivo.: Criar os Itens relacionados com o Item.
     P*=======================================================
     P CheckPai        B                   EXPORT
     D CheckPai        PI             5S 0
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D    PRM_CDSUPER                 5S 0 CONST
     D WNuSqSup        S              5S 0
     D WCdSuper        S              5S 0
      /FREE
         WNuSqSup = *ZEROS;
         CHAIN (PRM_CdRamo:PRM_Apolice:PRM_CdSuper) RPRDP01001;
         IF (%FOUND());
            WNuSqSup = PRD_NUSEQU;
         ELSE;
            CHAIN (PRM_CdProd:PRM_CdSuper) RCATP106;
            IF (%FOUND());
               WCdSuper = CAT_CDSUPER;
               IF (CAT_CDSUPER <> CAT_CDTEXTO);
                  WNuSqSup = CheckPai(PRM_CdRamo:PRM_CdRamo:
                                      PRM_Apolice:WCdSuper);
               ELSE;
               ENDIF;

               SETGT (PRM_CdRamo:PRM_APOLICE) RPRDP010;
               READP(N) RPRDP010;
               PRD_CDRAMO   = PRM_CdRamo;
               PRD_NUAPOL   = PRM_Apolice;
               PRD_NUSEQU   = PRD_NUSEQU + 1;
               PRD_CDTEXTO  = PRM_CDSUPER;
               PRD_CPSEGURO = *ZEROS;
               IF (WNuSqSup = *ZEROS);
                  WNuSqSup = PRD_NUSEQU;
               ENDIF;
               PRD_NUSQSUP  = WNuSqSup;
               WRITE RPRDP010;
               WNuSqSup = PRD_NUSEQU;
            ENDIF;
         ENDIF;
         RETURN WNuSqSup;
      /END-FREE
     P CheckPai        E
     P*=======================================================
     P* Processo..: UpdatePai
     P* Objectivo.: Actualizar os Capitais ou apagar os Itens
     P*             Superiores.
     P*=======================================================
     P UpdatePai       B                   EXPORT
     D UpdatePai       PI            11S 2
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D    PRM_NuSqSup                 5S 0 CONST
     D WCapSeguro      S             11S 2 INZ(*ZEROS)
      /FREE
         WCapSeguro = *ZEROS;
         SETLL (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP01002;
         READE (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP01002;
         DOW (NOT %EOF());
            IF (PRD_NuSqSup <> PRD_NuSequ);
               WCapSeguro += PRD_CpSeguro;
            ENDIF;
            READE (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP01002;
         ENDDO;

         CHAIN(N) (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP010;
         IF (%FOUND());
            IF (WCapSeguro > *ZEROS);
               CHAIN (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP010;
               PRD_CpSeguro = WCapSeguro;
               UPDATE RPRDP010 %FIELDS(PRD_CPSEGURO);
            ELSE;
               DELETE (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP010;
            ENDIF;
            IF (PRD_NuSqSup <> PRM_NuSqSup);
               WCapSeguro = UpdatePai(PRM_CdRamo:PRM_Apolice:PRD_NuSqSup);
            ENDIF;
         ENDIF;

         RETURN WCapSeguro;
      /END-FREE
     P UpdatePai       E
     P*=======================================================
     P* Processo..: UpdCapRisco
     P* Objectivo.: Actualizar os Capitais ou apagar os Itens
     P*             Superiores.
     P*=======================================================
     P UpdCapRisco     B                   EXPORT
     D UpdCapRisco     PI
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D WCapSeguro      S             11S 2 INZ(*ZEROS)
     D WCdRisco        S              5S 0 INZ(*ZEROS)
      /FREE
         //-------------------------------------------------------
         // Posicionar no nível Superior
         //-------------------------------------------------------
         OR_CAP1 = *ZEROS;
         OR_CAP2 = *ZEROS;
         OR_CAP3 = *ZEROS;

         SETLL (PRM_CdRamo:PRM_Apolice) RPRDP01002;
         READE (PRM_CdRamo:PRM_Apolice) RPRDP01002;
         DOW (NOT %EOF());
            IF (PRD_NUSEQU = PRD_NUSQSUP);
               SETLL (WCodProduto) RCATP105;
               READE (WCodProduto) RCATP105;
               DOW (NOT %EOF());
                  IF (OR_CR1 = CAT_CDRISCO) AND (CAT_CDTEXTO = PRD_CDTEXTO);
                     OR_CAP1 += PRD_CpSeguro;
                  ELSEIF (OR_CR2=CAT_CDRISCO) AND (CAT_CDTEXTO=PRD_CDTEXTO);
                     OR_CAP2 += PRD_CpSeguro;
                  ELSEIF (OR_CR3=CAT_CDRISCO) AND (CAT_CDTEXTO=PRD_CDTEXTO);
                     OR_CAP3 += PRD_CpSeguro;
                  ENDIF;
                  READE (WCodProduto) RCATP105;
               ENDDO;
            ENDIF;
            READE (PRM_CdRamo:PRM_Apolice) RPRDP01002;
         ENDDO;
      /END-FREE
     P UpdCapRisco     E
