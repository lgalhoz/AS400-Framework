     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO102
     H* Descrição.: Este programa possibilita a gestão de textos ou Notas
     H*             Descritivas a diferentes níveis (n).
     H*
     H*==================================================================
     FFPRDT001  IF   E           K DISK    PREFIX(PRD_)
     FFPRDP010  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP012  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP011  UF   E           K DISK    PREFIX(PRD_) COMMIT
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
      /Copy QRPGLESRC,SPRDO002
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
     D   Cdfunction                   2A   CONST
     D   Data                         8S 0 CONST
     D
     D*==> Riscos da Apólice
     D LST#Risco       DS                  QUALIFIED DIM(10)
     D   CodRisco                     5S 0 INZ(*ZEROS)
     D   CapRisco                    11S 2 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Completar Arvore se não existir
     D CheckPai        PR             5S 0
     D  CdProd                        5S 0 Const
     D  CdRamo                        2S 0 CONST
     D  NuApolice                     7S 0 CONST
     D  CdSuper                       5S 0 CONST
     D
     D*==> Eliminar Níveis Superiores
     D DelPai          PR              N
     D  CdRamo                        2S 0 CONST
     D  NuApolice                     7S 0 CONST
     D  CdSuper                       5S 0 CONST
     D
     D*==> Actualizar Capitais ou Eliminar Itens Superiores
     D UpdatePai       PR            11S 2
     D  CdRamo                        2S 0 CONST
     D  NuApolice                     7S 0 CONST
     D  CdSuper                       5S 0 CONST
     D
     D*==> Actualizar Capitais ou Eliminar Itens Superiores
     D GetCpSeguro     PR            11S 2
     D  CdRamo                        2S 0 CONST
     D  NuApolice                     7S 0 CONST
     D  CdSuper                       5S 0 CONST
     D
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
     D   DatInicio                    8S 0 CONST
     D   ModeIO                        N   CONST
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
     D WptrOr          S               *
     D WCdRisco        S              5S 0 INZ(*ZEROS)
     D WCapRisco       S             11S 2 INZ(*ZEROS)
     D WRecAct         S              3S 0 INZ(*ZEROS)
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
          $ShowDialog('SEC0001');
          RETURN;
       ENDIF;

       ptrOr       = WptrOR;
       ECR_NOMSEG  = OR_SEG;
       ECR_RAMO    = OR_RAMO;
       ECR_APOLICE = OR_APOL;

       //---------------------------------------------
       // Validar se os dados estão convertidos
       //---------------------------------------------
       CHAIN (OR_RAMO:OR_APOL) RPRDP001;
       IF (%FOUND(FPRDT001));
          IF (PRD_TPACTA = 'A');
             $ShowDialog('PRD0160');
             RETURN;
          ENDIF;
       ELSE;
          $ShowDialog('PRD0161');
          RETURN;
       ENDIF;

       WIcDisplay = WModo;
       MntTextos(WCodProduto:OR_RAMO:OR_APOL:*ZEROS);
       *IN12 = *OFF;
       *IN03 = *OFF;
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
     D
     D    WRRN         S              4S 0 INZ(*ZEROS)
     D    WRegAct      S              5S 0 INZ(*ZEROS)
     D    WSessCatNot  S              5S 0 INZ(*ZEROS)
     D    WSesCatNot2  S              5S 0 INZ(*ZEROS)
     D    WSesCatNot4  S              5S 0 INZ(*ZEROS)
     D    WSesCatNot5  S              5S 0 INZ(*ZEROS)
     D    WSessPrdNot  S              5S 0 INZ(*ZEROS)
     D    WSessPrdTxt  S              5S 0 INZ(*ZEROS)
     D    WCdRisco     S              5S 0 INZ(*ZEROS)
     D*=================================================================
      /FREE
       WIcLoad  = *ON;

       DOW (NOT *IN03 AND NOT *IN12);
          //-----------------------------------------------
          // Apresentar Informação Actualizada dos Capitais
          // de Risco.
          //-----------------------------------------------
          LST#Risco = $GetCapRisco(WCodProduto:ECR_RAMO:ECR_Apolice:*ZEROS:
                                   *OFF:WDatInicio);
          EXSR ShowCapRisco;

          //-----------------------------------------------
          // Obter registos em função do Nível
          //-----------------------------------------------
          IF (WIcLoad);
             //---------------------------------------------
             // Declaração de ficheiros de leitura
             //---------------------------------------------
             WSessPrdNot = $SetFiles('FPRDP010'); // Notas descritivas do Produto
             $AddDBDate($IntToDate(WDatInicio));
             WSessPrdTxt = $SetFiles('FPRDP012'); // Texto das notas descritivas
             $AddDBDate($IntToDate(WDatInicio));
             WSesCatNot4 = $SetFiles('FCATP106 A INNER JOIN +
                            FCATP105 B ON (A.CDPROD = B.CDPROD AND A.CDTEXTO +
                             = B.CDTEXTO)');
             $AddDBDate($IntToDate(WDatInicio));
             $SetFields('FCATP106':'A');
             $SetFields('FCATP105':'B':*OFF);
             WSesCatNot5 = $SetFiles('FCATP106':'A');
             $AddDBDate($IntToDate(WDatInicio));

             EXSR InzRecTxt; // Limpar a Lista de Ecrã
             EXSR  GetRecTexto; // Carregar Lista com Registos
             WIcLoad = *OFF;
             $RmvDBSession(WSessPrdNot);
             $RmvDBSession(WSessPrdTxt);
             $RmvDBSession(WSesCatNot4);
             $RmvDBSession(WSesCatNot5);
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
             WSesCatNot2 = $SetFiles('FCATP106'); // Notas descritivas de Catalogo
             $AddDBDate($IntToDate(WDatInicio));  //
             $SetCriteria('(CDPROD = '''+%CHAR(PRM_CdProd)+''') AND +
                          (CDTEXTO = '''+%CHAR(PRM_CdSuper)+''')');
             IF ($GetRecord(1) > *ZEROS);
                ECR_DSHDR = $GetField('DSTEXTO');
                WIcDetail = $GetField('ICDETAIL');
                WNoFunction = 'RPRDE10208'; // Manutenção de Itens
                IF (($GetField('ICRESERV') = 'S') OR (WIcDisplay));
                   WExceptKeys = '000001'; // F06 Off
                ELSE;
                   WExceptKeys = *BLANKS;
                ENDIF;
             ENDIF;
             $RmvDBSession(WSesCatNot2);
          ENDIF;
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:WExceptKeys);
          ECR_S#PGM = S#PGM;
          WRITE     RPRDE10200;
          WRITE     RPRDE10299;
          WRITE     RPRDE10298;
          IF (WRRN <> *ZEROS);
             IF (WRRN <= REC#);
                ECR_RRN = WRRN;
             ELSE;
                ECR_RRN = 1;
             ENDIF;
          ELSE;
             ECR_RRN = 1;
          ENDIF;
          WRRN = *ZEROS;
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
                PrintPreview(WPtrOr:'NT':WDatInicio);
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
                LEAVE;
             ENDIF;
          ENDIF;
       ENDDO;
       RETURN WResult;

       //============================================================
       // Subrotina..: ShowCapRisco
       // Objectivo..: repor Capitais de Risco
       //============================================================
       BEGSR ShowCapRisco;
          ECR_DSRISC001 = *BLANKS;
          ECR_CPRISC001 = *ZEROS;
          ECR_DSRISC002 = *BLANKS;
          ECR_CPRISC002 = *ZEROS;
          ECR_DSRISC003 = *BLANKS;
          ECR_CPRISC003 = *ZEROS;
          ECR_DSRISC004 = *BLANKS;
          ECR_CPRISC004 = *ZEROS;
          WRecAct = 1;
          DOW (NOT Lst#Risco(WRecAct).IsEmpty);
             WCdRisco  = Lst#Risco(WRecAct).CodRisco;
             WCapRisco = Lst#Risco(WRecAct).CapRisco;
             IF (WCdRisco > *ZEROS);
                IF (ECR_DSRISC001 = *BLANKS);
                   ECR_DSRISC001 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC001 = WCapRisco;
                ELSEIF (ECR_DSRISC002 = *BLANKS);
                   ECR_DSRISC002 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC002 = WCapRisco;
                ELSEIF (ECR_DSRISC003 = *BLANKS);
                   ECR_DSRISC003 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC003 = WCapRisco;
                ELSEIF (ECR_DSRISC004 = *BLANKS);
                   ECR_DSRISC004 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC004 = WCapRisco;
                   LEAVE;
                ENDIF;
             ENDIF;
             WRecAct += 1;
          ENDDO;
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
          IF (PRM_CdSuper = *ZEROS);
             WSessCatNot = WSesCatNot4;
             $SetDBSession(WSessCatNot);
             $SetSort('CDRISCO');
             $SetCriteria('A.CDPROD = ''' + %CHAR(PRM_CdProd) + '''');
          ELSE;
             WSessCatNot = WSesCatNot5;
             $SetDBSession(WSessCatNot);
             $SetSort('CDTEXTO');
             $SetCriteria('A.CDPROD = ''' + %CHAR(PRM_CdProd) + ''' +
                        AND A.CDSUPER = ''' + %CHAR(PRM_CdSuper) +
                       ''' AND A.CDSUPER <> A.CDTEXTO ');
          ENDIF;
          WRegAct = 1;
          DOW ($GetRecord(WRegAct) > *ZEROS);
             //----------------------------------------------
             // Validar se o texto está associado ao Risco
             //----------------------------------------------
             WIcExtract = *ON;
             IF (PRM_CDSUPER = *ZEROS);
                WIcExtract = *OFF;
                WIdx = 1;
                MONITOR;
                   DOW (NOT LST#Risco(Widx).IsEmpty);
                      IF ($GetFieldNum('B.CDRISCO')=Lst#Risco(WIdx).CodRisco);
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
                ECR_ICRESERV  = 'N';    // Pode adicionar linhas
                ECR_ICPROTEC  = 'N';    // Pode alterar o Capital Seguro
                ECR_NUSQSUP   = *ZEROS; // Não existe nível Superior
                ECR_DSTEXTO   = $GetField('A.DSTEXTO');
                WCdTexto      = $GetFieldNum('A.CDTEXTO');
                $SetDBSession(WSessPrdNot);
                $SetCriteria('(CDRAMO = '''+%CHAR(PRM_CdRamo)+''') AND +
                             (NUAPOL = '''+%CHAR(PRM_APOLICE)+''') AND +
                            (CDTEXTO = '''+%CHAR(WCdTexto)+''')');
                IF ($GetRecord(1) > *ZEROS);
                   ECR_NUSQSUP   = $GetFieldNum('NUSQSUP');   // Sequencia Superior
                   ECR_NUSEQU    = $GetFieldNum('NUSEQU');    // Numero de Sequencia
                   ECR_CPSEGURO =GetCpSeguro(PRM_CdRamo:PRM_Apolice:ECR_NUSEQU);
                   ECR_CPSEGUROBK= ECR_CPSEGURO;

                   $SetDBSession(WSessPrdNot);
                   $SetCriteria('(CDRAMO = '''+%CHAR(PRM_CdRamo)+''') AND +
                                 (NUAPOL = '''+%CHAR(PRM_APOLICE)+''') AND +
                                 (NUSQSUP = '''+%CHAR(ECR_NUSEQU)+''') AND +
                                 (NUSEQU <> NUSQSUP)');
                   IF ($GetRecord(1) > *ZEROS);
                      ECR_DSTEXTO = %TRIM(WMoreText) + ECR_DSTEXTO;
                      ECR_ICPROTEC = 'S';
                      *IN45 = *ON;
                   ENDIF;
                ENDIF;

                $SetDBSession(WSessCatNot);
                REC# += 1;
                ECR_CDTEXTO   = WCdTexto;
                ECR_CDPROD    = PRM_CDPROD;
                ECR_ICOBRIGA  = $GetField('A.ICOBRIGA');
                ECR_ICRESERV  = $GetField('A.ICRESERV');
                ECR_ICDETAIL  = $GetField('A.ICDETAIL');
                ECR_DSTEXTOBK = ECR_DSTEXTO;
                WRITE RPRDE10202;
             ENDIF;
             WRegAct += 1;
          ENDDO;

          EXSR GetFreeTexto;
          IF (REC# = *ZEROS);
             *IN52 = *OFF;
          ENDIF;
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
             $SetDBSession(WSessPrdNot);
             $SetCriteria('(CDRAMO = '''+%CHAR(PRM_CdRamo)+''') AND +
                           (NUAPOL = '''+%CHAR(PRM_Apolice)+''') AND +
                          (CDTEXTO = '''+%CHAR(PRM_CdSuper)+''')');
             IF ($GetRecord(1) > *ZEROS);
                WNuSqSup = $GetFieldNum('NUSEQU');
                WSesCatNot2 = $SetFiles('FCATP106'); // Notas descritivas de Catalogo
                $AddDBDate($IntToDate(WDatInicio));  //
                $SetCriteria('(CDPROD = '''+%CHAR(PRM_CdProd)+''') AND +
                             (CDTEXTO = '''+%CHAR(PRM_CdSuper)+''')');
                IF ($GetRecord(1) > *ZEROS);
                   WNoApp = $GetFunction($GetFieldNum('CDFUNCT'));
                ENDIF;
                WIcExtract = *ON;
                $RmvDBSession(WSesCatNot2);

                WRegAct = 1;
                $SetDBSession(WSessPrdNot);
                $SetCriteria('(CDRAMO = '''+%CHAR(PRM_CdRamo)+''') AND +
                              (NUAPOL = '''+%CHAR(PRM_Apolice)+''') AND +
                             (NUSQSUP = '''+%CHAR(WNuSqSup)+''')');
                DOW ($GetRecord(WRegAct) > *ZEROS);
                   IF ($GetFieldNum('CDTEXTO') = *ZEROS);
                      ECR_DSTEXTO = '<NÃO ESPECIFICADO>';
                      WNuSequ = $getFieldNum('NUSEQU');
                      $SetDBSession(WSessPrdTxt);
                      $SetCriteria('(CDRAMO = '''+%CHAR(PRM_CdRamo)+''') AND +
                               (NUAPOL = '''+%CHAR(PRM_Apolice)+''') AND +
                               (NUSEQU = '''+%CHAR(WNuSequ)+''')');
                      IF ($GetRecord(1) > *ZEROS);
                         ECR_DSTEXTO = $GetField('DSDETAIL');
                         IF ($GetField('ICDETAIL') = 'S');
                            ECR_DSTEXTO = %TRIM(WMoreText)+%TRIM(ECR_DSTEXTO);
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
                      $SetDBSession(WSessPrdNot);
                      ECR_CDTEXTO   = *ZEROS;
                      ECR_NUSQSUP   = $GetFieldNum('NUSQSUP');  // Sequencia Superior
                      ECR_NUSEQU    = $GetFieldNum('NUSEQU');   // Numero de Sequencia
                      ECR_CPSEGURO  = GetCpSeguro(PRM_CdRamo:PRM_Apolice:
                                                  ECR_NUSEQU);
                      ECR_CPSEGUROBK= ECR_CPSEGURO; // Backup do Capital Seguro
                      ECR_ICDETAIL  = 'N';
                      ECR_ICRESERV  = 'N';          // Pode adicionar linhas
                      ECR_ICPROTEC  = 'N';
                      ECR_NUSQSUP   = WNuSqSup;     // Não existe nível Superio
                      ECR_DSTEXTOBK = ECR_DSTEXTO;
                      WRITE RPRDE10202;
                   ENDIF;
                   $SetDBSession(WSessPrdNot);
                   WRegAct += 1;
                ENDDO;
             ENDIF;
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
             // Produto parameterizado para abrir outro nível
             //------------------------------------------------
             IF (ECR_OPTION <> *BLANKS);
                WSelRec    = REC#;
                WIcDetail  = WICDetail;
                ECR_OPTION = *BLANKS;
             ENDIF;
             READC RPRDE10202;
          ENDDO;

          IF (WIcChanged = 'S');
             UpdCapRisco(PRM_CdRamo:PRM_APOLICE);
             WIcLoad = *ON;
          ENDIF;

          //----------------------------------
          // Invocar nivel Seguinte
          //----------------------------------
          IF (WSelRec > *ZEROS);
             WRRN = REC#;
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
                   UpdCapRisco(PRM_CdRamo:PRM_APOLICE);
                ON-ERROR;
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
                   CHAIN(N) (PRM_CdRamo:PRM_APOLICE:ECR_NUSEQU) RPRDP010;
                   WNuSqSup = PRD_NUSQSUP;
                   MONITOR;
                      DELETE (PRM_CdRamo:PRM_APOLICE:ECR_NUSEQU) RPRDP012;
                   ON-ERROR;
                   ENDMON;
                   DELETE (PRM_CdRamo:PRM_APOLICE:ECR_NUSEQU) RPRDP010;

                   //----------------------------------------
                   // Eliminar níveis Superiores
                   //----------------------------------------
                   DelPai(PRM_CdRamo:PRM_APOLICE:WNuSqSup);
                ON-ERROR;
                ENDMON;
             ELSE;
                //---------------------
                // Alterar o Registo
                //---------------------
                CHAIN (PRM_CdRamo:PRM_APOLICE:ECR_NUSEQU) RPRDP012;
                IF (%FOUND(FPRDP012));
                   PRD_CPSEGURO = ECR_CPSEGURO;
                   PRD_DSDETAIL = ECR_DSTEXTO;
                   UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_CPSEGURO);
                ELSE;
                   PRD_CDRAMO   = PRM_CdRamo;
                   PRD_NUAPOL   = PRM_Apolice;
                   PRD_NUSEQU   = ECR_NUSEQU;
                   PRD_CPSEGURO = ECR_CPSEGURO;
                   PRD_DSDETAIL = ECR_DSTEXTO;
                // PRD_PRD01202  = %DATE('9999-12-31');
                // PRD_PRD01203  = 1;
                   WRITE RPRDP012;
                ENDIF;
             ENDIF;
          ELSE;
             //-----------------------------------------------------
             // Adicionar o Registo
             //-----------------------------------------------------
             IF (ECR_CPSEGURO > *ZEROS);
                // IF (WNuSqSup = *ZEROS) AND (PRM_CdSuper > *ZEROS);
                IF (PRM_CdSuper > *ZEROS);
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
          PRD_CDRAMO   = PRM_CdRamo;
          PRD_NUAPOL   = PRM_Apolice;
          PRD_NUSEQU   = *ZEROS;
          PRD_CDTEXTO  = WCdTexto;
          PRD_NUSQSUP  = WNuSqSup;
          WRITE RPRDP010;

          PRD_CDRAMO = PRM_CdRamo;
          PRD_NUAPOL = PRM_Apolice;
          PRD_NUSEQU = $GetNextKey('FPRDP010':'NUSEQU':'(CDRAMO='''+
                       %TRIM(%CHAR(PRM_CDRAMO)) + ''') AND (NUAPOL='''+
                       %TRIM(%CHAR(PRM_Apolice)) + ''')');
          PRD_NUSEQU  -= 1;
          PRD_ICDETAIL = 'N';
          PRD_DSDETAIL = ECR_DSTEXTO;
          PRD_CPSEGURO = ECR_CPSEGURO;
       // PRD_PRD01202  = %DATE('9999-12-31');
       // PRD_PRD01203  = 1;
          WRITE RPRDP012;
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
     D WSessPrdNot     S              5S 0
     D WSessCatNot     S              5S 0
     D WCdSuper        S              5S 0
     D WCdTexto        S              5S 0
     D*=======================================================
      /FREE
         //---------------------------------------------
         // Notas descritivas do Produto
         //---------------------------------------------
         WSessPrdNot = $SetFiles('FPRDP010'); // Notas descritivas produros
         $AddDBDate($IntToDate(WDatInicio));
         WSessCatNot = $SetFiles('FCATP106'); // Notas descritivas catalogo
         $AddDBDate($IntToDate(WDatInicio));

         WNuSqSup = *ZEROS;
         $SetDBSession(WSessPrdNot);
         $SetCriteria('(CDRAMO='''+%CHAR(PRM_CdRamo)+''') AND +
                       (NUAPOL='''+%CHAR(PRM_Apolice)+''') AND +
                      (CDTEXTO='''+%CHAR(PRM_CdSuper)+''')');
         IF ($GetRecord(1) > *ZEROS);
            //--------------------------------------------------
            // Validar se Existe detalhe associado á nota
            //--------------------------------------------------
            WNuSqSup = $GetFieldNum('NUSEQU');
            CHAIN (PRM_CdRamo:PRM_Apolice:WNuSqSup) RPRDP012;
            IF (%FOUND(FPRDP012));
               DELETE RPRDP012;
            ENDIF;
         ELSE;
            $SetDBSession(WSessCatNot);
            $SetCriteria('(CDPROD='''+%CHAR(PRM_CdProd)+''') AND +
                         (CDTEXTO='''+%CHAR(PRM_CdSuper)+''')');
            IF ($GetRecord(1) > *ZEROS);
               WCdSuper = $GetFieldNum('CDSUPER');
               WCdTexto = $GetFieldNum('CDTEXTO');
               IF (WCdSuper <> WCdTexto);
                  WNuSqSup = CheckPai(PRM_CdRamo:PRM_CdRamo:
                                      PRM_Apolice:WCdSuper);
               ENDIF;

               PRD_CDRAMO   = PRM_CdRamo;
               PRD_NUAPOL   = PRM_Apolice;
               PRD_NUSEQU   = *ZEROS;
               PRD_CDTEXTO  = PRM_CDSUPER;
               PRD_NUSQSUP  = WNuSqSup;
               WRITE RPRDP010;
               WNuSqSup  = $GetNextKey('FPRDP010':'NUSEQU':'(CDRAMO='''+
                           %TRIM(%CHAR(PRM_CDRAMO)) + ''') AND (NUAPOL='''+
                           %TRIM(%CHAR(PRM_Apolice)) + ''')');
               WNuSqSup -= 1;
            ENDIF;
         ENDIF;

         $RmvDBSession(WSessPrdNot);
         $RmvDBSession(WSessCatNot);
         RETURN (WNuSqSup);
      /END-FREE
     P CheckPai        E

     P*=======================================================
     P* Processo..: DelPai
     C* Objectivo.: Eliminar níveis Superiores.
     P*=======================================================
     P DelPai          B                   EXPORT
     D DelPai          PI              N
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D    PRM_CdSuper                 5S 0 CONST
     D
     D*==> Campos de Trabalho
     D WCdSuper        S              5S 0
     D WSessPrdNot     S              5S 0
     D*=======================================================
      /FREE
         //---------------------------------------------
         // Notas descritivas do Produto
         //---------------------------------------------
         IF (PRM_CdSuper > *ZEROS);
            WSessPrdNot = $SetFiles('FPRDP010'); // Notas descritivas produtos
            $AddDBDate($IntToDate(WDatInicio));
            $SetCriteria('(CDRAMO='''+%CHAR(PRM_CdRamo)+''') AND +
                          (NUAPOL='''+%CHAR(PRM_Apolice)+''') AND +
                         (NUSQSUP='''+%CHAR(PRM_CdSuper)+''') AND +
                         (NUSEQU <> NUSQSUP)');
            IF ($GetRecord(1) = *ZEROS);
               $RmvDBSession(WSessPrdNot);
               //--------------------------------------------------
               // Validar se Existe detalhe associado á nota
               //--------------------------------------------------
               CHAIN (PRM_CdRamo:PRM_Apolice:PRM_CdSuper) RPRDP010;
               IF (%FOUND(FPRDP010));
                  WCdSuper = PRD_NuSqSup;
                  DELETE RPRDP010;
               ENDIF;
               IF (WCdSuper <> PRM_CDSUPER);
                  DelPai(PRM_CdRamo:PRM_Apolice:WCdSuper);
               ENDIF;
            ELSE;
               $RmvDBSession(WSessPrdNot);
               RETURN *ON;
            ENDIF;
         ENDIF;

         RETURN *ON;
      /END-FREE
     P DelPai          E
     P*=======================================================
     P* Processo..: GetCpSeguro
     P* Objectivo.: Obter o Capital Seguro, a partir dos níveis
     P*             depemdentes.
     P*=======================================================
     P GetCpSeguro     B                   EXPORT
     D GetCpSeguro     PI            11S 2
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D    PRM_NuSqSup                 5S 0 CONST
     D
     D*==> Campos de Trabalho
     D WRegAct         S              5S 0 INZ(*ZEROS)
     D WNuSequ         S              5S 0 INZ(*ZEROS)
     D WCapSeguro      S             11S 2 INZ(*ZEROS)
     D WSessPrdNot     S              5S 0
     D WSessPrdTxt     S              5S 0
     D*=======================================================
      /FREE
         WCapSeguro = *ZEROS;

         //---------------------------------------------
         // Notas descritivas do Produto
         //---------------------------------------------
         WSessPrdNot = $SetFiles('FPRDP010');
         $AddDBDate($IntToDate(WDatInicio));
         WSessPrdTxt = $SetFiles('FPRDP012');
         $AddDBDate($IntToDate(WDatInicio));

         $SetDBSession(WSessPrdTxt);
         $SetCriteria('(CDRAMO='''+%CHAR(PRM_CdRamo)+''') AND +
                       (NUAPOL='''+%CHAR(PRM_Apolice)+''') AND +
                       (NUSEQU='''+%CHAR(PRM_NuSqSup)+''')');
         IF ($GetRecord(1) > *ZEROS);
            WCapSeguro = $GetFieldNum('CPSEGURO');
         ELSE;
            WRegAct = 1;
            $SetDBSession(WSessPrdNot);
            $SetCriteria('(CDRAMO='''+%CHAR(PRM_CdRamo)+''') AND +
                          (NUAPOL='''+%CHAR(PRM_Apolice)+''') AND +
                         (NUSQSUP='''+%CHAR(PRM_NuSqSup)+''') AND +
                         (NUSQSUP <> NUSEQU)');
            DOW ($GetRecord(WRegAct) > *ZEROS);
               WNuSequ = $GetFieldNum('NUSEQU');
               WCapSeguro += GetCpSeguro(PRM_CdRamo:PRM_Apolice:WNuSequ);
               $SetDBSession(WSessPrdNot);
               WRegAct += 1;
            ENDDO;
         ENDIF;
         $RmvDBSession(WSessPrdNot);
         $RmvDBSession(WSessPrdTxt);

         RETURN WCapSeguro;
      /END-FREE
     P GetCpSeguro     E
     P*=======================================================
     P* Processo..: UpdCapRisco
     P* Objectivo.: Actualizar os Capitais ou apagar os Itens
     P*             Superiores.
     P*=======================================================
     P UpdCapRisco     B                   EXPORT
     D UpdCapRisco     PI
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D
     D*==> Lista de Trabalho de Capitais
     D LST#Cap         DS                  QUALIFIED DIM(20)
     D    CdRisco                     5S 0 INZ(*ZEROS)
     D    VlCapital                  11S 2 INZ(*ZEROS)
     D    IsEmpty                      N   INZ(*ON)
     D WIdx            S              3S 0 INZ(*ZEROS)
     D WIcencontrou    S               N   INZ(*OFF)
     D
     D*==> Campos de Trabalho
     D WRegActPrd      S              5S 0 INZ(*ZEROS)
     D WRegActCat      S              5S 0 INZ(*ZEROS)
     D WCapSeguro      S             11S 2 INZ(*ZEROS)
     D WSessPrdNot     S              5S 0 INZ(*ZEROS)
     D WSessCatRis     S              5S 0 INZ(*ZEROS)
     D WSessCapRisc    S              5S 0 INZ(*ZEROS)
     D WNuSequ         S              5S 0 INZ(*ZEROS)
     D WCdRisco        S              5S 0 INZ(*ZEROS)
     D WCdSuper        S              5S 0 INZ(*ZEROS)
     D WCodigo         S              5S 0 INZ(*ZEROS)
     D WCdTexto        S              5S 0 INZ(*ZEROS)
     D*=======================================================
      /FREE
         //---------------------------------------------
         // Notas descritivas do Produto
         //---------------------------------------------
         WSessPrdNot = $SetFiles('FPRDP010');
         $AddDBDate($IntToDate(WDatInicio));
         WSessCatRis = $SetFiles('FCATP101 A INNER JOIN FCATP105 B +
                                  ON (A.CDPROD = B.CDPROD) AND +
                                  (A.CDRISCO = B.CDRISCO)');
         $AddDBDate($IntToDate(WDatInicio));
         $SetFields('FCATP101':'A');
         $SetFields('FCATP105':'B':*OFF);

         WRegActPrd = 1;
         $SetDBSession(WSessPrdNot);
         $SetCriteria('(CDRAMO ='''+%CHAR(PRM_CdRamo)+''') AND +
                       (NUAPOL ='''+%CHAR(PRM_Apolice)+''') AND +
                       (NUSEQU = NUSQSUP)');
         DOW ($GetRecord(WRegActPrd) > *ZEROS);
            WCdTexto   = $GetFieldNum('CDTEXTO');
            WNuSequ    = $GetFieldNum('NUSEQU');
            WCapSeguro = GetCpSeguro(PRM_CdRamo:PRM_Apolice:WNuSequ);
            WRegActCat = 1;
            $SetDBSession(WSessCatRis);
            $SetCriteria('(A.CDPROD='''+%CHAR(WCodProduto)+''') AND +
                          (B.CDTEXTO = '''+%CHAR(WCdTexto)+''')');
            DOW ($GetRecord(WRegActCat) > *ZEROS);
               //-----------------------------------
               // Actualizar a informação do Risco
               //-----------------------------------
               WCdRisco = $GetFieldNum('A.CDRISCO');
               WCodigo = WCdRisco;
               EXSR ActListas;

               //-----------------------------------
               // Actualizar a informação do Superior
               //-----------------------------------
               WCdSuper = $GetFieldNum('A.CDSUPER');
               IF (WCdSuper > *ZEROS);
                  WCodigo = WCdSuper;
                  EXSR ActListas;
               ENDIF;
               WRegActCat += 1;
            ENDDO;
            $SetDBSession(WSessPrdNot);
            WRegActPrd += 1;
         ENDDO;
         $RmvDBSession(WSessPrdNot);
         $RmvDBSession(WSessCatRis);

         //-------------------------------------------------------
         // Actualizar os Valores dos Códigos de Risco
         //-------------------------------------------------------
         WSessCapRisc = $SetFiles('FPRDP011');
         $AddDBDate($IntToDate(WDatInicio));
         OR_CAP1 = *ZEROS;
         OR_CAP2 = *ZEROS;
         OR_CAP3 = *ZEROS;
         Widx = 1;
         DOW (NOT LST#Cap(Widx).IsEmpty);
            WCdRisco = LST#Cap(Widx).CdRisco;
            WCapSeguro = LST#Cap(Widx).VlCapital;
            $SetCriteria('(CDRAMO='''+%CHAR(PRM_CDRAMO)+''') AND +
                          (NUAPOL='''+%CHAR(PRM_Apolice)+''') AND +
                          (CDRISCO= '''+%CHAR(WCdRisco)+''')');
            IF ($GetRecord() > *ZEROS);
               WNuSequ = $GetfieldNum('NUSEQU');
               CHAIN (PRM_CDRAMO:PRM_Apolice:WNuSequ) RPRDP011;
               IF (%FOUND(FPRDP011));
                  PRD_IMCPRISC = WCapSeguro;
                  UPDATE RPRDP011 %FIELDS(PRD_IMCPRISC);
               ENDIF;
            ENDIF;

            IF (OR_CR1 = WCdRisco);
               OR_CAP1 += WCapSeguro;
            ELSEIF (OR_CR2 = WCdRisco);
               OR_CAP2 += WCapSeguro;
            ELSEIF (OR_CR3 = WCdRisco);
               OR_CAP3 += WCapSeguro;
            ENDIF;
            Widx += 1;
         ENDDO;
         $RmvDBSession(WSessCapRisc);

         //======================================================================
         // Subrotina..: ActListas
         // Objecitvo..: Actualizar a Lista de Riscos
         //======================================================================
         BEGSR Actlistas;
            WIcEncontrou = *OFF;
            WIdx = 1;
            DOW (NOT LST#Cap(Widx).IsEmpty);
               IF (LST#Cap(Widx).CdRisco = WCodigo);
                  LST#Cap(Widx).VlCapital += WCapSeguro;
                  WIcEncontrou = *ON;
                  LEAVE;
               ENDIF;
               Widx += 1;
            ENDDO;
            IF (NOT WIcEncontrou);
               LST#Cap(Widx).CdRisco   = WCodigo;
               LST#Cap(Widx).VlCapital = WCapSeguro;
               LST#Cap(Widx).IsEmpty   = *OFF;
            ENDIF;
         ENDSR;
      /END-FREE
     P UpdCapRisco     E

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
       // WCapSeguro = *ZEROS;
       // SETLL (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP01002;
       // READE (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP01002;
       // DOW (NOT %EOF());
       //    IF (PRD_NuSqSup <> PRD_NuSequ);
       //       WCapSeguro += PRD_CpSeguro;
       //    ENDIF;
       //    READE (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP01002;
       // ENDDO;

       // CHAIN(N) (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP010;
       // IF (%FOUND());
       //    IF (WCapSeguro > *ZEROS);
       //       CHAIN (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP010;
       //       PRD_CpSeguro = WCapSeguro;
       //       UPDATE RPRDP010 %FIELDS(PRD_CPSEGURO);
       //    ELSE;
       //       DELETE (PRM_CdRamo:PRM_Apolice:PRM_NuSqSup) RPRDP010;
       //    ENDIF;
       //    IF (PRD_NuSqSup <> PRM_NuSqSup);
       //       WCapSeguro = UpdatePai(PRM_CdRamo:PRM_Apolice:PRD_NuSqSup);
       //    ENDIF;
       // ENDIF;

         RETURN WCapSeguro;
      /END-FREE
     P UpdatePai       E
