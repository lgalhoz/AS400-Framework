     H NOMAIN
     H*BNDDIR('JMDIR')
     H*ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H*AUT(*ALL)
     H*================================================================
     H* Dat.Cria...: 2011-05-12
     H* Autor......: Luis Galhoz
     H* Objectivo..: Devolver o Header da função, junto com as
     H*              teclas a que o utilizador está autorizado.
     H*
     H*----------------------------------------------------------------
     H* [OK] ==> Colocar a função NOMAIN, à cabeça. Criar estrutura de
     H* dados indexada ao programa e função, para passar a
     H* gravar as opções e teclas de função a omitir.
     H*----------------------------------------------------------------
     H* [OK] ==> Alterar as funções $GetOptions e $GetFooter, para passarem a
     H* receber os Parametros Hide, como opcional.
     H*----------------------------------------------------------------
     H* [OK] ==> Criar uma nova função para passar a receber excepção, ou as
     H* teclas e funções a omitir. Em baixo as funções:
     H*     $KeysToOmit(<programa>:<função>:<Teclas/Nível>);
     H*     $OptionsToOmit(<programa>:<função>:<opções/Nível>);
     H*
     H*----------------------------------------------------------------
     H* [OK] E em matéria de hierarquias, as opções actualizadas pelas
     H* funções $KeysToOmit() e $OptionsToOmit(), sobrepoem-se
     H* a todas as outras.
     H*----------------------------------------------------------------
     H* [xx] ==> Alterar a base de dados de Seguranças, para passar a
     H* receber o nível de risco das opções e teclas de função, e onde
     H* os níveis mais baixos correspondem às acções simples e
     H* sem risco, e os níveis mais elevados as acções complexas e de
     H* risco elevado.
     H* Por exemplo e para as Teclas de Função:
     H*    [x] F03=Sair             --> Nível 0
     H*    [x] F12=Cancelar         --> Nível 0
     H*    [x] F04=Valores          --> Nível 1
     H*    [x] F06=Criar registo    --> Nível 2
     H*    [x] F21=Alterar registo  --> Nível 3
     H*    [x] F22=Eliminar registo --> Nível 4
     H*----------------------------------------------------------------
     H* [xx] ==> Alterar o programa de manutenção de Opções e Teclas de
     H* função para passar a registar o nível da função ou opção.
     H*================================================================
     FFSECP03101IF   E           K DISK    PREFIX(SEC_)
     F                                     RENAME(RSECP031:RSECP03101)
     F*================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SSECO010

     D*==> Procedimentos internos
     D ValidSpecial_   PR                  EXTPGM('RSECO002')
     D   NomAppl                     10A   CONST
     D   NomFunction                 10A   CONST
     D   NumOption                    2S 0 CONST
     D   Origem                       1A   CONST
     D   Valor                       50A   CONST
     D   Resultado                     N   CONST
     D
     D*==> Recebe o devolver as omissões especiais
     D GetHideSett_    PR            24A
     D   NomAPP                      10A   Const
     D   NomFmt                      10A   Const
     D   Incidencia                   1A   Const
     D   IsToInsert                    N   Const Options(*NOPASS)
     D   Hide                        24A   Const Options(*NOPASS)
     D
     D*==> Estrutura de dados dos ecrãs
     D LST#SCREEN      DS                  QUALIFIED DIM(KTE#MaxScr)
     D   Name                        10A   INZ(*BLANKS)
     D   Fmt                               LIKEDS(TPLSCREEN) DIM(KTE#MaxFmt)
     D   IsEmpty                       N   INZ(*ON)
     D WIdxScn         S              3S 0
     D
     D TPLSCREEN       DS                  QUALIFIED
     D   Name                        10A   INZ(*BLANKS)
     D   MaxKeyLvl                    2S 0 INZ(99)
     D   KeyToHide                   24A   INZ(*BLANKS)
     D   MaxOptLvl                    2S 0 INZ(99)
     D   OptionToHide                24A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D WIdxFmt         S              3S 0
     D
     D KTE#MaxFmt      C                   10
     D KTE#MaxScr      C                   25
     D
     D*==> Campos de Trabalho
     D WDfAccess       S              1A
     D WDsOpera        S             60A
     D WDescCod        S             20A
     D WNomAppl        S             10A
     D WNomFunction    S             10A
     D WMaxRec         S              5S 0 INZ(KTEMaxRec)
     D KTEMaxRec       C                    25
     C*=======================================================
     C* Processo..: $GetHeader
     C* Objectivo.: Disponibilizar o Titulo Para a Função
     C*=======================================================
     P $GetHeader      B                   EXPORT
     D                 PI           114A
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D WResult         S            114A
     D WDummy          S            114A
     D WDsProgr        S             57A
     D WDsFunct        S             57A
     D WFillSpace      S              5S 0
     D WSqlStmt        S            300A
     C
      /FREE
         //------------------------------------------
         // Obter a descrição para o titulo da Função
         //------------------------------------------
         WNomAppl     = AppName;
         WNomFunction = Function;
         WResult ='Titulo não definido';
         WSqlStmt = 'SELECT dsprogr, dsfunct from fsecp010 INNER JOIN +
                     fsecp011 ON (fsecp010.cdprogr = fsecp011.cdprogr) +
                     WHERE (fsecp010.nmprogr = '''+ %TRIM(AppName) +
                     ''' AND fsecp011.nmfunct = ''' + %TRIM(Function)+''')';
         EXEC SQL PREPARE STMTC1 FROM :WSqlStmt;
         EXEC SQL DECLARE C1 CURSOR FOR STMTC1;
         EXEC SQL OPEN C1;
         IF (sqlcod = 0);
            exec sql fetch C1 INTO :WDsProgr, :WDsFunct;
         ENDIF;
         EXEC SQL CLOSE C1;

          //-----------------------------------------
          // Centrar Texto (Titulo e Sub-Titulo)
          //-----------------------------------------
          WDummy = *BLANKS;
          WFillSpace = %int((57-%Len(%Trim(WDsProgr)))/2);
          WResult = %subst(WDummy:1:WFillSpace) + WDsProgr;
          WFillSpace = %int((57-%Len(%Trim(WDsFunct)))/2);
          WResult = %subst(WResult:1:57)+
                    %subst(WDummy:1:WFillSpace) + WDsFunct;
          RETURN WResult;
      /END-FREE
     P $GetHeader      E

     C*=======================================================
     C* Processo..: GetHideSett_
     C* Objectivo.: Gererir excepções nas seguranças
     C*=======================================================
     P GetHideSett_    B                   EXPORT
     D                 PI            24A
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D  Incidencia                    1A   CONST
     D  IsToInsert                     N   CONST Options(*NOPASS)
     D  Hide                         24A   CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WIdxScn         S              3S 0
     D WIdxFmt         S              3S 0
     D WIcFound        S               N
     D WMaxKeyLvl      S              2S 0
     D WKeyToHide      S             24A
     D WMaxOptLvl      S              2S 0
     D WOptionToHide   S             24A
     D WMaxLevel       S              2S 0
     D WHide           S             24A
     D WResult         S             24A
     D WIsToInsert     S               N
     D WHideParm       S             24A
     D*=======================================================
      /FREE
        //---------------------------------------------------------
        // ForCompr Parametros Recebidos
        //---------------------------------------------------------
        WResult     = *ZEROS;
        WIsToInsert = *OFF;
        WHideParm   = *ZEROS;
        IF (%PARMS() > 3);
           WIsToInsert = IsToInsert;
           IF (%PARMS() > 4);
              WHideParm = Hide;
              WResult   = WHideParm;
           ENDIF;
        ENDIF;

        WMaxLevel = 99;
        WHide     = *ZEROS;
        IF (WHideParm <> *ZEROS);
           IF (%LEN(%TRIM(WHideParm)) <= 2);
              MONITOR;
                 WMaxLevel = %DEC(WHideParm:2:0);
              ON-ERROR;
              ENDMON;
           ELSE;
              Whide = WHideParm;
           ENDIF;
        ENDIF;

        //---------------------------------------------------------
        // Localizar o ecrã
        //---------------------------------------------------------
        WIdxScn = 1;
        WIdxFmt = 1;
        DOW ((NOT LST#Screen(WIdxScn).IsEmpty) AND (WIdxScn <= KTE#MaxScr));
           WIcFound = *OFF;
           WIdxFmt = 1;
           IF (%TRIM(LST#Screen(WIdxScn).Name) = %TRIM(AppName));
              DOW (NOT LST#Screen(WIdxScn).Fmt(WIdxFmt).IsEmpty);
                 IF (%TRIM(LST#Screen(WIdxScn).Fmt(WidxFmt).Name) =
                        %TRIM(Function));
                    WIcFound = *ON;
                    IF (Incidencia = 'O');
                       WResult = LST#Screen(WIdxScn).Fmt(WIdxFmt).OptionToHide;
                       IF (LST#Screen(WIdxScn).Fmt(WIdxFmt).MaxOptLvl <> 99);
                          WResult = %CHAR(
                                   LST#Screen(WIdxScn).Fmt(WIdxFmt).MaxOptLvl);
                       ENDIF;
                    ELSE;
                       Wresult = LST#Screen(WIdxScn).Fmt(WIdxFmt).KeyToHide;
                       IF (LST#Screen(WIdxScn).Fmt(WIdxFmt).MaxKeyLvl <> 99);
                          WResult = %CHAR(
                                   LST#Screen(WIdxScn).Fmt(WIdxFmt).MaxKeyLvl);
                       ENDIF;
                    ENDIF;
                    LEAVE;
                 ENDIF;
                 WIdxFmt += 1;
              ENDDO;
              LEAVE; // Não incrementa o ecrã
           ENDIF;
           WIdxScn += 1;
        ENDDO;

        //--------------------------------------------------
        // Criar o registo
        //--------------------------------------------------
        IF (WIsToInsert);
           IF (NOT WIcFound);
              IF (WIdxScn > KTE#MaxScr);
                 WIdxScn = 1;
              ENDIF;
              LST#Screen(WIdxScn).Name    = AppName;
              LST#Screen(WIdxScn).IsEmpty = *OFF;
              LST#Screen(WIdxScn).Fmt(WIdxFmt).Name = Function;
              LST#Screen(WIdxScn).Fmt(WIdxFmt).IsEmpty = *OFF;
              LST#Screen(WIdxScn).Fmt(WIdxFmt+1).IsEmpty = *ON;
              LST#Screen(WIdxScn).Fmt(WIdxFmt).MaxKeyLvl    = 99;
              LST#Screen(WIdxScn).Fmt(WIdxFmt).KeyToHide    = *ZEROS;
              LST#Screen(WIdxScn).Fmt(WIdxFmt).MaxOptLvl    = 99;
              LST#Screen(WIdxScn).Fmt(WIdxFmt).OptionToHide = *ZEROS;
              IF (Incidencia = 'O');
                 LST#Screen(WIdxScn).Fmt(WIdxFmt).MaxOptLvl    = WMaxLevel;
                 LST#Screen(WIdxScn).Fmt(WIdxFmt).OptionToHide = WHide;
              ELSE;
                 LST#Screen(WIdxScn).Fmt(WIdxFmt).MaxKeyLvl    = WMaxLevel;
                 LST#Screen(WIdxScn).Fmt(WIdxFmt).KeyToHide    = WHide;
              ENDIF;

              IF (WIdxScn > KTE#MaxScr);
                 LST#Screen(1).IsEmpty = *ON;
              ELSE;
                 LST#Screen(WIdxScn+1).IsEmpty = *ON;
              ENDIF;
           ELSE;
              IF (Incidencia = 'O');
                 LST#Screen(WIdxScn).Fmt(WIdxFmt).MaxOptLvl    = WMaxLevel;
                 LST#Screen(WIdxScn).Fmt(WIdxFmt).OptionToHide = WHide;
              ELSE;
                 LST#Screen(WIdxScn).Fmt(WIdxFmt).MaxKeyLvl    = WMaxLevel;
                 LST#Screen(WIdxScn).Fmt(WIdxFmt).KeyToHide    = WHide;
              ENDIF;
           ENDIF;
        ENDIF;
        RETURN WResult;
      /END-FREE
     P GetHideSett_    E

     C*=======================================================
     C* Processo..: $OptionsToOmit
     C* Objectivo.: Indentifica as opções a omitir na sessão
     C*=======================================================
     P $OptionsToOmit  B                   EXPORT
     D                 PI
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D  Hide                         24A   CONST
     D
     D*=======================================================
      /FREE
         GetHideSett_(AppName:Function:'O':*ON:Hide);
      /END-FREE
     P $optionsToOmit  E

     C*=======================================================
     C* Processo..: $GetOption
     C* Objectivo.: Disponibilizar a Barra de Opções
     C*=======================================================
     P $GetOption      B                   EXPORT
     D                 PI           152A
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D  Hide                         24A   CONST Options(*NOPASS)
     D
     D WFuncKeyDesc    DS           156
     D   WDescLine1            1     78A
     D   WDescLine2           79    156A
     D   WResult       S            156A
     D   WResultb      S            156A
     D   WPosAct       S              5S 0 INZ(*ZEROS)
     D   WIcAutoriz    S               N
     D
     D WRegisto        DS                  QUALIFIED DIM(KTEMaxRec)
     D   WDsOpera                    20A
     D   WDfAccess                    1A
     D   WNuOption                    2S 0
     D
     D*==> Campos de Trabalho
     D WIdx            S              5S 0
     D WSqlStmt        S           1000A
     D WHide           S             24A
     D WNuOption       S              2S 0
     D*=================================================================
      /FREE
         WNomAppl     = AppName;
         WNomFunction = Function;
         WHide        = GetHideSett_(WnomAppl:WNomFunction:'O');

         //---------------------------------------------------
         // Respeitar a hierarquia de autorizações
         //---------------------------------------------------
         IF (%PARMS() > 2);
            WIdx  = 1;
            DOW (WIdx < 24);
               IF (%SUBST(WHide:Widx:1) = '1');
                  %SUBST(WHide:WIdx:1) = '1';
               ELSE;
                  %SUBST(Whide:Widx:1) = %SUBST(Hide:WIdx:1);
               ENDIF;
               WIdx += 1;
            ENDDO;
         ENDIF;

         WSqlStmt = 'SELECT DSOPERA, DFACCESS, NUOPTION FROM fsecp010 +
                     INNER JOIN (fsecp011 inner join fsecp012 on +
                     (fsecp011.cdprogr = fsecp012.cdprogr) and +
                     (fsecp011.cdfunct = fsecp012.cdfunct)) +
                     ON (fsecp010.cdprogr = fsecp011.cdprogr) WHERE +
                     (fsecp010.NmProgr = ''' + %TRIM(AppName)+
                     ''') AND (fsecp011.NMFunct = '''+%TRIM(Function)+
                     ''') AND (fsecp012.NuOption > 0)';

         //-------------------------------------------------
         // Obter o texto do rodapé
         //-------------------------------------------------
         EXEC SQL PREPARE STMTC2 FROM :WSqlStmt;
         EXEC SQL DECLARE C2 CURSOR FOR STMTC2;
         EXEC SQL OPEN C2;
         IF (sqlcod = 0);
            EXEC SQL FETCH NEXT FROM C2 FOR :WMaxRec ROWS INTO: WRegisto;
         ENDIF;
         EXEC SQL CLOSE C2;

         WIdx = 1;
         DOW (WRegisto(WIdx).WDsOpera <> *BLANKS);

            //-----------------------------------
            // Trabalhar valores reservados
            //-----------------------------------
            IF (%SUBST(WRegisto(WIdx).WDsOpera:1:1) = '*');
            // EXSR getRervado;
            ENDIF;

            //-----------------------------------
            // Trabalhar Excepções
            //-----------------------------------
            WIcAutoriz=$ValAccess(AppName:Function:
                                     WRegisto(WIdx).WNuOption:'O');
            //----------------------------------------
            // Escrever teclas a que está autorizado
            //----------------------------------------
            WNuOption = WRegisto(WIdx).WNuOption;
            IF ((WIcAutoriz) AND (WNuOption > *ZEROS) AND
               (%SUBST(WHide:WNuOption:1) <> '1'));
               WRegisto(WIdx).WDsOpera =
                   %TRIM(%EDITW(WRegisto(WIdx).WNuOption:'0  '))+'='+
                   %TRIM(WRegisto(WIdx).WDsOpera);
               WResultB = WResult;
               WResultB = %subst(WResult:1:WPosAct) +
                                  %trim(WRegisto(WIdx).WDsOpera);
               WResult  = WResultB;
               WPosAct = WPosAct + %len(%trim(WRegisto(WIdx).WDsOpera)) + 2;
            ENDIF;
            WIdx += 1;
         ENDDO;

         //------------------------------------------------
         // Partir correctamente o texto
         //------------------------------------------------
         $SetTexto(WResult:76);
         WResult = $GetFirstLine();
         WResult = %SUBST(WResult:1:76)+$GetNextLine();
         RETURN WResult;
      /END-FREE
     P $GetOption      E

     C*=======================================================
     C* Processo..: $KeysToOmit
     C* Objectivo.: Indentifica as teclas a omitir na sessão
     C*=======================================================
     P $KeysToOmit     B                   EXPORT
     D                 PI
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D  Hide                         24A   CONST
     D
     D*=======================================================
      /FREE
         GetHideSett_(AppName:Function:'K':*ON:Hide);
      /END-FREE
     P $KeysToOmit     E

     C*=======================================================
     C* Processo..: $GetFooter
     C* Objectivo.: Disponibilizar o Titulo Para a Função
     C*=======================================================
     P $GetFooter      B                   EXPORT
     D                 PI           156A
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D  Hide                         24A   CONST Options(*NOPASS)
     D
     D WFuncKeyDesc    DS           156
     D   WDescLine1            1     78A
     D   WDescLine2           79    156A
     D   WResult       S            156A
     D   WResultb      S            156A
     D   WPosAct       S              5S 0 INZ(*ZEROS)
     D   WIcAutoriz    S               N
     D
     D WRegisto        DS                  QUALIFIED DIM(KTEMaxRec)
     D   WDsOpera                    20A
     D   WDfAccess                    1A
     D   WCdTecla                     2S 0
     D WSqlStmt        S           1000A
     D WIdx            S              5S 0
     D WHide           S             24A
     D*======================================================================
      /FREE
         WNomAppl     = AppName;
         WNomFunction = Function;
         WHide        = GetHideSett_(WNomAppl:WNomFunction:'K');

         //---------------------------------------------------
         // Respeitar a hierarquia de autorizações
         //---------------------------------------------------
         IF (%PARMS() > 2);
            WIdx  = 1;
            DOW (WIdx < 24);
               IF (%SUBST(WHide:Widx:1) = '1');
                  %SUBST(WHide:WIdx:1) = '1';
               ELSE;
                  %SUBST(Whide:Widx:1) = %SUBST(Hide:WIdx:1);
               ENDIF;
               WIdx += 1;
            ENDDO;
         ENDIF;

         WSqlStmt = 'SELECT  DSOPERA, DFACCESS, CDTECLA from fsecp010 +
                     INNER JOIN (fsecp011 INNER JOIN fsecp012 ON +
                     (fsecp011.cdprogr = fsecp012.cdprogr) and +
                     (fsecp011.cdfunct = fsecp012.cdfunct)) +
                     ON (fsecp010.cdprogr = fsecp011.cdprogr) WHERE +
                     (fsecp010.NmProgr = ''' + %TRIM(AppName) +
                     ''') AND (fsecp011.NMFunct = ''' + %TRIM(Function) +
                     ''') AND (fsecp012.cdTecla > 0)';

         //-------------------------------------------------
         // Obter o texto do rodapé
         //-------------------------------------------------
         EXEC SQL PREPARE STMTC3 FROM :WSqlStmt;
         EXEC SQL DECLARE C3 CURSOR FOR STMTC3;
         EXEC SQL OPEN C3;
         IF (sqlcod = 0);
            EXEC SQL FETCH NEXT FROM C3 FOR :WMaxRec ROWS INTO: WRegisto;
         ENDIF;
         EXEC SQL CLOSE C3;

         WIdx = 1;
         DOW (WRegisto(WIdx).WDsOpera <> *BLANKS);

            //-----------------------------------
            // Trabalhar valores reservados
            //-----------------------------------
            IF (%SUBST(WRegisto(WIdx).WDsOpera:1:1) = '*');
            // EXSR getRervado;
            ENDIF;

            //-----------------------------------
            // Trabalhar Excepções
            //-----------------------------------
            WIcAutoriz=$ValAccess(AppName:Function:
                                     WRegisto(WIdx).WCdTecla:'T');

            //----------------------------------------
            // Escrever teclas a que está autorizado
            //----------------------------------------
            IF ((WIcAutoriz) AND (WRegisto(WIdx).WCdTecla < 25)
                AND (WRegisto(WIdx).WCdTecla > *ZEROS));
               IF (%SUBST(WHide:WRegisto(WIdx).WCdTecla:1) <> '1');
                  WRegisto(WIdx).WDsOpera = 'F' +
                      %TRIM(%EDITW(WRegisto(WIdx).WCdTecla:'0  '))+'='+
                      %TRIM(WRegisto(WIdx).WDsOpera);
                  WResultB = WResult;
                  WResultB = %subst(WResult:1:WPosAct) +
                      %TRIM(WRegisto(WIdx).WDsOpera);
                  WResult = WResultB;
                  WPosAct = WPosAct+%len(%trim(WRegisto(WIdx).WDsOpera))+2;
               ENDIF;
            ENDIF;
            WIdx += 1;
         ENDDO;

         //------------------------------------------------
         // Partir correctamente o texto
         //------------------------------------------------
         $SetTexto(WResult:78);
         WResult = $GetFirstLine();
         WResult = %SUBST(WResult:1:78)+$GetNextLine();
         RETURN WResult;
      /END-FREE
     P $GetFooter      E

     C*=======================================================
     C* Processo..: $ValAccess
     C* Objectivo.: Validar o acesso a uma operação
     C*=======================================================
     P $ValAccess      B                   EXPORT
     D                 PI              N
     D  AppName                      10A   CONST
     D  Function                     10A   CONST Options(*NOPASS)
     D  InVal                         5S 0 CONST Options(*NOPASS)
     D  TipOption                     1A   CONST Options(*NOPASS)
     D* IPValue                      15A   CONST Options(*NOPASS)
     D
     D WDfAccess       S              1A
     D WResult         S               N
     D WSqlStmt        S           1000A
     D WValDef         S              5A
     D WFunction       S             10A   INZ(*BLANKS)
     D WInVal          S              5S 0 INZ(99999)
     D WTipOption      S              1A
     D WCdUser         S             10A
     D WIcSair         S               N
     D WIPVal          S             15A
      *=============================================================
      /FREE

       WTipOption = 'F'; // Testar acesso à função
       WFunction  = *BLANKS;
       WInVal     = 99999;
       WValDef    = '99999';
       //---------------------------------------------------
       // Se o utilizador for HTTPSVR então obter o IP
       //---------------------------------------------------
       WCdUser = S#USR;
       IF (S#USR = 'QTMHHTTP');
          MONITOR;
             WIPVal = %str($getenv('REMOTE_ADDR'));
             CHAIN WIPVal RSECP03101;
             IF (%FOUND());
                WCdUser = $StrToUpper(SEC_CDUSER);
             ELSE;
                RETURN *OFF;
             ENDIF;
          ON-ERROR;
             RETURN *OFF;
          ENDMON;
       ENDIF;

       IF (%PARMS() > 1);
          WFunction = Function;
          IF (%PARMS() > 2);
             WInVal = InVal;
             IF (%PARMS() > 3);
                WTipOption = TipOption;
             ENDIF;
          ENDIF;
       ENDIF;

       WIcSair = *OFF;
       DOW NOT WIcSair;
          EXSR MakeSQLStmt;
          EXEC SQL PREPARE STMTC5 FROM :WSqlStmt;
          EXEC SQL DECLARE C5 CURSOR FOR STMTC5;
          EXEC SQL OPEN C5;
          IF (sqlcod = 0);
             EXEC SQL FETCH C5 INTO: WDfAccess;
          ENDIF;
          EXEC SQL CLOSE C5;
          IF (WDfAccess = *BLANKS) AND (WCdUser <> *BLANKS);
             WCdUser = *BLANKS;
             ITER;
          ENDIF;
          WIcSair = *ON;
       ENDDO;

       //---------------------------------------------------
       // ForCompr valor por defeito
       //---------------------------------------------------
       IF (WDfAccess = 'A');
          WResult = *ON;
       ELSE;
          WResult = *OFF;
       ENDIF;

       RETURN WResult;
       //========================================================
       // Subrotina.: MakeSQLStmt
       // Objectivo.: Construir a instrução de SQL
       //========================================================
       BEGSR MakeSQLStmt;
       //--------------------------------------------------------------
       // Obter o tipo de acesso para o utilizador
       //--------------------------------------------------------------
       IF (WCdUser <> *BLANKS);
          WSqlStmt = 'Select fsecp030.TPACCES from fsecp010 inner join +
                     (fsecp011 inner join +
                       (fsecp012 inner join fsecp030 +
                         on (fsecp012.cdprogr = fsecp030.cdprogr +
                             and fsecp012.cdfunct = fsecp030.cdfunct +
                             and fsecp012.cdopera = fsecp030.cdopera))';
       ELSE;
          WSqlStmt = 'Select fsecp012.DFACCESS from fsecp010 inner join +
                     (fsecp011 inner join fsecp012';
       ENDIF;
       WSqlStmt = %TRIM(WSqlStmt) +
                       ' on (fsecp011.cdprogr = fsecp012.cdprogr +
                             and fsecp011.cdfunct = fsecp012.cdfunct)) +
                         on (fsecp010.cdprogr = fsecp011.cdprogr) +
              WHERE (fsecp010.NmProgr = ''' + %TRIM(AppName) + ''') AND ';
       IF (WCdUser <> *BLANKS);
          WSqlStmt = %TRIM(WSqlStmt) +
                    ' (fsecp030.cduser = ''' +%TRIM(WCdUser)+ ''') AND';
       ENDIF;

       //---------------------------------------------------
       // Definir a profundidade da Função
       //---------------------------------------------------
       IF (%trim(WFunction) = '');
          WSqlStmt= %TRIM(WSqlStmt) +
            ' (fsecp011.CdFunct= 99999) +
                AND (fsecp012.cdopera = 99999)';
       ELSE;
          WSqlStmt= %TRIM(WSqlStmt) +
             ' (fsecp011.NMFunct = ''' + %TRIM(WFunction) + ''') AND';
          IF (WTipOption = 'F');
             WSqlStmt= %TRIM(WSqlStmt)+
                ' (fsecp012.cdopera = 99999)';
          ELSEIF (WTipOption = 'T');
             WSqlStmt= %TRIM(WSqlStmt)+
                ' (fsecp012.cdtecla = '+%CHAR(WInVal)+')';
          ELSEIF (WTipOption = 'O');
             WSqlStmt= %TRIM(WSqlStmt)+
                ' (fsecp012.nuoption = '+%CHAR(WInVal)+')';
          ENDIF;
       ENDIF;
       ENDSR;
      /END-FREE
     P $ValAccess      E

     C*=======================================================
     C* Processo..: $GetFunction
     C* Objectivo.: Devolve o nome do programa
     C*=======================================================
     P $GetFunction    B                   EXPORT
     D                 PI            10A
     D  NoFunction                    5S 0 CONST
     D
     D WSqlStmt        S           1000A
     D WResult         S             10A   INZ(*BLANKS)
      /FREE
         IF (NoFunction > *ZEROS);
            WSqlStmt = 'SELECT NMPROGR FROM FSECP010 WHERE  +
                           CDPROGR = ' + %CHAR(NoFunction);
            EXEC SQL PREPARE STMTC6 FROM :WSqlStmt;
            EXEC SQL DECLARE C6 CURSOR FOR STMTC6;
            EXEC SQL OPEN C6;
            IF (sqlcod = 0);
               EXEC SQL FETCH C6 INTO: WResult;
            ENDIF;
            EXEC SQL CLOSE C6;
         ENDIF;

         RETURN WResult;
      /END-FREE
     P $GetFunction    E

     C*=======================================================
     C* Processo..: $ValidKey
     C* Objectivo.: Validar a Tecla pressionada
     C*=======================================================
     P $ValidKey       B                   EXPORT
     D                 PI              N
     D  PressedKey                    1A   CONST
     D  ValidKey                      1A   CONST
     D  Valor                        50A   CONST Options(*NOPASS)
     D  AppName                      10A   CONST Options(*NOPASS)
     D  Function                     10A   CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WAppName        S             10A
     D WFunction       S             10A
     D WNumOption      S              2S 0 INZ(*ZEROS)
     D WValor          S             50A
     D WIcAutoriza     S               N
     D WHide           S             24A
     D*==========================================================
      /FREE
        IF (%PARMS() > 2);
           WValor = Valor;
           IF (%PARMS() > 3);
              WAppName = AppName;
              IF (%PARMS() > 4);
                 WFunction = Function;
              ELSE;
                 WFunction = WNomFunction;
              ENDIF;
           ENDIF;
        ELSE;
           WAppName  = WNomAppl;
           WFunction = WNomFunction;
           WValor    = *BLANKS;
        ENDIF;

        IF (PressedKey = ValidKey);
           IF (PressedKey = KEY#F01);
              WNumOption = 1;
           ELSEIF (PressedKey = KEY#F02);
              WNumOption = 2;
           ELSEIF (PressedKey = KEY#F03);
              WNumOption = 3;
           ELSEIF (PressedKey = KEY#F04);
              WNumOption = 4;
           ELSEIF (PressedKey = KEY#F05);
              WNumOption = 5;
           ELSEIF (PressedKey = KEY#F06);
              WNumOption = 6;
           ELSEIF (PressedKey = KEY#F07);
              WNumOption = 7;
           ELSEIF (PressedKey = KEY#F08);
              WNumOption = 8;
           ELSEIF (PressedKey = KEY#F09);
              WNumOption = 9;
           ELSEIF (PressedKey = KEY#F10);
              WNumOption = 10;
           ELSEIF (PressedKey = KEY#F11);
              WNumOption = 11;
           ELSEIF (PressedKey = KEY#F12);
              WNumOption = 12;
           ELSEIF (PressedKey = KEY#F13);
              WNumOption = 13;
           ELSEIF (PressedKey = KEY#F14);
              WNumOption = 14;
           ELSEIF (PressedKey = KEY#F15);
              WNumOption = 15;
           ELSEIF (PressedKey = KEY#F16);
              WNumOption = 16;
           ELSEIF (PressedKey = KEY#F17);
              WNumOption = 17;
           ELSEIF (PressedKey = KEY#F18);
              WNumOption = 18;
           ELSEIF (PressedKey = KEY#F19);
              WNumOption = 19;
           ELSEIF (PressedKey = KEY#F20);
              WNumOption = 20;
           ELSEIF (PressedKey = KEY#F21);
              WNumOption = 21;
           ELSEIF (PressedKey = KEY#F22);
              WNumOption = 22;
           ELSEIF (PressedKey = KEY#F23);
              WNumOption = 23;
           ELSEIF (PressedKey = KEY#F24);
              WNumOption = 24;
           ELSEIF (PressedKey = KEY#Enter);
              WNumOption = 30;
           ELSEIF (PressedKey = KEY#Help);
              WNumOption = 40;
           ELSEIF (PressedKey = KEY#PgUp);
              WNumOption = 50;
           ELSEIF (PressedKey = KEY#PgDown);
              WNumOption = 51;
           ELSEIF (PressedKey = KEY#Print);
              WNumOption = 60;
           ENDIF;

           IF (WNumOption = *ZEROS);
              WIcAutoriza= *OFF;
           ELSEIF ((WNumOption >= 1) AND (WNumOption <= 24));
              WHide = GetHideSett_(WnomAppl:WNomFunction:'K');
              IF (%SUBST(WHide:WNumOption:1) = '1');
                 WIcAutoriza = *OFF;
              ELSE;
                 WIcAutoriza= $ValAccess(WNomAppl:WFunction:WNumOption:'T');
                 IF (WIcAutoriza AND WValor <> *BLANKS);
                    MONITOR;
                    ValidSpecial_(WnomAppl:WFunction:WNumOption:'F':
                                 WValor:WIcAutoriza);
                    ON-ERROR;
                       WIcAutoriza = *OFF;
                    ENDMON;
                 ENDIF;
              ENDIF;
           ELSE;
              WIcAutoriza= *ON;
           ENDIF;
        ENDIF;

        RETURN WIcAutoriza;
      /END-FREE
     P $ValidKey       E

     C*=======================================================
     C* Processo..: $ValidOption
     C* Objectivo.: Validar se a opção está correcta
     C*=======================================================
     P $ValidOption    B                   EXPORT
     D                 PI              N
     D  Selected                      2A   CONST
     D  Option                        2A   CONST
     D  Valor                        50A   CONST Options(*NOPASS)
     D  AppName                      10A   CONST Options(*NOPASS)
     D  Function                     10A   CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WAppName        S             10A
     D WFunction       S             10A
     D WValor          S             50A
     D WNumOption      S              2S 0 INZ(*ZEROS)
     D WSelected       S              2S 0 INZ(*ZEROS)
     D WIcAutoriza     S               N
     D WHide           S             24A
     D*==========================================================
      /FREE
        IF (%PARMS() > 2);
           WValor = Valor;
           IF (%PARMS() > 3);
              WAppName = AppName;
              IF (%PARMS() > 4);
                 WFunction = Function;
              ELSE;
                 WFunction = WNomFunction;
              ENDIF;
           ENDIF;
        ELSE;
           WValor    = *BLANKS;
           WAppName  = WNomAppl;
           WFunction = WNomFunction;
        ENDIF;

        IF (Selected = *BLANKS);
           WIcAutoriza = *OFF;
        ELSE;
           MONITOR;
              WSelected = %DEC(%TRIM(Selected):2:0);
              WNumOption = %DEC(%TRIM(Option):2:0);
           ON-ERROR;
              WIcAutoriza = *OFF;
              RETURN *OFF;
           ENDMON;
           WHide = GetHideSett_(WnomAppl:WNomFunction:'O');
           IF (%SUBST(WHide:WNumOption:1) = '1');
              WIcAutoriza = *OFF;
           ELSEIF (WSelected = WNumOption);
              WIcAutoriza=$ValAccess(WNomAppl:WFunction:WNumOption:'O');
              IF (WIcAutoriza AND WValor <> *BLANKS);
                 MONITOR;
                 ValidSpecial_(WnomAppl:WFunction:WNumOption:'O':
                                 WValor:WIcAutoriza);
                 ON-ERROR;
                    WIcAutoriza = *OFF;
                 ENDMON;
              ENDIF;
           ELSE;
              WIcAutoriza = *OFF;
           ENDIF;
        ENDIF;

        RETURN WIcAutoriza;
      /END-FREE
     P $ValidOption    E
     C*=======================================================
     C* Processo..: $GetDescFunct
     C* Objectivo.: Obter a descrição para o Componente
     C*=======================================================
     P $GetDescFunct   B                   EXPORT
     D                 PI            50A
     D    RCodigo                     5S 0 CONST
     D
     D WSqlStmt        S           1000A
     D WResult         S             50A   INZ(*BLANKS)
      /FREE
         WSqlStmt = 'SELECT DSPROGR FROM FSECP010 WHERE  +
                        CDPROGR = ' + %CHAR(RCODIGO);
         EXEC SQL PREPARE STMTC7 FROM :WSqlStmt;
         EXEC SQL DECLARE C7 CURSOR FOR STMTC7;
         EXEC SQL OPEN C7;
         IF (sqlcod = 0);
            EXEC SQL FETCH C7 INTO: WResult;
         ENDIF;
         EXEC SQL CLOSE C7;

         RETURN WResult;
      /END-FREE
     P $GetDescFunct   E
     C*=======================================================
     C* Processo..: $ValTemplate
     C* Objectivo.: Vaildar se a Template é válida
     C*=======================================================
     P $ValTemplate    B                   EXPORT
     D                 PI              N
     D    RCdTmplt                    5S 0 CONST
     D    RCodigo                     5S 0 CONST
     D
     D WSqlStmt        S           1000A
     D WCdTmplt        S              5S 0
      /FREE
         WCdTmplt = *ZEROS;
         WSqlStmt = 'SELECT CDTMPLT FROM FSECP010 WHERE  +
                        CDPROGR = ' + %CHAR(RCODIGO);
         EXEC SQL PREPARE STMTC08 FROM :WSqlStmt;
         EXEC SQL DECLARE C08 CURSOR FOR STMTC08;
         EXEC SQL OPEN C08;
         IF (sqlcod = 0);
            EXEC SQL FETCH C08 INTO: WCdTmplt;
         ENDIF;
         EXEC SQL CLOSE C08;

         IF (WCdTmplt = *ZEROS) OR (WCdTmplt <> RCdTmplt);
            RETURN *OFF;
         ELSE;
            RETURN *ON;
         ENDIF;
      /END-FREE
     P $ValTemplate    E
