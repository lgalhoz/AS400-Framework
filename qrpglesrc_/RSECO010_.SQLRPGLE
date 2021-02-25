     H BNDDIR('JMDIR')
      *=======================================================
      * Dat.Cria...: 2011-05-12
      * Autor......: Luis Galhoz
      * Objectivo..: Devolver o Header da função, junto com as
      *              teclas a que o utilizador está autorizado.
      *=======================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SSECO010
     D
     D WDfAccess       S              1A
     D WDsOpera        S             60A
     D WDescCod        S             20A
     D WNomAppl        S             10A
     D WNomFunction    S             10A
      *=======================================================
      /FREE
         WDsOpera = $GetFooter('PTBLP010':'RTBLE01003':*BLANKS);
         WDescCod = 'x';

         *INLR = *ON;
         RETURN;
      /END-FREE
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
     C
      /FREE
         //------------------------------------------
         // Obter a descrição para o titulo da Função
         //------------------------------------------
         WNomAppl     = AppName;
         WNomFunction = Function;
         WResult ='Titulo não definido';
         EXEC SQL
            DECLARE C1 CURSOR FOR
            select dsprogr, dsfunct from fsecp010 inner join fsecp011 on
            (fsecp010.cdprogr = fsecp011.cdprogr) where fsecp010.nmprogr =
            :AppName  and fsecp011.nmfunct = :Function
            FOR FETCH ONLY;
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
     C* Processo..: $GetOption
     C* Objectivo.: Disponibilizar a Barra de Opções
     C*=======================================================
     P $GetOption      B                   EXPORT
     D                 PI           152A
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D  Hide                         24A   CONST
     D
     D WFuncKeyDesc    DS           156
     D   WDescLine1            1     78A
     D   WDescLine2           79    156A
     D   WResult       S            156A
     D   WResultb      S            156A
     D   WPosAct       S              5S 0 INZ(*ZEROS)
     D   WIcAutoriz    S               N
     D
     D WRegisto        DS
     D   WDsOpera                    20A
     D   WDfAccess                    1A
     D   WVlIndica                    5S 0
     C
      /FREE
         WNomAppl     = AppName;
         WNomFunction = Function;
         //-------------------------------------------------
         // Obter o texto do rodapé
         //-------------------------------------------------
         EXEC SQL
            DECLARE C_OPT CURSOR FOR
               select DSOPERA, DFACCESS, VLINDICA from fsecp010 inner
               join (fsecp011 inner join fsecp012 on
               (fsecp011.cdprogr = fsecp012.cdprogr) and
               (fsecp011.cdfunct = fsecp012.cdfunct))
               on (fsecp010.cdprogr = fsecp011.cdprogr) where
               (fsecp010.NmProgr = :AppName) AND (fsecp011.NMFunct
               = :Function) AND (fsecp012.cdTipo = 'O')
            FOR FETCH ONLY;
          EXEC SQL OPEN C_OPT;
          DOW (sqlcod = 0);
             EXEC SQL FETCH C_OPT INTO: WRegisto;
             if (sqlcod <> 0);
                leave;
             endif;

             //-----------------------------------
             // Trabalhar valores reservados
             //-----------------------------------
             IF (%SUBST(WDsOpera:1:1) = '*');
             // EXSR getRervado;
             ENDIF;

             //-----------------------------------
             // Trabalhar Excepções
             //-----------------------------------
             WIcAutoriz = $ValAccess(AppName:Function:WVlIndica);

             //----------------------------------------
             // Escrever teclas a que está autorizado
             //----------------------------------------
             IF ((WIcAutoriz) AND (WVlIndica > 24));
                WResultB = WResult;
                WResultB = %subst(WResult:1:WPosAct) + %trim(WDsOpera);
                WResult  = WResultB;
                WPosAct = WPosAct + %len(%trim(WDsOpera)) + 2;
             ENDIF;
          ENDDO;
          EXEC SQL CLOSE C_OPT;
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
     C* Processo..: $GetFooter
     C* Objectivo.: Disponibilizar o Titulo Para a Função
     C*=======================================================
     P $GetFooter      B                   EXPORT
     D                 PI           156A
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D  Hide                         24A   CONST
     D
     D WFuncKeyDesc    DS           156
     D   WDescLine1            1     78A
     D   WDescLine2           79    156A
     D   WResult       S            156A
     D   WResultb      S            156A
     D   WPosAct       S              5S 0 INZ(*ZEROS)
     D   WIcAutoriz    S               N
     D
     D WRegisto        DS
     D   WDsOpera                    20A
     D   WDfAccess                    1A
     D   WVlIndica                    5S 0
     C
      /FREE
         WNomAppl     = AppName;
         WNomFunction = Function;
         //-------------------------------------------------
         // Obter o texto do rodapé
         //-------------------------------------------------
         EXEC SQL
            DECLARE C2 CURSOR FOR
               select DSOPERA, DFACCESS, VLINDICA from fsecp010 inner
               join (fsecp011 inner join fsecp012 on
               (fsecp011.cdprogr = fsecp012.cdprogr) and
               (fsecp011.cdfunct = fsecp012.cdfunct))
               on (fsecp010.cdprogr = fsecp011.cdprogr) where
               (fsecp010.NmProgr = :AppName) AND (fsecp011.NMFunct
               = :Function) AND (fsecp012.cdTipo = 'T')
            FOR FETCH ONLY;
          EXEC SQL OPEN C2;
          DOW (sqlcod = 0);
             EXEC SQL FETCH C2 INTO: WRegisto;
             if (sqlcod <> 0);
                leave;
             endif;

             //-----------------------------------
             // Trabalhar valores reservados
             //-----------------------------------
             IF (%SUBST(WDsOpera:1:1) = '*');
             // EXSR getRervado;
             ENDIF;

             //-----------------------------------
             // Trabalhar Excepções
             //-----------------------------------
             WIcAutoriz = $ValAccess(AppName:Function:WVlIndica);

             //----------------------------------------
             // Escrever teclas a que está autorizado
             //----------------------------------------
             IF ((WIcAutoriz) AND (WVlIndica < 25) AND (WVlIndica > *ZEROS));
                IF (%SUBST(HIDE:WVlIndica:1) <> '1');
                   WResultB = WResult;
                   WResultB = %subst(WResult:1:WPosAct) + %trim(WDsOpera);
                   WResult  = WResultB;
                   WPosAct = WPosAct + %len(%trim(WDsOpera)) + 2;
                ENDIF;
             ENDIF;
          ENDDO;
          EXEC SQL CLOSE C2;
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
     D
     D WDfAccess       S              1A
     D WResult         S               N
     D WSqlStmt        S           1000A
     D WValDef         S              5A
     D WFunction       S             10A   INZ(*BLANKS)
     D WInVal          S              5S 0 INZ(99999)
      /FREE

       IF (%PARMS() > 1);
          WFunction = Function;
          IF (%PARMS() > 2);
             WInVal = InVal;
          ELSE;
             WInVal = 99999;
          ENDIF;
       ELSE;
          WFunction = *BLANKS;
          WInVal = 99999;
       ENDIF;

       //---------------------------------------------------
       // Obter o tipo de acesso por defeito para a função
       //---------------------------------------------------
       WDfAccess = 'R';
       IF (%trim(WFunction) = '');
          WValDef = '99999';
          WSqlStmt = 'select fsecp012.DFACCESS from fsecp010  +
                     inner join (fsecp011 inner join fsecp012 on  +
                     (fsecp011.cdprogr = fsecp012.cdprogr) and  +
                     (fsecp011.cdfunct = fsecp012.cdfunct))  +
                     on (fsecp010.cdprogr = fsecp011.cdprogr) where  +
                     (fsecp010.NmProgr = ''' + %TRIM(AppName) + ''')  +
                     AND (fsecp011.CdFunct = ' + WValDef + ') +
                     AND (fsecp012.CDOPERA = ' + WValDef + ')';
       ELSE;
          WSqlStmt = 'select fsecp012.DFACCESS from fsecp010 +
                      inner join (fsecp011 inner join fsecp012 on +
                      (fsecp011.cdprogr = fsecp012.cdprogr) and +
                      (fsecp011.cdfunct = fsecp012.cdfunct)) +
                      on (fsecp010.cdprogr = fsecp011.cdprogr) where +
                    (fsecp010.NmProgr = '''+ %TRIM(AppName) + ''') AND +
                    (fsecp011.NmFunct  = ''' + %TRIM(WFunction) + ''') +
                      AND (fsecp012.VLINDICA = ' + %CHAR(WInVal) + ')';
       ENDIF;
       EXEC SQL PREPARE STMTC3 FROM :WSqlStmt;
       EXEC SQL DECLARE C3 CURSOR FOR STMTC3;
       EXEC SQL OPEN C3;
       IF (sqlcod = 0);
          EXEC SQL FETCH C3 INTO: WDfAccess;
       ENDIF;
       EXEC SQL CLOSE C3;

       //---------------------------------------------------
       // Obter o tipo de acesso para o utilizador
       //---------------------------------------------------
       IF (%trim(WFunction) = '');
          WValDef = '99999';
          WSqlStmt = 'Select fsecp030.TPACCES +
              from fsecp010 inner join +
              (fsecp011 inner join (fsecp012 inner join +
              (fsecp030 inner join fsecp020 on (fsecp030.cduser +
              = fsecp020.cduser)) on (fsecp012.cdprogr = +
              fsecp030.cdprogr and fsecp012.cdfunct = fsecp030.cdfunct +
              and fsecp012.cdopera = fsecp030.cdopera)) on (fsecp011.cdprogr = +
              fsecp012.cdprogr and fsecp011.cdfunct = fsecp012.cdfunct)) +
              on (fsecp010.cdprogr = fsecp011.cdprogr) where +
               (fsecp010.NmProgr = ''' + %TRIM(AppName) + ''') AND +
               (fsecp011.CdFunct = ' + WvalDef + ') AND +
               (fsecp012.cdopera = ' +WValDef+') AND +
              (fsecp020.cduser = ''' + %TRIM(S#USR) + ''')';
       ELSE;
          WSqlStmt = 'Select fsecp030.TPACCES +
              from fsecp010 inner join +
              (fsecp011 inner join (fsecp012 inner join +
              (fsecp030 inner join fsecp020 on (fsecp030.cduser +
              = fsecp020.cduser)) on (fsecp012.cdprogr = +
              fsecp030.cdprogr and fsecp012.cdfunct = fsecp030.cdfunct +
              and fsecp012.cdopera = fsecp030.cdopera)) on (fsecp011.cdprogr = +
              fsecp012.cdprogr and fsecp011.cdfunct = fsecp012.cdfunct)) +
              on (fsecp010.cdprogr = fsecp011.cdprogr) where +
              (fsecp010.NmProgr = ''' + %TRIM(AppName) + ''') AND +
              (fsecp011.NMFunct = ''' + %TRIM(WFunction) + ''') AND +
              (fsecp012.vlindica = ' + %CHAR(WInVal) + ') AND +
              (fsecp020.cduser = ''' + %TRIM(S#USR) + ''')';
       ENDIF;

       EXEC SQL PREPARE STMTC4 FROM :WSqlStmt;
       EXEC SQL DECLARE C4 CURSOR FOR STMTC4;
       EXEC SQL OPEN C4;
       IF (sqlcod = 0);
          EXEC SQL FETCH C4 INTO: WDfAccess;
       ENDIF;
       EXEC SQL CLOSE C4;

       //---------------------------------------------------
       // Formatar valor por defeito
       //---------------------------------------------------
       IF (WDfAccess = 'A');
          WResult = *ON;
       ELSE;
          WResult = *OFF;
       ENDIF;

       RETURN WResult;
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
            EXEC SQL PREPARE STMTC5 FROM :WSqlStmt;
            EXEC SQL DECLARE C5 CURSOR FOR STMTC5;
            EXEC SQL OPEN C5;
            IF (sqlcod = 0);
               EXEC SQL FETCH C5 INTO: WResult;
            ENDIF;
            EXEC SQL CLOSE C5;
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
     D  AppName                      10A   CONST Options(*NOPASS)
     D  Function                     10A   CONST Options(*NOPASS)
     D
     D WAppName        S             10A
     D WFunction       S             10A
     D WNumOption      S              2S 0 INZ(*ZEROS)
      /FREE
        IF (%PARMS() > 2);
           WAppName = AppName;
           IF (%PARMS() > 3);
              WFunction = Function;
           ELSE;
              WFunction = WNomFunction;
           ENDIF;
        ELSE;
           WAppName  = WNomAppl;
           WFunction = WNomFunction;
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
              RETURN *OFF;
           ELSEIF ((WNumOption >= 1) AND (WNumOption <= 24));
              RETURN $ValAccess(WNomAppl:WNomFunction:WNumOption);
           ELSE;
              RETURN *ON;
           ENDIF;
        ELSE;
           RETURN *OFF;
        ENDIF;
      /END-FREE
     P $ValidKey       E

     C*=======================================================
     C* Processo..: $ValidOption
     C* Objectivo.: Validar se a opção está correcta
     C*=======================================================
     P $ValidOption    B                   EXPORT
     D                 PI              N
     D  Selected                      2S 0 CONST
     D  Option                        2S 0 CONST
     D  AppName                      10A   CONST Options(*NOPASS)
     D  Function                     10A   CONST Options(*NOPASS)
     D
     D WAppName        S             10A
     D WFunction       S             10A
     D WNumOption      S              2S 0 INZ(*ZEROS)
      /FREE
        IF (%PARMS() > 2);
           WAppName = AppName;
           IF (%PARMS() > 3);
              WFunction = Function;
           ELSE;
              WFunction = WNomFunction;
           ENDIF;
        ELSE;
           WAppName  = WNomAppl;
           WFunction = WNomFunction;
        ENDIF;

        IF (Selected = *ZEROS);
           RETURN *OFF;
        ELSE;
           RETURN $ValAccess(WNomAppl:WNomFunction:Option);
        ENDIF;
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
         EXEC SQL PREPARE STMTC8 FROM :WSqlStmt;
         EXEC SQL DECLARE C8 CURSOR FOR STMTC8;
         EXEC SQL OPEN C8;
         IF (sqlcod = 0);
            EXEC SQL FETCH C8 INTO: WResult;
         ENDIF;
         EXEC SQL CLOSE C8;

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
         EXEC SQL PREPARE STMTC09 FROM :WSqlStmt;
         EXEC SQL DECLARE C09 CURSOR FOR STMTC09;
         EXEC SQL OPEN C09;
         IF (sqlcod = 0);
            EXEC SQL FETCH C09 INTO: WCdTmplt;
         ENDIF;
         EXEC SQL CLOSE C09;

         IF (WCdTmplt = *ZEROS) OR (WCdTmplt <> RCdTmplt);
            RETURN *OFF;
         ELSE;
            RETURN *ON;
         ENDIF;
      /END-FREE
     P $ValTemplate    E
