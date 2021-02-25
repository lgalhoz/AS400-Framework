     H NOMAIN
     H*=========================================================
     H* Modulo....: RDBSO013
     H* Descrição.: Obter informação sobreo ficheiro de dados.
     H*
     H*=========================================================
     FQADSPFFD  IF   E           K DISK    USROPN
     F*=========================================================
      /Copy QRPGLESRC,SDBSO010

     D*==> Extrair Campos usados no ficheiro
     D DSPFFD          PR             3S 0
     D   PRM_File                    10A   Const
     D   PRM_Lib                     10A   Const Options(*NOPASS)
     D   PRM_Prefix                  10A   Const Options(*NOPASS)
     D
     D*==> Lista de Campos do Ficheiro
     D Lst#File        DS                  QUALIFIED DIM(25)
     D   FileName                    10A   INZ(*BLANKS)
     D   LibName                     10A   INZ('*LIBL')
     D   Prefix                      10A   INZ(*BLANKS)
     D   F#LST                             LIKEDS(Lst#Fields) DIM(300)
     D   IsEmpty                       N   INZ(*ON)
     D WIdxFile        S              3S 0 INZ(*ZEROS)
     D WIdxField       S              3S 0 INZ(*ZEROS)
     D WIdxRec         S              3S 0 INZ(*ZEROS)
     D
     D*==> Lista de Campos
     D Lst#Fields      DS                  QUALIFIED
     D   Nome                              LIKE(WHFLDI)
     D   Tipo                              LIKE(WHFLDT)
     D   Dimensao                          LIKE(WHFLDB)
     D   Decimais                          LIKE(WHFLDP)
     D   Descricao                         LIKE(WHFTXT)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                    3000A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D*================================================================

     P*=========================================================
     P* Processo..: DSPFFD
     P* Objectivo.: Extrairr a definição dos Campos
     P*=========================================================
     P DSPFFD          B                   EXPORT
     D                 PI             3S 0
     D   PRM_File                    10A   Const
     D   PRM_Lib                     10A   Const Options(*NOPASS)
     D   PRM_Prefix                  10A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WFileName       S                   LIKE(PRM_File)
     D WLibName        S                   LIKE(PRM_Lib)
     D WPrefix         S                   LIKE(PRM_Prefix)
     D*=========================================================
      /FREE
        WFileName = PRM_File;
        WLibName  = '*LIBL';
        WPrefix = *BLANKS;
        IF (%PARMS() > 1);
           WLibName = PRM_Lib;
           IF (%PARMS() > 2);
              WPrefix = PRM_Prefix;
           ENDIF;
        ENDIF;

        //-------------------------------------------------
        // Obter indice para o ficheiro
        //-------------------------------------------------
        WIdxRec = 1;
        DOW (NOT Lst#File(WIdxRec).IsEmpty);
           IF (Lst#File(WIdxRec).FileName = WFileName) AND
                (Lst#File(WIdxRec).LibName = WLibName) AND
                (Lst#File(WIdxRec).Prefix = WPrefix);
                RETURN WIdxRec;
           ENDIF;
           WIdxRec += 1;
        ENDDO;
        WIdxFile = WIdxRec;
        Lst#File(WIdxFile).FileName = WFileName;
        Lst#File(WIdxFile).LibName  = WLibName;
        Lst#File(WIdxFile).Prefix   = WPrefix;
        Lst#File(WIdxFile).IsEmpty  = *OFF;

        //-------------------------------------------------
        // Obter a definição dos Campos do Ficheiro
        //-------------------------------------------------
        MONITOR;
           WStdCmd = 'DLTF FILE(QTEMP/QADSPFFD)';
           WStdLen = %LEN(WStdCmd);
           EXEC(WStdCmd:WStdLen);
        ON-ERROR;
        ENDMON;
        MONITOR;
           WStdCmd = 'DSPFFD FILE('+%TRIM(WLibName)+'/'+%TRIM(WFileName)+') +
                         OUTPUT(*OUTFILE) OUTFILE(QTEMP/QADSPFFD)';
           WStdLen = %LEN(WStdCmd);
           EXEC(WStdCmd:WStdLen);
        ON-ERROR;
           RETURN *ZEROS;
        ENDMON;

        //-------------------------------------------------
        // Extrair informação dos Campos
        //-------------------------------------------------
        MONITOR;
           WStdCmd = 'OVRDBF FILE(QADSPFFD) TOFILE(QTEMP/QADSPFFD)';
           WStdLen = %LEN(WStdCmd);
           EXEC(WStdCmd:WStdLen);
        ON-ERROR;
           RETURN *ZEROS;
        ENDMON;

        WIdxRec = 1;
        OPEN QADSPFFD;
        READ QADSPFFD;
        DOW NOT (%EOF());
           Lst#File(WIdxFile).F#Lst(WIdxRec).Nome      = WHFLDI;
           Lst#File(WIdxFile).F#Lst(WIdxRec).Tipo      = WHFLDT;
           Lst#File(WIdxFile).F#Lst(WIdxRec).Dimensao  = WHFLDB;
           Lst#File(WIdxFile).F#Lst(WIdxRec).Decimais  = WHFLDP;
           Lst#File(WIdxFile).F#Lst(WIdxRec).Descricao = WHFTXT;
           Lst#File(WIdxFile).F#Lst(WIdxRec).IsEmpty   = *OFF;
           WIdxRec += 1;
           READ QADSPFFD;
        ENDDO;
        CLOSE QADSPFFD;

        MONITOR;
           WStdCmd = 'DLTF FILE(QTEMP/QADSPFFD)';
           WStdLen = %LEN(WStdCmd);
           EXEC(WStdCmd:WStdLen);
        ON-ERROR;
        ENDMON;

         RETURN WIdxFile;
      /END-FREE
     P DSPFFD          E

     P*=========================================================
     P* Processo..: $getFieldList
     P* Objectivo.: Obter a lista de Campos do ficheiro
     P*=========================================================
     P $getFieldList   B                   EXPORT
     D                 PI          3000A
     D   PRM_File                    10A   Const
     D   PRM_Prefix                  10A   Const Options(*NOPASS)
     D   PRM_Lib                     10A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WPrefix         S                   LIKE(PRM_Prefix)
     D WFile           S                   LIKE(PRM_File)
     D WLib            S                   LIKE(PRM_Lib)
     D WIdFile         S              3S 0 INZ(*ZEROS)
     D WTipo           S              1A   INZ(*BLANKS)
     D WNome           S             10A   INZ(*BLANKS)
     D WResult         S           3000A   INZ(*BLANKS)
     D*=========================================================
      /FREE
        WResult = *BLANKS;
        WFile   = PRM_File;
        WPrefix = *BLANKS;
        Wlib    = '*LIBL';
        IF (%PARMS() > 1);
           WPrefix = PRM_Prefix;
           IF (WPrefix <> *BLANKS);
              WPrefix = %TRIM(WPrefix) + '.';
           ENDIF;
           IF (%PARMS() > 2);
              WLib = PRM_Lib;
           ENDIF;
        ENDIF;

        //-------------------------------------------------
        // Extrair informação dos Campos
        //-------------------------------------------------
        WIdFile = DSPFFD(WFile:WLib:WPrefix);
        IF (WIdFile > *ZEROS);
           WPrefix = Lst#File(WIdFile).Prefix;
           WIdxRec = 1;
           DOW (NOT Lst#File(WIdFile).F#Lst(WIdxRec).IsEmpty);
              WTipo = Lst#File(WIdFile).F#Lst(WIdxRec).Tipo;
              WNome = Lst#File(WIdFile).F#Lst(WIdxRec).Nome;
              IF (WNome = *BLANKS);
                 LEAVE;
              ENDIF;
              IF (WTipo='S') OR (WTipo='A') OR (WTipo='Z');
                 WResult = %TRIM(WResult)+','+%TRIM(WPrefix)+%TRIM(WNome);
              ENDIF;
              WIdxRec += 1;
           ENDDO;
        ENDIF;

        RETURN %SUBST(WResult:2);
      /END-FREE
     P $getFieldList   E

     P*=========================================================
     P* Processo..: $getFieldCtl
     P* Objectivo.: Obter Campos de Controlo
     P*              DI = Data Inicio (01)
     P*              DT = Data Termo  (02)
     P*              ST = Estado      (xx)
     P*=========================================================
     P $getFieldCtl    B                   EXPORT
     D                 PI            20A
     D   PRM_FILE                    10A   Const
     D   PRM_TIPO                     2A   Const
     D   PRM_Lib                     10A   Const Options(*NOPASS)
     D
     D Wlib            S             10A
     D WIdFile         S              3S 0
     D WNome           S             10A
     D WNomeStd        S             10A
     D WTipo           S              1A
     D WPrefix         S              1A
     D WResult         S             10A
     D*=========================================================
      /FREE
        Wlib    = '*LIBL';
        IF (%PARMS() > 2);
           WLib = PRM_Lib;
        ENDIF;

        //-------------------------------------------------
        // ForCompr nome do Campos Standard (Controlo)
        //-------------------------------------------------
        WNomeStd = %SUBST(PRM_FILE:2:3)+%SUBST(PRM_FILE:6:3);
        IF (PRM_TIPO = 'DI');
           WNomeStd = %TRIM(WNomeStd)+'01';
        ELSEIF (PRM_TIPO = 'DF');
           WNomeStd = %TRIM(WNomeStd)+'02';
        ENDIF;

        //-------------------------------------------------
        // Extrair informação dos Campos
        //-------------------------------------------------
        WIdFile = DSPFFD(PRM_FILE:WLib);
        IF (WIdFile > *ZEROS);
           WIdxRec = 1;
           DOW (NOT Lst#File(WIdFile).F#Lst(WIdxRec).IsEmpty);
              WTipo = Lst#File(WIdFile).F#Lst(WIdxRec).Tipo;
              WNome = Lst#File(WIdFile).F#Lst(WIdxRec).Nome;
              WPrefix = Lst#File(WIdFile).Prefix;
              IF (WNomeStd = WNome);
                 RETURN %TRIM(WPrefix)+%TRIM(WNome);
              ELSEIF (WNome = *BLANKS);
                 RETURN *BLANKS;
              ENDIF;
              WIdxRec += 1;
           ENDDO;
        ENDIF;
        RETURN *BLANKS;
      /END-FREE
     P $getFieldCtl    E
