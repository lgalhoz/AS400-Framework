     H NOMAIN
     H*=======================================================
     H* Dat.Cria...: 2012-03-29
     H* Autor......: Luis Galhoz
     H* Objectivo..: Funções de Leitura aos Dados da Base de Dados
     H*
     H* <Versão Estável>
     H*
     H* Descrição:
     H*    Foi alterado o modo de calculo usado para o posicionamento
     H*    dos registos.
     H*
     H*
     H*
     H*
     H*
     H*
     H*
     H*=======================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,STBLO010

     D*-----------------------------------------------------------------------
     D* Controlo de Sessões
     D*-----------------------------------------------------------------------
     D Var#Sess        DS                  QUALIFIED DIM(25)
     D
     D Lst#Data                            LIKEDS(Lst#Data) DIM(25)
     D ActRec                         9S 0
     D
     D Lst#Criteria                        LIKEDS(Lst#Criteria) DIM(25)
     D QtdCriter                      3S 0 INZ(*ZEROS)
     D
     D Lst#Fields                          LIKEDS(Lst#Fields) DIM(25)
     D QtdVirtFields                  3S 0 INZ(*ZEROS)
     D
     D Lst#Scr                             LIKEDS(Lst#Scr) DIM(10)
     D QtdFldScr                      3S 0 INZ(*ZEROS)
     D
     D Files                        300A   INZ(*BLANKS)
     D Fields                      1000A   INZ(*BLANKS)
     D Where                        300A   INZ(*BLANKS)
     D HstField                     200A   INZ(*BLANKS)
     D OrderBy                      100A   INZ(*BLANKS)
     D Repetidos                      1A   INZ('N')
     D
     D PagAct                         5S 0 INZ(*ZEROS)
     D QtdRec                         9S 0 INZ(*ZEROS)
     D MaxRec                         9S 0 INZ(*ZEROS)
     D
     D IsEOF                           N   INZ(*ON)
     D IsEmpty                         N   INZ(*ON)
     D IsActive                        N   INZ(*OFF)
     D DatSelect                       D
     D PrevSession                    5S 0 INZ(*ZEROS)
     D
     D WSessID         S              5S 0 INZ(1)
     D
     D*==> Declaração de Variáveis Globais
     D Lst#Data        DS                  QUALIFIED DIM(25)
     D   F#Rec                        9S 0
     D   F#Data                    1000A
     D   F#Fill                        N
     D
     D*==> Criterio de Pesquisa
     D Lst#Criteria    DS                  QUALIFIED
     D   Label                       60A   INZ(*BLANKS)
     D   Field                       20A   INZ(*BLANKS)
     D   Where                       50A   INZ('LK')
     D   Valor                       50A   INZ(*BLANKS)
     D   Sort                         1A   INZ('A')
     D   Empty                         N   INZ(*ON)
     D   Select                        N   INZ(*OFF)
     D
     D*==> Campos virtuais
     D Lst#Fields      DS                  QUALIFIED
     D   Label                       20A   INZ(*BLANKS)
     D   Field                       20A   INZ(*BLANKS)
     D   Valor                       30A   INZ(*BLANKS)
     D   Tipo                         1A   INZ(*BLANKS)
     D   Table                        5S 0 INZ(*ZEROS)
     D
     D*==> Estrutura da Lista de Ecrã
     D Lst#Scr         DS                  QUALIFIED
     D   Field                       20A   INZ(*BLANKS)
     D   Label                       40A   INZ(*BLANKS)
     D   Largura                      3S 0 INZ(*ZEROS)
     D   Alinhamento                  1A   INZ(*BLANKS)
     D   EdtCode                      1A   INZ(*BLANKS)
     D   Tabela                       5S 0 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Obter a posição do Campos
     D GetPosIni       PR             3S 0
     D    FldName                    15A   Const
     D
     D*==> Obter os limites do valor do campo
     D GetFieldLimit   PR                  LIKEDS(OB#Coord)
     D    FldPosis                    3S 0 Const
     D
     D*==> Coordenadas dos Limites
     D OB#Coord        DS                  QUALIFIED
     D    PIni                        5S 0
     D    PFim                        5S 0
     D
     D*==> Definção de constantes
     D PAG#Stay        C                   x'f1'
     D PAG#Up          C                   x'f4'
     D PAG#Down        C                   x'f5'
     D
     D*==============================================================

     P*=======================================================
     P* Processo..: $AddDBDate
     P* Descrição.: Adicionar data
     P*=======================================================
     P $AddDBDate      B                   EXPORT
     D                 PI
     D   Data                          D   Const
     D*===========================================================
      /FREE
         Var#Sess(WSessID).DatSelect = Data;
      /END-FREE
     P $AddDBDate      E
     P*=======================================================
     P* Processo..: $SetDBSession
     P* Descrição.: Preencher as variáveis globais com valores
     P*=======================================================
     P $SetDBSession   B                   EXPORT
     D                 PI
     D    SessionID                   5S 0 Const
     D    InzSession                   N   Const Options(*NOPASS)
     D*===========================================================
      /FREE
       IF (WSessID > 25);
          WSessID = 25;
       ELSEIF (WSessID < 1);
          WSessID = 1;
       ENDIF;

       IF (NOT Var#Sess(SessionID).IsActive);
          Var#Sess(SessionID).IsActive = *ON;
          $InzDBSession(WSessID);
       ENDIF;
       WSessID = SessionID;

       //---------------------------------------
       // Inicializar as variáveis de sessão
       //---------------------------------------
       IF (%PARMS() > 1);
          $InzDBSession(WSessID);
       ENDIF;
      /END-FREE
     P $SetDBSession   E
     P*=======================================================
     P* Processo..: $GetDBSession
     P* Descrição.: Obter a sessão corrente
     P*=======================================================
     P $GetDBSession   B                   EXPORT
     D                 PI             5S 0
     D    SessionID    S              5S 0
     D WIdx            S              5S 0
     D*===========================================================
      /FREE
       IF (NOT Var#Sess(WSessID).IsActive);
          WIdx = WSessID - 1;
          DOW (WIdx > 1);
             IF (Var#Sess(WIdx).IsActive);
                WSessID = WIdx;
                RETURN WIdx;
             ENDIF;
             WIdx -= 1;
          ENDDO;
          RETURN *ZEROS;  // Erro: Não existe nenhuma sessão activa
       ELSE;
          Return WSessID;
       ENDIF;
      /END-FREE
     P $GetDBSession   E
     P*=======================================================
     P* Processo..: $AddDBSession
     P* Descrição.: Adicionar uma nova Sessão
     P*=======================================================
     P $AddDBSession   B                   EXPORT
     D                 PI             5S 0
     D WIdx            S              5S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE
       WIdx = *ZEROS;
       DOW (WIdx < 25);
          WIdx += 1;
          IF (NOT Var#Sess(WIdx).IsActive);
             Var#Sess(WIdx).PrevSession = WSessID;
             WSessID = WIdx;
             Var#Sess(WIdx).IsActive = *ON;
             $InzDBSession(WIdx);
             LEAVE;
          ENDIF;
       ENDDO;

       IF (WIdx = 25);
         WIdx = *ZEROS;  // Não existem sessões disponíveis
       ENDIF;

       Return WIdx;
      /END-FREE
     P $AddDBSession   E
     P*=======================================================
     P* Processo..: $RmvDBSession
     P* Descrição.: Eliminar a sessão corrente
     P*=======================================================
     P $RmvDBSession   B                   EXPORT
     D                 PI
     D    SessionID                   5S 0 Const Options(*NOPASS)
     D WSession        S              5S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE
       IF (%Parms() > *ZEROS);
          WSession = SessionID;
       ELSE;
          WSession = WSessID;
       ENDIF;

       IF (Var#Sess(WSession).IsActive);
          Var#Sess(WSession).IsActive = *OFF;
          $InzDBSession(WSession);
          WSessID = Var#Sess(WSession).PrevSession;
       ENDIF;
      /END-FREE
     P $RmvDBSession   E
     P*=======================================================
     P* Processo..: $InzDBSession
     P* Descrição.: Inicializar uma Sessão
     P*=======================================================
     P $InzDBSession   B                   EXPORT
     D                 PI
     D    SessionID                   5S 0 Const
     D*===========================================================
      /FREE
       Var#Sess(SessionID).QtdVirtFields = *ZEROS;
       Var#Sess(SessionID).QtdCriter  = *ZEROS;
       Var#Sess(SessionID).Files      = *BLANKS;
       Var#Sess(SessionID).Fields     = *BLANKS;
       Var#Sess(SessionID).DatSelect  =%DATE();
       Var#Sess(SessionID).Where      = *BLANKS;
       Var#Sess(SessionID).OrderBy    = *BLANKS;
       Var#Sess(SessionID).Repetidos = 'N';
       Var#Sess(SessionID).MaxRec    = *ZEROS;
       Var#Sess(SessionID).QtdRec    = *ZEROS;
       Var#Sess(SessionID).IsEOF     = *ON;
       Var#Sess(SessionID).IsEmpty   = *ON;
       Var#Sess(SessionID).PagAct    = 1;
       Var#Sess(SessionID).Lst#Scr(1).IsEmpty = *ON;
       Var#Sess(SessionID).QtdFldScr = *ZEROS;
      /END-FREE
     P $InzDBSession   E

     P*=======================================================
     P* Processo..: $SetQuery
     P* Descrição.: Preencher as variáveis globais com valores
     P*=======================================================
     P $SetQuery       B                   EXPORT
     D                 PI
     D    Fields                    300A   Const
     D    Files                     300A   Const
     D    Where                     300A   Const Options(*NOPASS)
     D    OrderBy                   100A   Const Options(*NOPASS)
     D WWhere          S                   LIKE(Where)
     D WOrderBy        S                   LIKE(OrderBy)
     D*===========================================================
      /FREE
       WWHere   = *BLANKS;
       WOrderBy = *BLANKS;
       IF (%PARMS() > 2);
          WWHere = Where;
          IF (%PARMS() > 3);
             WOrderBy = OrderBy;
          ENDIF;
       ENDIF;
       Var#Sess(WSessID).Fields  = Fields;
       Var#Sess(WSessID).Files   = Files;
       Var#Sess(WSessID).Where   = WWhere;
       Var#Sess(WSessID).OrderBy = WOrderBy;

       //-------------------------------------------
       //  Inicializar controlo de sessão
       //-------------------------------------------
       Var#Sess(WSessID).MaxRec  = *ZEROS;
       Var#Sess(WSessID).IsEOF   = *ON;
       Var#Sess(WSessID).IsEmpty = *ON;
       Var#Sess(WSessID).PagAct  = 1;
      /END-FREE
     P $SetQuery       E

     P*=======================================================
     P* Processo..: $SetFiles
     P* Descrição.: ForCompr com  valores o parametro Ficheiros
     P*=======================================================
     P $SetFiles       B                   EXPORT
     D                 PI             5S 0
     D    Files                     300A   Const
     D    Prefix                     10A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WPrefix         S                   LIKE(Prefix) INZ(*BLANKS)
     D WResult         S              5S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE
       WPrefix = *BLANKS;
       IF (%PARMS() > 1);
          WPrefix = Prefix;
       ENDIF;

       //-------------------------------------------
       //  Inicializar controlo de sessão
       //-------------------------------------------
       WResult = $AddDBSession();

       IF (WResult > *ZEROS);
          //-------------------------------------------
          //  Inicializar controlo de sessão
          //-------------------------------------------
          Var#Sess(WSessID).Files     = %TRIM(Files) + ' ' + %TRIM(WPrefix);
          Var#Sess(WSessID).HstField  = *BLANKS;

          //-------------------------------------------
          //  Validar se é para Importar os Campos
          //-------------------------------------------
          IF (%LEN(%TRIM(Files)) <= 10);
             $SetFields(Files:WPrefix);
          ENDIF;
       ENDIF;
       RETURN WResult;
      /END-FREE
     P $SetFiles       E

     P*=======================================================
     P* Processo..: $SetFields
     P* Descrição.: ForCompr com  valores o parametro Ficheiros
     P*             recebe, como parâmetros a lista de campos
     P*             ou o nome do Ficheiro e o prefixo
     P*=======================================================
     P $SetFields      B                   EXPORT
     D                 PI
     D    Fields                   1000A   Const
     D    Prefix                     10A   Const Options(*NOPASS)
     D    IsToReplace                  N   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WFields         S           1000A   INZ(*BLANKS)
     D WPrefix         S              1A   INZ(*BLANKS)
     D WDatInicio      S             20A   INZ(*BLANKS)
     D WDatFim         S             20A   INZ(*BLANKS)
     D WBakHstField    S            200A   INZ(*BLANKS)
     D WHstField       S            200A   INZ(*BLANKS)
     D WIsToReplace    S               N   INZ(*ON)
     D WLastFields     S           1000A   INZ(*BLANKS)
     D*===========================================================
      /FREE
       WFields = Fields;
       WPrefix = *BLANKS;
       IF (%Parms() > 1);
          WPrefix = Prefix;
          WIsToReplace = *ON;
          IF (%Parms() > 2);
             WIsToReplace = IsToReplace;
          ENDIF;

          IF (%LEN(%TRIM(Fields)) <= 10);
             WFields     = $getFieldList(%TRIM(FIELDS):%TRIM(WPrefix));
          ELSE;
             //---->
          ENDIF;

          WLastFields = *BLANKS;
          IF (NOT WIsToReplace);
             WLastFields = Var#Sess(WSessID).Fields;
             IF (WLastFields <> *BLANKS);
                WFields = %TRIM(WFields)+','+WLastFields;
             ENDIF;
          ENDIF;

          WDatInicio = $getFieldCtl(%TRIM(Fields):'DI');
          WDatFim    = $getFieldCtl(%TRIM(Fields):'DF');
          WHstField   = *BLANKS;
          IF (WDatInicio<>*BLANKS) AND (WDatFim<>*BLANKS);
             WHstField = '('+%TRIM(WDatInicio)+'<=''@DATA()'') AND ('+
                          %TRIM(WDatFim)+'>''@DATA()'')';
             WBakHstField = Var#Sess(WSessID).HstField;
             IF (WBakHstField <> *BLANKS);
                WHstField = %TRIM(WHstField)+' AND '+%TRIM(WBakHstField);
             ENDIF;
             Var#Sess(WSessID).HstField = WHstField;
          ENDIF;
       ENDIF;

       //-------------------------------------------
       //  Inicializar controlo de sessão
       //-------------------------------------------
       Var#Sess(WSessID).Fields  = WFields;
       Var#Sess(WSessID).MaxRec  = *ZEROS;
       Var#Sess(WSessID).IsEOF   = *ON;
       Var#Sess(WSessID).IsEmpty = *ON;
       Var#Sess(WSessID).PagAct  = 1;
      /END-FREE
     P $SetFields      E

     P*=======================================================
     P* Processo..: $AddField
     P* Descrição.: Adicionar um Campo Virtual à lista
     P*=======================================================
     P $AddField       B                   EXPORT
     D                 PI
     D    FldName                    20A   Const
     D    FldType                     1A   Const Options(*NOPASS)
     D    FldCode                    20A   Const Options(*NOPASS)
     D    FldTable                    5S 0 Const Options(*NOPASS)
     D WFldType        S                   LIKE(FldType)
     D WFldCode        S                   LIKE(FldCode)
     D WFldTable       S                   LIKE(FldTable)
     D WIdx            S              5S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE
       WFldType = 'D';      // {D=Detalhe;H=Header}
       WFldCode = '<RES>';  // Assumir valor parametrizado
       WFldTable = 99999;   // Assumir valor parametrizado
       IF (%Parms() > 1);
          WFldType = FldType;
          IF (%Parms() > 2);
             WFldCode = FldCode;
             IF (%Parms() > 3);
                WFldTable = FldTable;
             ENDIF;
          ENDIF;
       ENDIF;
       WIdx = Var#Sess(WSessID).QtdVirtFields + 1;
       Var#Sess(WSessID).Lst#Fields(WIdx).Label = FldName;
       Var#Sess(WSessID).Lst#Fields(WIdx).Tipo  = WFldType;
       Var#Sess(WSessID).Lst#Fields(WIdx).Field = WFldCode;
       Var#Sess(WSessID).Lst#Fields(WIdx).Table = WFldTable;
       Var#Sess(WSessID).Lst#Fields(WIdx).Valor = *BLANKS;
       Var#Sess(WSessID).QtdVirtFields = WIdx;
      /END-FREE
     P $AddField       E

     P*=======================================================
     P* Processo..: $SetSort
     P* Descrição.: ForCompr com  valores o parametro Ficheiros
     P*=======================================================
     P $SetSort        B                   EXPORT
     D                 PI
     D    OrderBy                   100A   Const
     D*===========================================================
      /FREE
       Var#Sess(WSessID).OrderBy = OrderBy;

       //-------------------------------------------
       //  Inicializar controlo de sessão
       //-------------------------------------------
       Var#Sess(WSessID).MaxRec  = *ZEROS;
       Var#Sess(WSessID).IsEOF   = *ON;
       Var#Sess(WSessID).IsEmpty = *ON;
       Var#Sess(WSessID).PagAct  = 1;
      /END-FREE
     P $SetSort        E

     P*=======================================================
     P* Processo..: $SetRepetidos
     P* Descrição.: Obter Registos Repetidos (S=Sim, N=Não)
     P*=======================================================
     P $SetRepetidos   B                   EXPORT
     D                 PI
     D    Repetidos                   1A   Const
     D*===========================================================
      /FREE
       Var#Sess(WSessID).Repetidos =  Repetidos;

       //-------------------------------------------
       //  Inicializar controlo de sessão
       //-------------------------------------------
       Var#Sess(WSessID).MaxRec  = *ZEROS;
       Var#Sess(WSessID).IsEOF   = *ON;
       Var#Sess(WSessID).IsEmpty = *ON;
       Var#Sess(WSessID).PagAct  = 1;
      /END-FREE
     P $SetRepetidos   E

     P*=======================================================
     P* Processo..: $getCritText()
     P* Descrição.: Obter o texto do modo de Pesquisa actvo
     P*=======================================================
     P $getCritText    B                   EXPORT
     D                 PI            60A
     D WIdx            S              3S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE
       WIdx = *ZEROS;
       DOW (WIdx < Var#Sess(WSessID).QtdCriter);
          WIdx += 1;
          IF (Var#Sess(WSessID).Lst#Criteria(WIdx).Select);
             RETURN Var#Sess(WSessID).Lst#Criteria(WIdx).Label;
          ENDIF;
       ENDDO;
       RETURN *BLANKS;
      /END-FREE
     P $getCritText    E
     P*=======================================================
     P* Processo..: $AddCriteria
     P* Descrição.: Adicionar o texto para pesquisa multipla
     P*=======================================================
     P $AddCriteria    B                   EXPORT
     D                 PI
     D    Label                      60A   Const
     D    Field                      20A   Const
     D    Compar                      2A   Const Options(*NOPASS)
     D    Sort                         N   Const Options(*NOPASS)
     D*==> Campos de Trabalho
     D WField          S             20A
     D WCompar         S              2A   INZ(*BLANKS)
     D WOperador       S             50A   INZ(*BLANKS)
     D WIdx            S              3S 0 INZ(*ZEROS)
     D WIdxPosis       S              3S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE
       WIdx = Var#Sess(WSessID).QtdCriter + 1;
       WField = Field;

       //--------------------------------------------------
       // Se já existir então actualiza
       //--------------------------------------------------
       IF (WIdx > 1);
         WIdxPosis = 1;
         DOW (WIdxPosis < Widx);
            IF (Var#Sess(WSessID).Lst#Criteria(WIdxPosis).Field = WField);
               WIdx = WIdxPosis;
               LEAVE;
            ENDIF;
            WIdxPosis += 1;
         ENDDO;
       ENDIF;

       Var#Sess(WSessID).Lst#Criteria(WIdx).Label = Label;
       Var#Sess(WSessID).Lst#Criteria(WIdx).Field = WField;
       Var#Sess(WSessID).Lst#Criteria(WIdx).Empty = *OFF;
       Var#Sess(WSessID).Lst#Criteria(WIdx).Valor = *BLANKS;
       Var#Sess(WSessID).Lst#Criteria(WIdx).Sort  = 'A';
       Var#Sess(WSessID).QtdCriter = WIdx;
       IF (%PARMS() > 2);
          WCompar = Compar;
       ELSE;
          WCompar = 'GE';
       ENDIF;
       IF (WCompar ='LK');
          WOperador = %TRIM(WField) + ' LIKE ''%@VAR()%''';
       ELSEIF (WCompar ='GT');
          WOperador = %TRIM(WField) + ' > ''@VAR()''';
       ELSEIF (WCompar ='GE');
          WOperador = %TRIM(WField) + '>= ''@VAR()''';
       ELSEIF (WCompar ='EQ');
          WOperador = %TRIM(WField) + '= ''@VAR()''';
       ELSEIF (WCompar ='LE');
          WOperador = %TRIM(WField) + '<= ''@VAR()''';
       ELSEIF (WCompar ='LT');
          WOperador = %TRIM(WField) + '< ''@VAR()''';
       ENDIF;
       Var#Sess(WSessID).Lst#Criteria(WIdx).Where = WOperador;

       //-----------------------------------------------------------
       // Pode defeito o primeiro registo é seleccionado
       //-----------------------------------------------------------
       IF (%PARMS() > 3);
          Var#Sess(WSessID).Lst#Criteria(WIdx).Sort = Sort;
       ENDIF;
       IF (WIdx = 1);
          Var#Sess(WSessID).Lst#Criteria(WIdx).Select = *ON;
          IF (Var#Sess(WSessID).Lst#Criteria(WIdx).Sort = 'A');
             $SetSort(WField+' #ASC');
          ELSE;
             $SetSort(WField+' #DESC');
          ENDIF;
       ENDIF;
      /END-FREE
     P $AddCriteria    E
     P*=======================================================
     P* Processo..: $ChgCriteria
     P* Descrição.: Mudar o ecrã de pesquisa actual
     P*=======================================================
     P $ChgCriteria    B                   EXPORT
     D                 PI
     D
     D*==> Campos de Trabalho
     D WIdx            S              3S 0 INZ(*ZEROS)
     D WField          S             20A
     D*===========================================================
      /FREE
       WIdx = *ZEROS;
       DOW (WIdx < Var#Sess(WSessID).QtdCriter);
          WIdx += 1;
          IF (Var#Sess(WSessID).Lst#Criteria(WIdx).Select);
             Var#Sess(WSessID).Lst#Criteria(WIdx).Select = *OFF;
             IF (WIdx = Var#Sess(WSessID).QtdCriter);
                WIDX = 1;
             ELSE;
                WIDX += 1;
             ENDIF;
             Var#Sess(WSessID).Lst#Criteria(WIdx).Select = *ON;
             IF (Var#Sess(WSessID).Lst#Criteria(WIdx).Sort = 'A');
                $SetSort(Var#Sess(WSessID).Lst#Criteria(WIdx).Field+' #ASC');
             ELSE;
                $SetSort(Var#Sess(WSessID).Lst#Criteria(WIdx).Field+' #DESC');
             ENDIF;
             $SetCriteria(*BLANKS:*ON);
             LEAVE;
          ENDIF;
       ENDDO;
      /END-FREE
     P $ChgCriteria    E
     P*=======================================================
     P* Processo..: $GetCriteria
     P* Descrição.: Obter a condição de selecção de registos
     P*=======================================================
     P $GetCriteria    B                   EXPORT
     D                 PI           300A
     D WIdx            S              3S 0 INZ(*ZEROS)
     D WValor          S             50A   INZ(*BLANKS)
     D WWhere          S            300A   INZ(*BLANKS)
     D WData           S             10A   INZ(*BLANKS)
     D WDatSelect      S               D
     D WHstField       S            200A   INZ(*BLANKS)
     D*===========================================================
      /FREE
       WWHere = *BLANKS;
       IF (Var#Sess(WSessID).QtdCriter > *ZEROS);
          WIdx = *ZEROS;
          DOW (WIdx < Var#Sess(WSessID).QtdCriter);
             WIdx += 1;
             IF (Var#Sess(WSessID).Lst#Criteria(WIdx).Select);
                WVAlor = Var#Sess(WSessID).Lst#Criteria(WIdx).Valor;
                IF (%TRIM(WValor) <> *BLANKS);
                   WWhere = %TRIM(WWHere) + ' ' +
                     $StrReplace('@VAR()':%TRIM(WVAlor):
                     Var#Sess(WSessID).Lst#Criteria(WIdx).Where);
                   Var#Sess(WSessID).OrderBy =
                     %TRIM(Var#Sess(WSessID).Lst#Criteria(WIdx).Field);
                ENDIF;
                LEAVE;
             ENDIF;
          ENDDO;
       ENDIF;
       IF (WWHere<>*BLANKS);
          IF (%TRIM(Var#Sess(WSessID).Where)<>*BLANKS);
             WWHere = %TRIM(WWHere)+' AND '+%TRIM(Var#Sess(WSessID).Where);
          ENDIF;
       ELSE;
          WWHere = %TRIM(Var#Sess(WSessID).Where);
       ENDIF;

       WHstField = Var#Sess(WSessID).HstField;
       IF (%TRIM(WHstField) <> *BLANKS);
          WDatSelect = Var#Sess(WSessID).DatSelect;
          WData      = %CHAR(%SUBDT(WDatSelect:*YEARS))+'-'+
                       %CHAR(%SUBDT(WDatSelect:*MONTHS))+'-'+
                       %CHAR(%SUBDT(WDatSelect:*DAYS));
          WHstField  = $StrReplace('@DATA()':%TRIM(WData):WHstField);
          WHstField  = $StrReplace('@DATA()':%TRIM(WData):WHstField);
          IF (WWHere = *BLANKS);
             WWHere     = %TRIM(WHstField);
          ELSE;
             WWHere     = %TRIM(WWHere)+' AND '+%TRIM(WHstField);
          ENDIF;
       ENDIF;
       RETURN WWhere;
      /END-FREE
     P $GetCriteria    E
     P*=======================================================
     P* Processo..: $SetCriteria
     P* Descrição.: ForCompr com  valores o parametro Ficheiros
     P*=======================================================
     P $SetCriteria    B                   EXPORT
     D                 PI
     D    Value                     300A   Const
     D    Intern                       N   Const Options(*NOPASS)
     D WIdx            S              3S 0 INZ(*ZEROS)
     D WOperador       S             20A   INZ(*BLANKS)
     D WInstr          S              2A   INZ(*BLANKS)
     D WWhere          S            300A   INZ(*BLANKS)
     D*===========================================================
      /FREE
       IF (%PARMS() > 1);
          WIdx = *ZEROS;
          DOW (WIdx < Var#Sess(WSessID).QtdCriter);
             WIdx += 1;
             IF (Var#Sess(WSessID).Lst#Criteria(WIdx).Select);
                Var#Sess(WSessID).Lst#Criteria(WIdx).Valor = %TRIM(VALUE);
                LEAVE;
             ENDIF;
          ENDDO;
       ELSE;
          Var#Sess(WSessID).Where = Value;
       ENDIF;

       //-------------------------------------------
       //  Inicializar controlo de sessão
       //-------------------------------------------
       Var#Sess(WSessID).MaxRec  = *ZEROS;
       Var#Sess(WSessID).IsEOF   = *ON;
       Var#Sess(WSessID).IsEmpty = *ON;
       Var#Sess(WSessID).PagAct = 1;
      /END-FREE
     P $SetCriteria    E

     P*=======================================================
     P* Processo..: $getParametro
     P* Descrição.: Obter os Prâmetros da Query
     C*=======================================================
     P $GetParametro   B                   EXPORT
     D                 PI           300A
     D    Tipo                        1A   Const
     D*===========================================================
      /FREE
       IF (TIPO = 'W'); // Devolve a Criteria
          RETURN $GetCriteria();
       ELSEIF (Tipo = 'F');
          RETURN Var#Sess(WSessID).Files; // Devolve os Ficheiros
       ELSEIF (Tipo = 'C');
          RETURN Var#Sess(WSessID).Fields; // Devolve os Campos
       ELSEIF (Tipo = 'O');
          RETURN Var#Sess(WSessID).OrderBy; // Devolve a Ordenção
       ENDIF;
      /END-FREE
     P $GetParametro   E

     P*=======================================================
     P* Processo..: $SetPage
     P* Descrição.: Definir o número máximo de Registos
     P*=======================================================
     P $SetPage        B                   EXPORT
     D                 PI
     D    WQtdRegisto                 5S 0 Const
     D*===========================================================
      /FREE
       Var#Sess(WSessID).QtdRec = WQtdRegisto;
      /END-FREE
     P $SetPage        E
     P*=======================================================
     P* Processo..: $getPageNbr
     P* Descrição.: Obter o actual número de Página
     P*=======================================================
     P $GetPageNbr     B                   EXPORT
     D                 PI             5S 0
     D*=======================================================
      /FREE
         RETURN Var#Sess(WSessId).PagAct;
      /END-FREE
     P $GetPageNbr     E
     P*=======================================================
     P* Processo..: $isLastPage
     P* Descrição.: Está na última página
     P*=======================================================
     P $isLastPage     B                   EXPORT
     D                 PI              N
     D
     D*==> Campos de Trabalho
     D WActRec         S              9S 0
     D WMaxRec         S              9S 0
     D WRecPag         S              9S 0
     D WMaxPag         S              9S 0
     D*===========================================================
      /FREE
        WActRec = Var#Sess(WSessID).ActRec;
        WRecPag = Var#Sess(WSessID).QtdRec;
        WMaxRec = Var#Sess(WSessID).MaxRec;
        WMaxPag = %INT((WMaxRec - 1) / WRecPag) + 1;
        IF (Var#Sess(WSessID).PagAct = WMaxPag);
           RETURN *ON;
        ELSE;
           RETURN *OFF;
        ENDIF;
      /END-FREE
     P $isLastPage     E
     P*=======================================================
     P* Processo..: $getPage
     P* Descrição.: Obter uma página
     P*=======================================================
     P $GetPage        B                   EXPORT
     D                 PI             5S 0
     D   Page#Cur                     1A   Const
     D   Default                      1A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D
     D WPagAct         S              5S 0 INZ(*ZEROS)
     D WRecIni         S              9S 0 INZ(*ZEROS)
     D WActRec         S              9S 0
     D WQtdRec         S              5S 0 INZ(*ZEROS)
     D WIdx            S              5S 0 INZ(*ZEROS)
     D*=======================================================
      /FREE
         MONITOR;
         //-----------------------------------------------------
         // ForCompr o número máximo de registos da Consulta
         //-----------------------------------------------------
        // IF (NOT Var#Sess(WSessID).IsEOF);
            IF (Var#Sess(WSessID).MaxRec = *ZEROS);
               Var#Sess(WSessID).MaxRec = $GetQtdeReg();
            ENDIF;
        // ELSE;
        //    RETURN *ZEROS;
        // ENDIF;

         //-----------------------------------------------------
         // Proceder à navegação dentro dos registos da Consulta
         //-----------------------------------------------------
         IF (Var#Sess(WSessID).MaxRec > *ZEROS);
            WQtdRec = Var#Sess(WSessID).QtdRec;
            WPagAct = Var#Sess(WSessID).PagAct;
            // WRecIni = 1;
            IF (Page#Cur = Pag#Up);
               IF (WPagAct > 1);
                  WPagAct -= 1;
               ENDIF;
            ELSEIF (Page#Cur = Pag#Down);
               IF (NOT $isLastPage());
                  WPagAct += 1;
               ENDIF;
            ENDIF;
            WRecIni = ((WPagAct - 1 ) * WQtdRec) + 1;
            Var#Sess(WSessID).PagAct = WPagAct;
            RETURN $getRecord(WRecIni:WQtdRec);
         ELSE;
            RETURN *ZEROS;
         ENDIF;
         ON-ERROR;
            RETURN *ZEROS;
         ENDMON;
      /END-FREE
     P $GetPage        E

     P*=======================================================
     P* Processo..: $GetQtdeReg
     P* Descrição.: Obter o total de registos
     C*=======================================================
     P $GetQtdeReg     B                   EXPORT
     D                 PI             9S 0
     D
     D WSQLStmt        S           2000A
     D WActRec         S              9S 0
     D WResult         S              9S 0
     D*===========================================================
      /FREE

       //------------------------------------------------
       // Seleccionar os registos a apresentar
       //------------------------------------------------
       WActRec = 1;
       WSqlStmt = $getSQLStmt(%TRIM(Var#Sess(WSessID).Fields):
                              %TRIM(Var#Sess(WSessID).Files):
                              %TRIM($GetCriteria()):
                              %TRIM(Var#Sess(WSessID).OrderBy):
                              %TRIM(Var#Sess(WSessID).Repetidos):
                              1:1:*ON);
       EXEC SQL PREPARE STMREC2 FROM :WSqlStmt;
       EXEC SQL DECLARE REC2 CURSOR FOR STMREC2;
       EXEC SQL OPEN REC2;
       DOW (sqlcod = 0);
          EXEC SQL FETCH REC2 INTO: WResult;
       ENDDO;
       EXEC SQL CLOSE REC2;

       RETURN WResult;
      /END-FREE
     P $GetQtdeReg     E

     P*=======================================================
     P* Processo..: $getRecord
     P* Descrição.: ForCompr com  valores o parametro Ficheiros
     C*=======================================================
     P $GetRecord      B                   EXPORT
     D                 PI             5S 0
     D    RegIni                      9S 0 Const Options(*NOPASS)
     D    QtdReg                      3S 0 Const Options(*NOPASS)
     D    IsLogical                    N   Const Options(*NOPASS)
     D    Reset                        N   Const Options(*NOPASS)
     D
     D WIsLogical      S               N   INZ(*OFF)
     D WSQLStmt        S           2000A
     D WReset          S               N   INZ(*ON)
     D WIdxLst         S              3S 0 INZ(*ZEROS)
     D WIdxDesc        S              3S 0 INZ(*ZEROS)
     D WActRec         S              9S 0 INZ(*ZEROS)
     D WValor          S             30A   INZ(*BLANKS)
     D WNome           S             20A   INZ(*BLANKS)
     D WTipo           S              1A   INZ(*BLANKS)
     D WResult         S              5S 0 INZ(*ZEROS)
     D WQtdReg         S              3S 0 INZ(*ZEROS)
     D WRegIni         S              9S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE
       WQtdReg = 1;
       WRegIni = 1;
       IF (%PARMS() >= 1);
          WRegIni = RegIni;
          IF (%PARMS() > 1);
             WQtdReg = QtdReg;
             IF (%PARMS() > 2);
                WIsLogical = IsLogical;
                IF (%PARMS() > 3);
                   WReset = Reset;
                ENDIF;
             ENDIF;
          ENDIF;
       ENDIF;

       //------------------------------------------------
       // Inicalizar lista de Trabalho
       //------------------------------------------------
       WIdxLst = 1;
       DOW (WIdxLst < 25);
          Var#Sess(WSessID).Lst#Data(WIdxLst).F#Rec = *ZEROS;
          Var#Sess(WSessID).Lst#Data(WIdxLst).F#Data = *BLANKS;
          Var#Sess(WSessID).Lst#Data(WIdxLst).F#Fill = *OFF;
          Lst#Data(WIdxLst).F#Rec = *ZEROS;
          Lst#Data(WIdxLst).F#Data = *BLANKS;
          Lst#Data(WIdxLst).F#Fill = *OFF;
          WIdxLst += 1;
       ENDDO;

       //------------------------------------------------
       // Seleccionar os registos a apresentar
       //------------------------------------------------
       Var#Sess(WSessID).IsEof  = *ON;
       WActRec = 1;

       WSqlStmt = $getSQLStmt(%TRIM(Var#Sess(WSessID).Fields):
                    %TRIM(Var#Sess(WSessID).Files):
                    %TRIM($GetCriteria()):
                    %TRIM(Var#Sess(WSessID).OrderBy):
                    %TRIM(Var#Sess(WSessID).Repetidos):
                   WRegIni:WQtdReg:WIsLogical);

       IF (NOT WIsLogical);
          EXEC SQL PREPARE STMRECPF FROM :WSqlStmt;
          EXEC SQL DECLARE RECPF CURSOR FOR STMRECPF;
          EXEC SQL OPEN RECPF;
          DOW (sqlcod = 0);
          EXEC SQL FETCH NEXT FROM RECPF FOR :WQtdReg ROWS INTO: Lst#Data;
             Var#Sess(WSessID).IsEof  = *OFF;
          ENDDO;
          EXEC SQL CLOSE RECPF;
       ELSE;
          IF (WReset);
             EXEC SQL CLOSE RECLF;
             EXEC SQL PREPARE STMRECLF  FROM :WSqlStmt;
             EXEC SQL DECLARE RECLF INSENSITIVE SCROLL CURSOR FOR STMREC;
             EXEC SQL OPEN RECLF;
             DOW (sqlcod = 0);
                EXEC SQL FETCH RELATIVE :WRegIni FROM RECLF
                                      FOR :WQtdReg ROWS INTO: Lst#Data;
                Var#Sess(WSessID).IsEof  = *OFF;
             ENDDO;
          ELSE;
             DOW (sqlcod = 0);
                EXEC SQL FETCH CURRENT FROM RECLF FOR :WQtdReg ROWS INTO:
                  Lst#Data;
                Var#Sess(WSessID).IsEof  = *OFF;
             ENDDO;
             sqlCod = *ZEROS;
          ENDIF;
       ENDIF;

       //------------------------------------------------
       // ForCompr lista com os registos
       //------------------------------------------------
       WIdxLst  = *ZEROS;
       WIdxDesc = *ZEROS;
       WValor   = *BLANKS;
       IF (NOT Var#Sess(WSessID).IsEof);
          DOW (WIdxLst < WQtdReg);
             WIdxLst += 1;
             IF (Lst#Data(WIdxLst).F#Fill);
                Var#Sess(WSessID).Lst#Data(WIdxLst).F#Rec =
                   Lst#Data(WIdxLst).F#Rec;
                Var#Sess(WSessID).Lst#Data(WIdxLst).F#Data =
                   Lst#Data(WIdxLst).F#Data;
                Var#Sess(WSessID).Lst#Data(WIdxLst).F#Fill =
                   Lst#Data(WIdxLst).F#Fill;
                WActRec = WIdxLst;
             ELSE;
                WIdxLst -= 1;
                LEAVE;
             ENDIF;
          ENDDO;
          Var#Sess(WSessID).ActRec = 1;
       ENDIF;

       RETURN WIdxLst; // Devolve o número de registos extraídos
      /END-FREE
     P $GetRecord      E

     P*=======================================================
     P* Processo..: $getFirst
     P* Descrição.: Posicionar no Primeiro Registo
     C*=======================================================
     P $GetFirst       B                   EXPORT
     D                 PI
     D
     D*==> Campos de Trabalho
     D
     D*===========================================================
      /FREE
         Var#Sess(WSessID).ActRec = 1;
         Var#Sess(WSessID).IsEof  = *OFF;
      /END-FREE
     P $GetFirst       E
     P*=======================================================
     P* Processo..: $getLast
     P* Descrição.: Posicionar no Último Registo
     C*=======================================================
     P $GetLast        B                   EXPORT
     D                 PI
     D
     D*==> Campos de Trabalho
     D WActRec         S              9S 0
     D WMaxRec         S              9S 0
     D WMaxPag         S              9S 0
     D WRecPag         S              9S 0
     D*===========================================================
      /FREE
        WRecPag = Var#Sess(WSessID).QtdRec;
        WMaxPag = %INT(WMaxRec / WRecPag);
        IF (WMaxPag = Var#Sess(WSessID).PagAct);
           WActRec = Var#Sess(WSessID).MaxRec;
        ELSE;
           WActRec = WRecPag;
        ENDIF;
        Var#Sess(WSessID).ActRec = WActRec;
        Var#Sess(WSessID).IsEof  = *OFF;
      /END-FREE
     P $GetLast        E
     P*=======================================================
     P* Processo..: $getNext
     P* Descrição.: Posicionar no Registo Seguinte
     C*=======================================================
     P $GetNext        B                   EXPORT
     D                 PI
     D
     D*==> Campos de Trabalho
     D WActRec         S              9S 0
     D WMaxRec         S              9S 0
     D WRecPag         S              9S 0
     D WMaxPag         S              9S 0
     D*===========================================================
      /FREE
        WActRec = Var#Sess(WSessID).ActRec;
        WRecPag = Var#Sess(WSessID).QtdRec;
        WMaxRec = Var#Sess(WSessID).MaxRec;
        WMaxPag = %INT((WMaxRec - 1) / WRecPag) + 1;
        IF (WMaxPag = Var#Sess(WSessID).PagAct);
           WMaxRec = WMaxRec - (WRecPag * (WMaxPag - 1));
        ELSE;
           WMaxRec = WRecPag;
        ENDIF;

        Var#Sess(WSessID).IsEof  = *ON;
        IF (WActRec < WMaxRec);
           Var#Sess(WSessID).ActRec += 1;
           Var#Sess(WSessID).IsEof = *OFF;
        ELSE;
           Var#Sess(WSessID).IsEof = *ON;
        ENDIF;
      /END-FREE
     P $GetNext        E
     P*=======================================================
     P* Processo..: $getPrevious
     P* Descrição.: Posicionar no Registo Anterior
     C*=======================================================
     P $GetPrevious    B                   EXPORT
     D                 PI
     D
     D*==> Campos de Trabalho
     D WActRec         S              5S 0
     D*===========================================================
      /FREE
        WActRec = Var#Sess(WSessID).ActRec;
        Var#Sess(WSessID).IsEof  = *ON;
        IF (WActRec > 1);
           Var#Sess(WSessID).ActRec -= 1;
           Var#Sess(WSessID).IsEof  = *OFF;
        ENDIF;
      /END-FREE
     P $GetPrevious    E
     P*=======================================================
     P* Processo..: $getNextKey
     P* Descrição.: Obter a última chave do registo
     C*=======================================================
     P $GetNextKey     B                   EXPORT
     D                 PI            10S 0
     D  PTabela                      40A   Const
     D  PCampo                       20A   Const
     D  PCondicao                   100A   Const Options(*NOPASS)
     D WSqlStmt        S            250A
     D WMaximo         S             10S 0
     D*===========================================================
      /FREE
         WMaximo = *ZEROS;
         WSqlStmt = 'SELECT MAX('+%TRIM(PCampo)+') AS MAXIMO FROM '+
                     %TRIM(PTabela) ;
         IF (%PARMS()> 2);
            WSqlStmt = %TRIM(WSqlStmt) + ' WHERE ' + %TRIM(PCondicao);
         ENDIF;

         EXEC SQL PREPARE STMLAST  FROM :WSqlStmt;
         EXEC SQL DECLARE RECLST CURSOR FOR STMLAST;
         EXEC SQL OPEN RECLST;
         DOW (sqlcod = 0);
            EXEC SQL FETCH RECLST INTO: WMaximo;
         ENDDO;
         EXEC SQL CLOSE RECLST;
         RETURN (WMaximo + 1);
      /END-FREE
     P $GetNextKey     E

     P*=======================================================
     P* Processo..: $getFieldNum
     P* Descrição.: Obter o Valor para o Campo num formato numerico
     C*=======================================================
     P $GetFieldNum    B                   EXPORT
     D                 PI            15S 5
     D    FldName                    15A   Const
     D
     D*==> Campos de Trabalho
     D WResultado      S             15S 5
     D*=======================================================
      /FREE
         WResultado = *ZEROS;
         MONITOR;
            WResultado = %DEC($GetField(FldName):15:5);
         ON-ERROR;
         ENDMON;
         RETURN WResultado;
      /END-FREE
     P $GetFieldNum    E

     P*=======================================================
     P* Processo..: $getFieldText
     P* Descrição.: Obter o Valor para o Campo num formato numerico
     C*=======================================================
     P $GetFieldText   B                   EXPORT
     D                 PI           500A
     D    FldName                    15A   Const
     D
     D*==> Campos de Trabalho
     D WF#Data         S           1000A   INZ(*BLANKS)
     D WPosField       S              3S 0 INZ(*ZEROS)
     D WActRec         S              9S 0 INZ(*ZEROS)
     D WLimite         DS                  LIKEDS(OB#Coord)
     D WResult         S            500A
     D*=======================================================
      /FREE
        WPosField = *ZEROS;
        WResult   = *BLANKS;
        IF (Var#Sess(WSessID).IsEof);
           RETURN *BLANKS;
        ENDIF;
        WPosField = getPosIni(FldName);

        //----------------------------------------------------------
        // Extrair o valor do Campo
        //----------------------------------------------------------
        IF (WPosField = *ZEROS);
           //-----------------------------------------------
           // Pesquisar em na lista de campos virtuais
           //-----------------------------------------------
           IF (WPosField = *ZEROS);
           ENDIF;
           WResult = *BLANKS;
        ELSE;
           WLimite = GetFieldLimit(WPosField);
           WActRec = Var#Sess(WSessID).ActRec;
           IF (WLimite.PIni > *ZEROS);
              WF#Data = Var#Sess(WSessID).Lst#Data(WActRec).F#Data;
              IF (WLimite.PFim = *ZEROS);
                 WRESULT = %SUBST(WF#Data:WLimite.PIni);
              ELSE;
                 WRESULT = %SUBST(WF#Data:WLimite.PIni:WLimite.PFim);
              ENDIF;
           ENDIF;
        ENDIF;

        RETURN WResult;
      /END-FREE
     P $GetFieldText   E

     P*=======================================================
     P* Processo..: $getField
     P* Descrição.: Obter o Valor para o Campo físico ou Virtual
     P*             Se for Virtual então aplicar regras para
     P*             ForCompr o Valor.
     C*=======================================================
     P $GetField       B                   EXPORT
     D                 PI           150A
     D    FldName                    15A   Const
     D
     D*==> Campos de Trabalho
     D WF#Data         S           1000A   INZ(*BLANKS)
     D WPosField       S              3S 0 INZ(*ZEROS)
     D WActRec         S              9S 0 INZ(*ZEROS)
     D WLimite         DS                  LIKEDS(OB#Coord)
     D WResult         S            150A   INZ(*BLANKS)
     D*===========================================================
      /FREE
        MONITOR;
        WPosField = *ZEROS;
        WResult   = *BLANKS;
        IF (Var#Sess(WSessID).IsEof);
           RETURN *BLANKS;
        ENDIF;
        WPosField = getPosIni(FldName);

        //----------------------------------------------------------
        // Extrair o valor do Campo
        //----------------------------------------------------------
        IF (WPosField = *ZEROS);
           //-----------------------------------------------
           // Pesquisar em na lista de campos virtuais
           //-----------------------------------------------
           IF (WPosField = *ZEROS);
           ENDIF;
           WResult = *BLANKS;
        ELSE;
           WActRec = Var#Sess(WSessID).ActRec;
           WLimite = GetFieldLimit(WPosField);
           IF (WLimite.PIni > *ZEROS);
              WF#Data = Var#Sess(WSessID).Lst#Data(WActRec).F#Data;
              IF (WLimite.PFim = *ZEROS);
                 WRESULT = %SUBST(WF#Data:WLimite.PIni);
              ELSE;
                 WRESULT = %SUBST(WF#Data:WLimite.PIni:WLimite.PFim);
              ENDIF;
           ENDIF;
        ENDIF;
        ON-ERROR;
        ENDMON;

        RETURN WResult;
      /END-FREE
     P $GetField       E

     P*=======================================================
     P* Processo..: GetFieldLimit
     P* Descrição.: Obter os limite do Campo
     C*=======================================================
     P GetFieldLimit   B                   EXPORT
     D GetFieldLimit   PI                  LIKEDS(OB#Coord)
     D    FldPosis                    3S 0 Const
     D
     D*==> Campos de Trabalho
     D WPosIni         S              3S 0 INZ(*ZEROS)
     D WPosFim         S              3S 0 INZ(*ZEROS)
     D WStrLen         S              3S 0 INZ(*ZEROS)
     D WPosField       S              3S 0 INZ(*ZEROS)
     D WField          S             15A   INZ(*BLANKS)
     D WActRec         S              3S 0 INZ(*ZEROS)
     D WF#Data         S           1000A   INZ(*BLANKS)
     D WLimite         DS                  LIKEDS(OB#Coord)
     D*=======================================================
      /FREE
         WLimite.PIni = 0;
         WLimite.PFim = 0;

         WPosField   = 1;
         WActRec = Var#Sess(WSessID).ActRec;
         WF#Data     = Var#Sess(WSessID).Lst#Data(WActRec).F#Data;
         WPosIni     = 1;
         WPosFim     = %SCAN(%TRIM(';'):WF#Data:WPosIni);
         DOW (WPosFim > *ZEROS);
            IF (WPosField = FldPosis);
               LEAVE;
            ENDIF;
            WPosIni = WPosFim + 1;
            WPosFim = %SCAN(%TRIM(';'):WF#Data:WPosIni);
            WPosField += 1;
         ENDDO;

         IF (WPosField = FldPosis);
            WLimite.PIni = WPosIni;
            IF (WPosFim > *ZEROS);
               WLimite.PFim = WPosFim-WPosIni;
            ENDIF;
         ENDIF;
         RETURN WLimite;
      /END-FREE
     P GetFieldLimit   E

     P*=======================================================
     P* Processo..: GetPosIni
     P* Descrição.: Obter a Posição do Campo
     C*=======================================================
     P GetPosIni       B                   EXPORT
     D                 PI             3S 0
     D    FldName                    15A   Const
     D
     D*==> Campos de Trabalho
     D WPosField       S              3S 0 INZ(*ZEROS)
     D WFieldsBk       S                   LIKE(Var#Sess.Fields)
     D WPosIni         S              3S 0 INZ(*ZEROS)
     D WPosFim         S              3S 0 INZ(*ZEROS)
     D WStrLen         S              3S 0 INZ(*ZEROS)
     D WField          S             15A   INZ(*BLANKS)
     D*===========================================================
      /FREE
        //----------------------------------------------------------
        // Validar a forma de acesso, por nome ou posição
        //----------------------------------------------------------
        MONITOR;
           WPosField = %INT(FldName);
           RETURN WPosField;
        ON-ERROR;
        ENDMON;

        WPosField = 1;
        WFieldsBk = Var#Sess(WSessID).Fields;
        WPosIni   = %SCAN(%TRIM(','):WFieldsBk);
        DOW (WPosIni > *ZEROS);
           WPosFim   = WPosIni;
           WPosIni   = 1;
           WStrLen   = WPosFim - WPosIni;
           WField    = %SUBST(WFieldsBk:1:WStrLen);
           WFieldsBk = %SUBST(WFieldsBk:(WPosFim+1));
           IF (%TRIM(WField) = %TRIM(FldName));
              LEAVE;
           ENDIF;
           WPosIni = %SCAN(%TRIM(','):WFieldsBk);
           WPosField+= 1;
        ENDDO;

        IF (%TRIM(WFieldsBk) <> %TRIM(FldName)) AND
           (%TRIM(WField) <> %TRIM(FldName));
           WPosField = *ZEROS;
        ENDIF;
        RETURN WPosField;
      /END-FREE
     P GetPosIni       E

     P*=======================================================
     P* Processo..: $isEOF
     P* Descrição.: Validar se Chegou ao fim do Ficheiro
     C*=======================================================
     P $IsEof          B                   EXPORT
     D                 PI              N
     D*===========================================================
      /FREE
        RETURN   Var#Sess(WSessID).IsEof;
      /END-FREE
     P $IsEof          E

     P*=======================================================
     P* Processo..: $SetLstFld
     P* Descrição.: Adicionar Parâmetros ao campo da Listagem
     C*=======================================================
     P $SetLstFld      B                   EXPORT
     D                 PI
     D Campo                         20A   CONST
     D Label                         25A   CONST Options(*NOPASS)
     D Largura                        3S 0 CONST Options(*NOPASS)
     D Alinhamento                    1A   CONST Options(*NOPASS)
     D EditCode                       1A   CONST Options(*NOPASS)
     D*Tabela                         5S 0 CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WIx             S              3S 0
     D*===========================================================
      /FREE
        Wix = Var#Sess(WSessID).QtdFldScr + 1;
        Var#Sess(WSessID).Lst#Scr(Wix).Field   = Campo;
        Var#Sess(WSessID).Lst#Scr(Wix).Label   = *BLANKS;
        Var#Sess(WSessID).Lst#Scr(Wix).Largura = *ZEROS;
        Var#Sess(WSessID).Lst#Scr(Wix).Alinhamento = *BLANKS;
        Var#Sess(WSessID).Lst#Scr(Wix).EdtCode = *BLANKS;
        Var#Sess(WSessID).Lst#Scr(Wix).Tabela  = *ZEROS;
        IF (%PARMS > 1);
           Var#Sess(WSessID).Lst#Scr(Wix).Label   = Label;
           IF (%PARMS > 2);
              Var#Sess(WSessID).Lst#Scr(Wix).Largura = Largura;
              IF (%PARMS > 3);
                 Var#Sess(WSessID).Lst#Scr(Wix).Alinhamento = Alinhamento;
                 IF (%PARMS > 4);
                    Var#Sess(WSessID).Lst#Scr(Wix).EdtCode = EditCode;
                    IF (%PARMS > 5);
                    // Var#Sess(WSessID).Lst#Scr(Wix).Tabela = Tabela;
                    ENDIF;
                 ENDIF;
              ENDIF;
           ENDIF;
        ENDIF;
        Var#Sess(WSessID).Lst#Scr(Wix).isEmpty = *OFF;
        Var#Sess(WSessID).Lst#Scr(Wix+1).isEmpty = *ON;
        Var#Sess(WSessID).QtdFldScr = Wix;
      /END-FREE
     P $SetLstFld      E

     P*=======================================================
     P* Processo..: $GetLstHdr
     P* Descrição.: Adicionar Parâmetros ao campo da Listagem
     C*=======================================================
     P $GetLstHdr      B                   EXPORT
     D                 PI            75A
     D WIdx            S              5S 0
     D WCampo          S             20A
     D WLabel          S             25A
     D WLargura        S              3S 0
     D WAlinham        S              1A
     D WEditCode       S              1A
     D WPosActual      S              3S 0 INZ(*ZEROS)
     D WPosIni         S              3S 0 INZ(*ZEROS)
     D WResult         S             75A   INZ(*BLANKS)
     D*===========================================================
      /FREE
        WResult = *BLANKS;
        WPosIni = 1;
        WIdx = 1;
        DOW (NOT Var#Sess(WSessID).Lst#Scr(WIdx).isEmpty);
           WCampo = Var#Sess(WSessID).Lst#Scr(WIdx).Field;
           WLabel = Var#Sess(WSessID).Lst#Scr(WIdx).Label;
           WLargura = Var#Sess(WSessID).Lst#Scr(WIdx).Largura;
           WAlinham  = Var#Sess(WSessID).Lst#Scr(WIdx).Alinhamento;
           WEditCode = Var#Sess(WSessID).Lst#Scr(WIdx).EdtCode;

           MONITOR;
              %SUBST(WResult:WPosIni:WLargura) =
                          $AlignText(Wlabel:WLargura:WAlinham);
           ON-ERROR;
              RETURN WResult;
           ENDMON;

           WIdx += 1;
           WPosIni += WLargura+1;
        ENDDO;
        RETURN WResult;
      /END-FREE
     P $GetLstHdr      E

     P*=======================================================
     P* Processo..: $GetLstLine
     P* Descrição.: Obter o registo da listagem
     C*=======================================================
     P $GetLstLine     B                   EXPORT
     D                 PI            75A
     D WIdx            S              5S 0
     D WCampo          S             20A
     D WSubstCar       S              1A
     D WMascara        S             20A
     D WDimCampo       S              3S 0
     D WResultado      S             80A
     D WIcMascara      S               N
     D WCodTabela      S              5S 0
     D WZeros          S             80A
     D WLabel          S             25A
     D WLargura        S              3S 0
     D WAlinham        S              1A
     D WEditCode       S              1A
     D WFieldValue     S            100A
     D WPosIni         S              3S 0 INZ(*ZEROS)
     D WResult         S             75A   INZ(*BLANKS)
     D*===========================================================
      /FREE
        WResult = *BLANKS;
        WPosIni = 1;
        WIdx = 1;
        DOW (NOT Var#Sess(WSessID).Lst#Scr(WIdx).isEmpty);
           WCampo = Var#Sess(WSessID).Lst#Scr(WIdx).Field;
           WLabel = Var#Sess(WSessID).Lst#Scr(WIdx).Label;
           WLargura = Var#Sess(WSessID).Lst#Scr(WIdx).Largura;
           WAlinham  = Var#Sess(WSessID).Lst#Scr(WIdx).Alinhamento;
           WEditCode = Var#Sess(WSessID).Lst#Scr(WIdx).EdtCode;

           MONITOR;
              WFieldValue = $GetField(WCampo);
              WFieldValue = $AlignText(%TRIM(WFieldValue):WLargura:WAlinham);
              %SUBST(WResult:WPosIni:WLargura) = WFieldValue;
           ON-ERROR;
              RETURN WResult;
           ENDMON;

           //---------------------------------------------------------
           // Aplicar mascara de edição
           //---------------------------------------------------------
           WIcMascara = *OFF;
           WZeros     = *ZEROS;
           WResultado = %SUBST(WResult:WPosIni:WLargura);
           WMascara   = *BLANKS;
           WSubstCar  = *BLANKS;
           WDimCampo  = %LEN(%TRIM(%SUBST(WResult:WPosIni:WLargura)));
           IF (Var#Sess(WSessID).Lst#Scr(WIdx).EdtCode = 'L'); // DD-MM-AAAA
              WSubstCar = '-';
              IF (WdimCampo < 8);
                 WResultado = '0'+%TRIM(WResultado);
              ENDIF;
              WIcMascara = *ON;
              WMascara   = '##-##-####';
           ELSEIF (Var#Sess(WSessID).Lst#Scr(WIdx).EdtCode = 'D'); // AAAA-MM-DD
              WSubstCar = '-';
              WIcMascara = *ON;
              WMascara = '####-##-##';
           ELSEIF (Var#Sess(WSessID).Lst#Scr(WIdx).EdtCode = 'A'); // Código da ADC
              WIcMascara = *ON;
              WMascara = '######-##';
           ELSEIF (Var#Sess(WSessID).Lst#Scr(WIdx).EdtCode = 'R'); // Recibo
              WMascara = '##.##.#####';
           ELSEIF (Var#Sess(WSessID).Lst#Scr(WIdx).EdtCode = 'M'); // Importancia monteria
              IF (%SUBST(WResultado:WLargura-2:1) = ',');
                 WResultado = %SUBST(WResultado:1:WLargura-3) +
                                %SUBST(WResultado:WLargura-1:2);
              ENDIF;
              WIcMascara = *ON;
              WMascara   = '##.###.###.###,##';
           ELSEIF (Var#Sess(WSessID).Lst#Scr(WIdx).EdtCode = 'C'); // Apresentar descrição
              WCodTabela = Var#Sess(WSessID).Lst#Scr(WIdx).Tabela;
           // WResultado = $GetDescricao(WCodTabela:%TRIM(WResultado));
              WResultado = WResultado;
           ENDIF;
           IF (WIcMascara);
              WResultado = $SetTextMask(WResultado:WMascara:WSubstCar);
              WZeros     = *BLANKS;
              WDimCampo  = %LEN(%TRIM(WResultado));
              IF (WLargura > WDimCampo);
                 WResultado = %SUBST(WZeros:1:(WLargura-WDimCampo)) +
                              %TRIM(WResultado);
              ENDIF;
           ENDIF;
           %SUBST(WResult:WPosIni:WLargura) = %SUBST(WResultado:1:WLargura);
           WIdx += 1;
           WPosIni += WLargura + 1;
        ENDDO;
        RETURN WResult;
      /END-FREE
     P $GetLstLine     E
