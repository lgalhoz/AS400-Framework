     H*=======================================================
     H* Dat.Cria...: 2012-03-29
     H* Autor......: Luis Galhoz
     H* Biblioteca.: RDBSO010
     H* Objectivo..: Simplificação das operações de acesso aos
     H*              Utilizadas em AS/400 ILE (Cobol, RPG, ...)
     H*=======================================================
     D*==> Formatar a Intrução SQL
     D $SetDBSession   PR
     D    SessionId                   5S 0 Const
     D    InzSession                   N   Const Options(*NOPASS)
     D $GetDBSession   PR             5S 0
     D $AddDBSession   PR             5S 0
     D $RmvDBSession   PR
     D    SessionId                   5S 0 Const Options(*NOPASS)
     D $InzDBSession   PR
     D    SessionId                   5S 0 Const
     D*==> Formatar a Intrução SQL
     D $setquery       PR
     D    Fields                    300A   Const
     D    Files                     300A   Const
     D    Where                     300A   Const Options(*NOPASS)
     D    OrderBy                   100A   Const Options(*NOPASS)
     D
     D*==> Definir ficheiros a usar
     D $GetParametro   PR           300A
     D    Tipo                        1A   Const
     D
     D*==> Definir ficheiros a usar
     D $SetFiles       PR
     D    Files                     300A   Const
     D    IsLogical                    N   Const Options(*NOPASS)
     D    Membro                     10A   Const Options(*NOPASS)
     D
     D*==> Definir Campos a usar
     D $SetFields      PR
     D    Fields                    300A   Const
     D $AddField       PR
     D    FldName                    20A   Const
     D    FldType                     1A   Const Options(*NOPASS)
     D    FldCode                    20A   Const Options(*NOPASS)
     D    FldTable                    5S 0 Const Options(*NOPASS)
     D
     D*==> Definir Ordenação dos dados
     D $SetSort        PR
     D    OrderBy                   100A   Const
     D
     D*==> Posicionar no Primeiro Registo
     D $GetFirst       PR
     D $GetLast        PR
     D $GetNext        PR
     D $GetPrevious    PR
     D
     D*==> Obter a última chave disponível
     D $GetNextKey     PR            10S 0
     D    Tabela                     40A   Const
     D    Campo                      20A   Const
     D    Criteria                  100A   Const Options(*NOPASS)
     D
     D*==> Definir Critério de Selecção
     D $GetCritText    PR            60A
     D $ChgCriteria    PR
     D $AddCriteria    PR
     D    Label                      60A   Const
     D    Field                      20A   Const
     D    Compar                      2A   Const Options(*NOPASS)
     D    Select                       N   Const Options(*NOPASS)
     D
     D $GetCriteria    PR           300A
     D $SetCriteria    PR
     D    Value                     300A   Const
     D    Intern                       N   Const Options(*NOPASS)
     D
     D $SetRepetidos   PR
     D    Repetidos                   1A   Const
     D
     D*==> Estrutura das Páginas
     D $SetPage        PR
     D    WQtdRegisto                 5S 0 Const
     D $GetPageNbr     PR             5S 0
     D $IsLastPage     PR              N
     D $GetPage        PR             5S 0
     D   PrmAction                    1A   Const
     D   Default                      1A   Const Options(*NOPASS)
     D
     D*==> Obter os registos
     D $GetRecord      PR             5S 0
     D    RegIni                      9S 0 Const
     D    QtdReg                      3S 0 Const
     D    IsLogical                    N   Const Options(*NOPASS)
     D    Reset                        N   Const Options(*NOPASS)
     D
     D*==> Obter o valor para o Campo
     D $GetField       PR           150A
     D    FldName                    15A   Const
     D
     D*==> Validar se chegou ao fim do Registo
     D $IsEof          PR              N
     D $GetQtdeReg     PR             9S 0
     D
     D*==> Formatar a Intrução SQL
     D $getSQLStmt     PR          2000A
     D    Fields                    300A   Const
     D    Files                     300A   Const
     D    Where                     300A   Const
     D    OrderBy                   100A   Const
     D    Repetidos                   1A   Const
     D    PosIni                     10P 0 Const
     D    Qtde                        3P 0 Const Options(*NOPASS)
     D    Resumo                       N   Const Options(*NOPASS)
     D    IsLogical                    N   Const Options(*NOPASS)
     D
     D*==> Formatar os campos da Lista de ecrã
     D $SetLstFld      PR
     D Campo                         20A   CONST
     D Label                         25A   CONST Options(*NOPASS)
     D Dimensao                       3S 0 CONST Options(*NOPASS)
     D Alinhamento                    1A   CONST Options(*NOPASS)
     D EditCode                       1A   CONST Options(*NOPASS)
     D
     D*==> Obter o cabeçalho da Listagem
     D $GetLstHdr      PR            75A
     D
     D*==> Obter a Linha da Listagem
     D $GetLstLine     PR            75A
     D
