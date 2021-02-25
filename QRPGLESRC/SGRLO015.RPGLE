     D*--> Lista de Trabalho de Ficheiros
     D Fil#Info        DS                  QUALIFIED
     D   Date                         8S 0 INZ(*ZEROS)
     D   File                        10A   INZ(*BLANKS)
     D   Lib                         10A   INZ(*BLANKS)
     D
     D*=================================================================
     D* Funções de Histórico
     D*=================================================================
     D*--> Inicalizar lista de Histórico
     D $InzListHS      PR
     D
     D*--> Carregar ficheiros de Histórico
     D $FillListHS     PR
     D    LibName                    10A   Const
     D    FileName                   10A   Const
     D
     D*--> Obter o primeiro registo da Lista
     D $GetFirstHS     PR                  LIKEDS(Fil#Info)
     D
     D*--> Obter o regito seguinte
     D $GetNextHS      PR                  LIKEDS(Fil#Info)
     D
     D*--> Validar se chegou ao ultimo registo da Lista
     D $EofHS          PR              N
     D
     D*--> Ordenar a Lista
     D $SortHS         PR
     D
     D*--> Procurar uma palavra num qualquer ficheiro de dados AS/400
     D $FindRec        PR              N
     D    LibName                    10A   Const
     D    FileName                   10A   Const
     D    Chave                      20A   Const
     D    PosInic                     5S 0 Const
     D
