     D LST#TmpFile     DS                  QUALIFIED
     D    CodFile                     5S 0 INZ(*ZEROS)
     D    NomLibl                    10A   INZ(*BLANKS)
     D    NomFile                    10A   INZ(*BLANKS)
     D    AnoMes                      6S 0 INZ(*ZEROS)
     D    CodLetra                    1A   INZ(*BLANKS)
     D    DatCria                     8S 0 INZ(*ZEROS)
     D    HorCria                     6S 0 INZ(*ZEROS)
     D    IcHistor                    1A   INZ(*BLANKS)
     D    IsEmpty                      N   INZ(*ON)
     D
     D*==> Lista de Valores a Pesquisar
     D LST#ValField    DS                  QUALIFIED
     D    ValField                   20A   INZ(*BLANKS)
     D    CodField                    9S 0 INZ(*ZEROS)
     D    IsEmpty                      N   INZ(*ON)
     D
     D*==> Processo de Pesquisa Metadados
     D $GetTmpFiles    PR                  LIKEDS(LST#TmpFile) DIM(9999)
     D   ValFields                         CONST LIKEDS(LST#ValField) DIM(10)
     D   Ordenar                       N   CONST OPTIONS(*NOPASS)
     D
