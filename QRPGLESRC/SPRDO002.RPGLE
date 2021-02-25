     D*=======================================================
     D* Dat.Cria...: 2012-05-01
     D* Autor......: Luis Galhoz
     D* Biblioteca.: SPRDO002
     D* Objectivo..:
     D*=======================================================
     D*==> Parametros recebidos
     D WRamo           S              2S 0
     D WApolice        S              7S 0
     D WDatInicio      S              8S 0
     D WpLinhasTxt     S               *
     D
     D
     D*==> Capitais de Risco
     D $GetCapRisco    PR                  LIKEDS(LST#Capital) DIM(25)
     D   CodProduto                   5S 0 CONST
     D   CodRamo                      2S 0 CONST
     D   NumApolice                   7S 0 CONST
     D   CodRisco                     5S 0 CONST Options(*NOPASS)
     D   IcRefresh                     N   CONST Options(*NOPASS)
     D   DatCapital                   8S 0 CONST Options(*NOPASS)
     D
     D*==> Estrutura da lista de Capitais
     D LST#Capital     DS                  QUALIFIED
     D   CodRisco                     5S 0 INZ(*ZEROS)
     D   CapRisco                    11S 2 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Obter os limites de Indemnização
     D $GetLimites     PR                  LIKEDS(LST#Limite)
     D   CodProduto                   5S 0 CONST
     D   CodRamo                      2S 0 CONST
     D   NumApolice                   7S 0 CONST
     D   CodCobert                    5S 0 CONST
     D   CodLimite                    5S 0 CONST Options(*NOPASS)
     D   IcRefresh                     N   CONST Options(*NOPASS)
     D   DatCapital                   8S 0 CONST Options(*NOPASS)
     D
     D*==> Estrutura dos Limites
     D LST#Limite      DS                  QUALIFIED
     D   CDCOBERT                     5S 0 INZ(*ZEROS)
     D   CDRISCO                      5S 0 INZ(*ZEROS)
     D   CDINCIND                     2A   INZ(*BLANKS)
     D   VLPERIND                     5S 2 INZ(*ZEROS)
     D   VLMAXIND                    11S 2 INZ(*ZEROS)
     D   ICESCINF                      N   INZ(*OFF)
     D   CDINCFRQ                     2A   INZ(*BLANKS)
     D   VLPERFRQ                     5S 2 INZ(*ZEROS)
     D   VLMINFRQ                    11S 2 INZ(*ZEROS)
     D   ICESCFRQ                      N   INZ(*OFF)
     D   IsEmpty                       N   INZ(*ON)
     D*----------------------------------------------
     D*  Obter o Capital Alfanumérico
     D*----------------------------------------------
     D $GetCapitalAlf  PR            15A
     D   CodProduto                   5S 0 CONST
     D   CodRamo                      2S 0 CONST
     D   NumApolice                   7S 0 CONST
     D   CodTipo                      1A   CONST
     D   CodCobert                    5S 0 CONST
     D   CodLimite                    5S 0 CONST Options(*NOPASS)
     D   IcRefresh                     N   CONST Options(*NOPASS)
     D   DatInicio                    8S 0 CONST Options(*NOPASS)
     D
     D*----------------------------------------------
     D*  Obter o Capital Numérico
     D*----------------------------------------------
     D $GetCapitalNum  PR            15S 2
     D   CodProduto                   5S 0 CONST
     D   CodRamo                      2S 0 CONST
     D   NumApolice                   7S 0 CONST
     D   CodTipo                      1A   CONST
     D   CodCobert                    5S 0 CONST
     D   CodLimite                    5S 0 CONST Options(*NOPASS)
     D   IcRefresh                     N   CONST Options(*NOPASS)
     D   DatInicio                    8S 0 CONST Options(*NOPASS)
     D
     D*==> Obter a Franquia para o edificio
     D $GetEdifAgrava  PR            13S 2
     D    Ano                         4S 0 Const
