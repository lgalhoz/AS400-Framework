     H*=======================================================
     H* Dat.Cria...: 2014-08-28
     H* Biblioteca.: (INT) Interfaces
     H* Descrição..: Funções de Interfaces dos Mediadores
     H*=======================================================
     D*==> Iniciar uma sessão de Interfaces para o Mediador
     D $StrIntMedia    PR             7A
     D   CodMedia                     9S 0 CONST
     D   CodProcess                   5S 0 CONST
     D   CodFile                      5S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Terminar a sessão de Interfaces para o Mediador
     D $EndIntMedia    PR             7A
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Posicionar-se na sessão do Mediador
     D $GetIntMedia    PR             3S 0
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Obter o estado de Erro da Interface
     D $GetIntMedErro  PR             7A
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Adicionar o registo
     D $AddIntRecMed   PR             7A
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Adicionar um Campo à lista
     D $AddFldIntRec   PR             7A
     D   CurSession                   3S 0 CONST
     D   Nome                        20A   CONST
     D   Valor                      100A   CONST
     D   Reset                         N   CONST OPTIONS(*NOPASS)
     D
     D*==> Obter o código (José Mata, Lda) do Mediador
     D $GetCodMedia    PR             3S 0
     D   CodMedia                     9S 0 CONST
     D
     D*==> Obter o código (José Mata, Lda) do Mediador
     D $CvtCode        PR           100A
     D   FieldValue                 100A   CONST
     D   FmtCode                      2A   CONST
     D   CvtCode                      2A   CONST
     D
     D*==> Obter a Referência de Multibanco
     D $GetRefMB       PR             9A
     D   NumRecibo                    9S 0 CONST
     D   TotalRecibo                 11S 2 CONST
