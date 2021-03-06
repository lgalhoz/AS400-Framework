     H*=======================================================
     H* Dat.Cria...: 2012-03-29
     H* Autor......: Luis Galhoz
     H* Biblioteca.: SCATO010
     H* Objectivo..: Fun��es de Catalogo de Produto
     H*=======================================================
     D*==> Obter Valores para a Cobertura
     D $getCobValor    PR            11S 2
     D    CdProd                      5S 0 CONST
     D    CdRamo                      5S 0 Const
     D    NuApol                      7S 0 CONST
     D    CdCobert                    5S 0 Const
     D    CdTipo                      2A   Const
     D
     D*==> Obter o C�digo do Produto
     D $GetCodProduto  PR             5S 0
     D    CdRamo                      5S 0 Const
     D    CdVariante                  5S 0 Const Options(*NOPASS)
     D
     D*==> Obter a descri��o do Ramo
     D $GetDescRamo    PR           100A
     D    CdRamo                      5S 0 Const
     D    CdVariante                  5S 0 Const Options(*NOPASS)
     D
     D $GetDescRisco   PR           100A
     D    CdProduto                   5S 0 Const
     D    CdRisco                     5S 0 Const
     D
     D*==> Obter a Comiss�o Standard para o Ramo
     D $GetComissao    PR             5S 3
     D    CdRamo                      5S 0 Const
     D    CdVariante                  5S 0 Const Options(*NOPASS)
     D    TpComissao                  1A   Const Options(*NOPASS)
     D
     D*==> Pop-up de Riscos associados ao Produto
     D $RunRiskPopUp   PR             5S 0
     D    RCdProd                     5S 0 CONST
     D    RCdRisco                    5S 0 CONST
     D
     D*==> Devolve a Fun��o para Gerir os dados da Ap�lice
     D $getAppApol     PR            10A
     D    RCdProd                     5S 0 Const
     D
     D*==> Validar se o Produto � para Migrar
     D $MigrarProd     PR             1A
     D    RCdProd                     5S 0 Const
     D
     D*==> Validar se o Item � index�vel
     D $IsIndexavel    PR              N
     D    RCdProd                     5S 0 Const
     D    RCdItem                     5S 0 Const
     D    Reset                        N   Const Options(*NoPass)
