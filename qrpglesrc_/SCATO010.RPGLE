     H*=======================================================
     H* Dat.Cria...: 2012-03-29
     H* Autor......: Luis Galhoz
     H* Biblioteca.: SCATO010
     H* Objectivo..: Funções de Catalogo de Produto
     H*=======================================================
     D*==> Obter o Código do Produto
     D $GetCodProduto  PR             5S 0
     D    CdRamo                      5S 0 Const
     D    CdVariante                  5S 0 Const Options(*NOPASS)
     D
     D*==> Obter a descrição do Ramo
     D $GetDescRamo    PR           100A
     D    CdRamo                      5S 0 Const
     D    CdVariante                  5S 0 Const Options(*NOPASS)
     D
     D $GetDescRisco   PR           100A
     D    CdProduto                   5S 0 Const
     D    CdRisco                     5S 0 Const
     D
     D*==> Obter a Comissão Standard para o Ramo
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
