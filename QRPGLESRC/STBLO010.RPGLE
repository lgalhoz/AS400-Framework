     H*=======================================================
     H* Dat.Cria...: 2012-03-29
     H* Autor......: Luis Galhoz
     H* Biblioteca.: SENTO010
     H* Objectivo..: Fun��es de Entidades
     H*=======================================================
     D*==> Pop-up de Pessoas e Empresas
     D $RunCPPopUp     PR             8S 0
     D    RCodigo                     8S 0 CONST
     D
     D*==> Descri��o do C�digo Pessoa e Empresa
     D $GetCodPost     PR            40A
     D    RCodigo                     8S 0 CONST
     D
     D*==> Validar a estrutura de um c�digo
     D $ValCodStruct   PR              N
     D    Tabela                      5S 0 CONST
     D    Codigo                     15A   CONST
     D
     D*==> Validar um C�digo
     D $valCodigo      PR              N
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D   PAlias                        N   CONST Options(*NOPASS)
     D $valCodigoTbl   PR              N
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D   PAlias                        N   CONST Options(*NOPASS)
     D
     D*==> Obter a descri��o para o C�digo
     D $getDescricao   PR            40A
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D   PAlias                        N   CONST Options(*NOPASS)
     D
     D*==> Obter a descri��o para o C�digo
     D $getDescAbrev   PR            40A
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D   PAlias                        N   CONST Options(*NOPASS)
     D
     D*==> Obter o ALIAS de um C�digo
     D $getCodigo      PR            15A
     D    PCdIndice                   5S 0 CONST
     D    PCodigo                    15A   CONST
     D    PAlias                       N   CONST Options(*NOPASS)
     D
     D*==> Pop-up de valores poss�veis
     D $runPopUp       PR            15A
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D   PModo                         N   CONST Options(*NOPASS)
     D
     D*==> Pop-up de valores poss�veis
     D $ExeAjuda       PR
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D
