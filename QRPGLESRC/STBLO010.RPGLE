     H*=======================================================
     H* Dat.Cria...: 2012-03-29
     H* Autor......: Luis Galhoz
     H* Biblioteca.: SENTO010
     H* Objectivo..: Funções de Entidades
     H*=======================================================
     D*==> Pop-up de Pessoas e Empresas
     D $RunCPPopUp     PR             8S 0
     D    RCodigo                     8S 0 CONST
     D
     D*==> Descrição do Código Pessoa e Empresa
     D $GetCodPost     PR            40A
     D    RCodigo                     8S 0 CONST
     D
     D*==> Validar a estrutura de um código
     D $ValCodStruct   PR              N
     D    Tabela                      5S 0 CONST
     D    Codigo                     15A   CONST
     D
     D*==> Validar um Código
     D $valCodigo      PR              N
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D   PAlias                        N   CONST Options(*NOPASS)
     D $valCodigoTbl   PR              N
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D   PAlias                        N   CONST Options(*NOPASS)
     D
     D*==> Obter a descrição para o Código
     D $getDescricao   PR            40A
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D   PAlias                        N   CONST Options(*NOPASS)
     D
     D*==> Obter a descrição para o Código
     D $getDescAbrev   PR            40A
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D   PAlias                        N   CONST Options(*NOPASS)
     D
     D*==> Obter o ALIAS de um Código
     D $getCodigo      PR            15A
     D    PCdIndice                   5S 0 CONST
     D    PCodigo                    15A   CONST
     D    PAlias                       N   CONST Options(*NOPASS)
     D
     D*==> Pop-up de valores possíveis
     D $runPopUp       PR            15A
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D   PModo                         N   CONST Options(*NOPASS)
     D
     D*==> Pop-up de valores possíveis
     D $ExeAjuda       PR
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D
