     H*=======================================================
     H* Dat.Cria...: 2012-03-29
     H* Autor......: Luis Galhoz
     H* Biblioteca.: SENTO010
     H* Objectivo..: Funções de Entidades
     H*=======================================================
     D*==> Pop-up de Pessoas e Empresas
     D $RunEntPopUp    PR             8S 0
     D    RCodigo                     8S 0 CONST
     D
     D*==> Pop-up de Credores Hipotecários
     D $RunCredPopUp   PR             8S 0
     D    RCodigo                     8S 0 CONST
     D
     D*==> Descrição do Código Pessoa e Empresa
     D $getDescEnt     PR            40A
     D    RCodigo                     8S 0 CONST
     D
     D*==> Descrição do Credor Hipotecário
     D $getDescCred    PR            40A
     D    RCodigo                     8S 0 CONST
     D*==>
     D $RunPSPopUp     PR             8S 0
     D  Codigo                        8S 0 CONST
     D  Ramo                          2S 0 Const Options(*NOPASS)
     D  Apolice                       7S 0 Const Options(*NOPASS)
     D  Tipo                          1A   Const Options(*NOPASS)
     D
     D $ChkPSCode      PR             8S 0
     D  Codigo                        8S 0 CONST
     D  Ramo                          2S 0 Const Options(*NOPASS)
     D  Apolice                       7S 0 Const Options(*NOPASS)
     D  Tipo                          1A   Const Options(*NOPASS)
     D
     D $CheckNIF       PR             7A
     D    NumNIF                      9S 0 CONST
     D $CheckNIB       PR             7A
     D    NumNIB                     21S 0 CONST
     D $CheckADC       PR             7A
     D    NumADC                     11S 0 CONST
     D $IsPenhora      PR              N
     D    NumNIF                      9S 0 CONST
     D $IsLstNegra     PR              N
     D    NumNIF                      9S 0 CONST
