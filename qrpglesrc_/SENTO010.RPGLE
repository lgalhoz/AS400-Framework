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
