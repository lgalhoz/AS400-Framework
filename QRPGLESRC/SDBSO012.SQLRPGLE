     H*=======================================================
     H* Dat.Cria...: 2013-04-23
     H* Autor......: Luis Galhoz
     H* Biblioteca.: RDBSO012
     H* Objectivo..: Controlo de Consolidações
     H*=======================================================
     D*==> Validar se existem transacções pendentes
     D $isCMTPending   PR              N
     D
     D*==> Obter o estado do controlo de consolidações
     D $getCMTStatus   PR              N
     D
     D*==> Obter o Ambito (JOB ou ACTGROUP)
     D $getCMTAmbito   PR             8A
     D
     D*==> Terminar o Controlo de Consolidações
     D $endCommitment  PR              N
     D
     D*==> Iniciar o Controlo de Consolidações
     D $strCommitment  PR              N
     D     Ambito                     8A   Const Options(*NOPASS)
     D     Diario                    10A   Const Options(*NOPASS)
     D     Biblioteca                10A   Const Options(*NOPASS)
     D
     D*==> Confirmar se é para Continuar ou cancelar (uma janela)
     D $ConfAltera     PR             1A
