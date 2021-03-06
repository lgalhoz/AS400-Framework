     D*======================================================
     D*
     D*  Grupo de fun��es de Seguran�a
     D*
     D*======================================================
     D $RunPopUpComp   PR             5S 0
     D    RCodigo                     5S 0 CONST
     D $RunPopUpTempl  PR             5S 0
     D    RTemplate                   5S 0 CONST
     D    RCodigo                     5S 0 CONST
     D $GetDescFunct   PR            50A
     D    RCodigo                     5S 0 CONST
     D
     D*==> Obter o Nome de um programa a partir do C�digo
     D $GetFunction    PR            10A
     D  NoFunction                    5S 0 CONST
     D
     D*==> Validar se o Programa � v�lido de acordo com a Template
     D $ValTemplate    PR              N
     D  CdTemplate                    5S 0 CONST
     D  CdProgr                       5S 0 CONST
     D
     D*==> Obter o Cabe�alho da Fun��o
     D $GetHeader      PR           114A
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D   DS#HEADER     DS
     D   ECR_DS#PGM            1     57A
     D   ECR_DS#FNC           58    114A
     D
     D*==> Teclas de fun��o a omitir na sess�o
     D $KeysToOmit     PR
     D   AppName                     10A   CONST
     D   Function                    10A   CONST
     D   Hide                        24A   CONST
     D
     D*==> Obter as teclas de fun��o autorizadas
     D $GetFooter      PR           156A
     D   AppName                     10A   CONST
     D   Function                    10A   CONST
     D   Hide                        24A   CONST Options(*NOPASS)
     D   DS#FOOTER     DS
     D   ECR_DS#KEY01          1     78A
     D   ECR_DS#KEY02         79    156A
     D
     D*==> Op��es a omitir na sess�o
     D $OptionsToOmit  PR
     D   AppName                     10A   CONST
     D   Function                    10A   CONST
     D   Hide                        24A   CONST
     D
     D*==> Obter as op��es dispon�veis para a Barra
     D $GetOption      PR           152A
     D   AppName                     10A   CONST
     D   Function                    10A   CONST
     D   Hide                        24A   CONST Options(*NOPASS)
     D   DS#OPTIONS    DS
     D   ECR_DS#OPT01          1     76A
     D   ECR_DS#OPT02         77    152A
     D
     D*==> Validar se Est� autorizado � fun��o
     D $ValAccess      PR              N
     D  AppName                      10A   CONST
     D  Function                     10A   CONST Options(*NOPASS)
     D  InVal                         5S 0 CONST Options(*NOPASS)
     D  TipOption                     1A   CONST Options(*NOPASS)
     D
     D*==> Validar se a tecla pode ser executada
     D $ValidKey       PR              N
     D  PressedKey                    1A   CONST
     D  ValidKey                      1A   CONST
     D  Valor                        50A   CONST Options(*NOPASS)
     D  AppName                      10A   CONST Options(*NOPASS)
     D  Function                     10A   CONST Options(*NOPASS)
     D
     D*==> Validar se a op��o pode ser executada
     D $ValidOption    PR              N
     D  Selected                      2A   CONST
     D  Option                        2A   CONST
     D  Valor                        50A   CONST Options(*NOPASS)
     D  AppName                      10A   CONST Options(*NOPASS)
     D  Function                     10A   CONST Options(*NOPASS)
     D*======================================================
     D*
     D*  Grupo de fun��es de Auditoria
     D*
     D*======================================================
     D $MakeKey        PR
     D  Parm001                      10A   CONST
     D  Parm002                      10A   CONST Options(*NOPASS)
     D  Parm003                      10A   CONST Options(*NOPASS)
     D*======================================================
     D*
     D*  Grupo de fun��es de Controlo Cadeias Batch
     D*
     D*======================================================
     D*==> Validar Execu��o da fun��o na Cadeia Batch
     D $ValBatch       PR              N
     D  AppName                      10A   CONST
     D  NomBatch                      5A   CONST
     D
     D*==> Adicionar ficheiro Input usado pela Fun��o
     D $AddBatchInp    PR              N
     D  CodBatch                      9S 0 CONST
     D  CodFile                       5S 0 CONST
     D  CodFuncao                     5S 0 CONST
     D
     D*==> Adicionar ficheiro output gerado pela Fun��o
     D $AddBatchOut    PR              N
     D  NomFile                      10A   CONST
     D  QtdeRegis                     9S 0 CONST
     D  CodAcction                    1A   CONST Options(*NOPASS)
     D
     D*==> Actualizar informa��o do Processo Batch
     D $ChgBatchStat   PR              N
     D  DatAntiga                      D   CONST Options(*NOPASS)
     D  DatRecente                     D   CONST Options(*NOPASS)
     D
     D*==> Devolver informa��o do Processo
     D $getBatchSett   PR              D
     D  TipData                       1A   CONST
     D
     D*==> Remover um Processo Batch
     D $rmvBatch       PR              N
