     D*======================================================
     D*
     D*  Grupo de funções de Segurança
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
     D*==> Obter o Nome de um programa a partir do Código
     D $GetFunction    PR            10A
     D  NoFunction                    5S 0 CONST
     D
     D*==> Validar se o Programa é válido de acordo com a Template
     D $ValTemplate    PR              N
     D  CdTemplate                    5S 0 CONST
     D  CdProgr                       5S 0 CONST
     D
     D*==> Obter o Cabeçalho da Função
     D $GetHeader      PR           114A
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D   DS#HEADER     DS
     D   ECR_DS#PGM            1     57A
     D   ECR_DS#FNC           58    114A
     D
     D*==> Obter as teclas de função autorizadas
     D $GetFooter      PR           156A
     D   AppName                     10A   CONST
     D   Function                    10A   CONST
     D   Hide                        24A   CONST
     D   DS#FOOTER     DS
     D   ECR_DS#KEY01          1     78A
     D   ECR_DS#KEY02         79    156A
     D
     D*==> Obter as opções disponíveis para a Barra
     D $GetOption      PR           152A
     D   AppName                     10A   CONST
     D   Function                    10A   CONST
     D   Hide                        24A   CONST
     D   DS#OPTIONS    DS
     D   ECR_DS#OPT01          1     76A
     D   ECR_DS#OPT02         77    152A
     D
     D*==> Validar se Está autorizado à função
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
     D*==> Validar se a opção pode ser executada
     D $ValidOption    PR              N
     D  Selected                      2A   CONST
     D  Option                        2A   CONST
     D  Valor                        50A   CONST Options(*NOPASS)
     D  AppName                      10A   CONST Options(*NOPASS)
     D  Function                     10A   CONST Options(*NOPASS)
     D*======================================================
     D*
     D*  Grupo de funções de Auditoria
     D*
     D*======================================================
     D $MakeKey        PR
     D  Parm001                      10A   CONST
     D  Parm002                      10A   CONST Options(*NOPASS)
     D  Parm003                      10A   CONST Options(*NOPASS)
