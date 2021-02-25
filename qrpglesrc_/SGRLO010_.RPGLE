     D PSDS           SDS
     D   S#PGM                 1     10
     D   S#JOB               244    253
     D   S#USR               254    263
     D   S#JOB#              264    269  0
     D
     D*======================================================
     D*
     D*  Grupo de funções de Data e Hora
     D*
     D*======================================================
     D*==> Converter formatos de data
     D $CvtData        PR             8S 0
     D    DataIn                      8S 0 Const
     D    FmtIn                       4A   Const
     D    FmtOut                      4A   Const
     D                                     Options(*NOPASS)
     D
     D*==> Aplicar Mascaras de Edição
     D $ApplyDatFmt    PR            10A
     D    Data                        8S 0 Const
     D    FmtDatIn                    4A   Const
     D    Separador                   1A   Const
     D                                     Options(*NOPASS)
     D    FmtDatOut                   4A   Const
     D                                     Options(*NOPASS)
     D
     D
     D*==> Adicionar ou Subtrair valores a uma data
     D $addToDate      PR             8S 0
     D   Data                         8S 0 Const
     D   Qtde                         4S 0 Const
     D   Tipo                         1A   Const
     D                                     Options(*NOPASS)
     D
     D*==> Obter as datas correntes de Inicio ou Termo do Contrato
     D $findDate       PR             8S 0
     D   DataInicio                   8S 0 Const
     D   Tipo                         1A   Const
     D   DataEfeito                   8S 0 Const
     D                                     Options(*NOPASS)
     D
     D*======================================================
     D*
     D*  Grupo de funções do Sistema Operativo
     D*
     D*======================================================
     D*==> Obter Informação sobre o Utilizador
     D $getUserName    PR            50A
     D $getDefPrint    PR            10A
     D $getHomeDir     PR           100A
     D
     D*==> Invocar comandos de AS/400
     D $shellExec      PR              N
     D   StdCmd                     300A   CONST OPTIONS(*VARSIZE)
     D
     D*==> Validar a existência de um objecto
     D $checkObject    PR              N
     D PRM_ObjNam                    10A   Const
     D PRM_ObjLib                    10A   Const
     D PRM_ObjTyp                    10A   Const Options(*NOPASS)
     D
     D*==> Obter a localização de um campo
     D $getFldLoc      PR              N
     D  QDspFil                      10A   Const
     D  RcdFmt                       10A   Const
     D  FldNam                       10A   Const
     D  Row                           3P 0
     D  Col                           3P 0
     D
     D*==> Enviar uma mensagem para rodapé
     D $sendMessage    PR
     D   PRM_CodMsg                   7A   CONST
     D
     D*==> Obter o texto para a mensagem
     D $getMessage     PR           100A
     D   PRM_CodMsg                   7A   CONST
     D   PRM_Tipo                     1A   Const Options(*NOPASS)
     D
     D*==> Teclas de Função
     D KEY#F01         C                   x'31'
     D KEY#F02         C                   x'32'
     D KEY#F03         C                   x'33'
     D KEY#F04         C                   x'34'
     D KEY#F05         C                   x'35'
     D KEY#F06         C                   x'36'
     D KEY#F07         C                   x'37'
     D KEY#F08         C                   x'38'
     D KEY#F09         C                   x'39'
     D KEY#F10         C                   x'3a'
     D KEY#F11         C                   x'3b'
     D KEY#F12         C                   x'3c'
     D KEY#F13         C                   x'b1'
     D KEY#F14         C                   x'b2'
     D KEY#F15         C                   x'b3'
     D KEY#F16         C                   x'b4'
     D KEY#F17         C                   x'b5'
     D KEY#F18         C                   x'b6'
     D KEY#F19         C                   x'b7'
     D KEY#F20         C                   x'b8'
     D KEY#F21         C                   x'b9'
     D KEY#F22         C                   x'ba'
     D KEY#F23         C                   x'bb'
     D KEY#F24         C                   x'bc'
     D KEY#Enter       C                   x'f1'
     D KEY#Help        C                   x'f3'
     D KEY#PgUp        C                   x'f4'
     D KEY#PgDown      C                   x'f5'
     D KEY#Print       C                   x'f6'
     D
     D*======================================================
     D*
     D*  Grupo de funções de Segurança
     D*
     D*======================================================
     D*==> Obter o Nome de um programa a partir do Código
     D $GetFunction    PR            10A
     D  NoFunction                    5S 0 CONST
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

     D  INFD#KEYS      DS
     D*    All#DS              1    684
     D     Func#Key          369    369
     D     Cursor#Loc        370    371
     D        CM#CROW                 3i 0 OVERLAY(Cursor#Loc)
     D        CM#CCOL                 3i 0 OVERLAY(Cursor#Loc:*NEXT)
     D     Cursor#Set        382    383
     D        SF#CROW                 3i 0 OVERLAY(Cursor#Set)
     D        SF#CCOL                 3i 0 OVERLAY(Cursor#Set:*NEXT)

     D   DS#FOOTER     DS
     D   ECR_DS#KEY01          1     78A
     D   ECR_DS#KEY02         79    156A
     D
     D*==> Validar se Está autorizado à função
     D $ValAccess      PR              N
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D  InVal                         5S 0 CONST
     D
     D*======================================================
     D*  Grupo de funções para tratamento e transformação de
     D*  Texto.
     D*======================================================
     D* Substituir texto por palavras
     D $strReplace     PR          1000A
     D    #What                     100A   Const
     D    #With                    1000A   Const
     D    #Where                   1000A   Const
     D
     D* Segmentar uma linha de Textos
     D $SetTexto       PR
     D    Texto                    1000A   Const
     D    Dimensao                    3S 0 Const
     D    Alinhar                     1A   Const Options(*NOPASS)
     D $EOFText        PR              N
     D $GetFirstLine   PR           150A
     D  PRM_Posiciona                  N   CONST Options(*NOPASS)
     D $GetNextLine    PR           150A
     D
     D* Aplicar uma mascara de edição
     D $SetTextMask    PR           150A
     D    Texto                     130A   Const
     D    Mascara                    20A   Const
     D    DftChar                     1A   Const
     D $AlignText      PR           150A
     D    Texto                     150A   Const
     D    Dimensao                    3S 0 Const
     D    Alinha                      1A   Const
     D
     D*======================================================
     D*
     D* Funções Específicas dos Ecrãs SDA/400
     D*
     D*======================================================
