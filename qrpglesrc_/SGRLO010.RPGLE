     D PSDS           SDS
     D   S#PGM                 1     10
     D   S#PSTS               16     20  0
     D   S#MSGID              40     46
     D   S#JOB               244    253
     D   S#USR               254    263
     D   S#JOB#              264    269  0
     D   S#Loc           *ROUTINE
     D   S#Err           *STATUS
     D   S#Parms         *PARMS
     D   S#Name          *PROC
     D
     D*======================================================
     D*
     D*  Grupo de funções de Data e Hora
     D*
     D*======================================================
     D*==> Conversão de Tipos de Dados em Data
     D $DateToInt      PR             8S 0
     D    DataIn                       D   Const
     D
     D $IntToDate      PR              D
     D    DataIn                      8S 0 Const
     D
     D*==> Converter formatos de data
     D $CvtData        PR             8S 0
     D    DataIn                      8S 0 Const
     D    FmtIn                       4A   Const
     D    FmtOut                      4A   Const Options(*NOPASS)
     D
     D*==> Aplicar Mascaras de Edição
     D $ApplyDatFmt    PR            10A
     D    Data                        8S 0 Const
     D    FmtDatIn                    4A   Const
     D    Separador                   1A   Const Options(*NOPASS)
     D    FmtDatOut                   4A   Const Options(*NOPASS)
     D
     D*==> Adicionar ou Subtrair valores a uma data
     D $addToDate      PR             8S 0
     D   Data                         8S 0 Const
     D   Qtde                         4S 0 Const
     D   Tipo                         1A   Const Options(*NOPASS)
     D
     D*==> Obter as datas correntes de Inicio ou Termo do Contrato
     D $findDate       PR             8S 0
     D   DataInicio                   8S 0 Const
     D   Tipo                         1A   Const
     D   DataEfeito                   8S 0 Const Options(*NOPASS)
     D
     D*==> Validar dia util, devolve *OFF se for Feriado ou fim de Semana
     D $chkDUtil       PR              N
     D   Data                         8S 0 Const
     D   Local                        5S 0 Const Options(*NOPASS)
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
     D*==> Função para Comparar Palavras
     D $ValString      PR              N
     D    String01                   50A   CONST
     D    String02                   50A   CONST
     D    Rigor                       3S 2 CONST
     D
     D*==> Validar com que erro as palavras são iguais
     D $ValPrecisao    PR             3S 2
     D    String01                   50A   CONST
     D    String02                   50A   CONST
     D    Rigor                       3S 2 CONST
     D
     D*==> Transformar uma frase em minusculas
     D $strToLower     PR          1000A
     D    WTexto                   1000A   CONST
     D
     D*==> Transformar uma frase em maiusculas
     D $strToUpper     PR          1000A
     D    WTexto                   1000A   CONST
     D
     D*======================================================
     D*
     D* Processador de Texto
     D*
     D*======================================================
     D*==> Texto formatado (limitado a uma largura de 100)
     D #RetTexto       DS                  QUALIFIED DIM(1000)
     D   Texto                      100A
     D   IsEMpty                       N
     D
     D*==> Adicionar Excepção
     D $addException   PR
     D   DftException                  N   Const
     D
     D*==> Adicionar Excepção
     D $addLineType    PR
     D   DftLineType                  2A   Const
     D
     D*==> Inicializar a lista de Trabalho
     D $InzLista       PR
     D   Pagina                       5S 0 Const
     D
     D*==> Obter a Linhas de Texto a escrever
     D $GetLista       PR              *
     D
     D*==> Lista de Textos a escrever
     D $AddTextLine    PR
     D    Texto                     500A   Const Options(*NOPASS)
     D    Qtde                        3S 0 Const Options(*NOPASS)
     D    CdIdent                     8A   Const Options(*NOPASS)
     D    ImValor                    20A   Const Options(*NOPASS)
     D    CdMascara                   2A   Const Options(*NOPASS)
     D
     D*==> Obter númemo de linhas do Bloco de texto
     D $getQtdeLinhas  PR             5S 0
     D    NumRegisto                  5S 0 Const
     D
     D*==> Obter Titulo do Texto
     D $getLineHeader  PR           300A
     D    NumRegisto                  5S 0 Const
     D
     D*==> Popup de Estilos
     D $GetStyleDesc   PR           100A
     D    CdPagina                    5S 0 Const
     D    CdStyle                     2A   Const
     D
     D*==> Pop-up de Estilos associados ao Produto
     D $RunStylPopUp   PR             2A
     D    RCdProd                     5S 0 CONST
     D    RCdTipo                     2A   CONST
     D
     D*==> Construir Texto
     D $ProcessTexto   PR                  LikeDS(#RetTexto) DIM(1000)
     D    Tipo                        1A   Const Options(*NOPASS)
     D    Largura                     3S 0 Const Options(*NOPASS)
     D*======================================================
     D*
     D* Funções Específicas dos Ecrãs SDA/400
     D*
     D*======================================================
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
     D  INFD#KEYS      DS
     D*    All#DS              1    684
     D     Func#Key          369    369
     D     Cursor#Loc        370    371
     D        CM#CROW                 3i 0 OVERLAY(Cursor#Loc)
     D        CM#CCOL                 3i 0 OVERLAY(Cursor#Loc:*NEXT)
     D     Cursor#Set        382    383
     D        SF#CROW                 3i 0 OVERLAY(Cursor#Set)
     D        SF#CCOL                 3i 0 OVERLAY(Cursor#Set:*NEXT)
     D
     D*==> Obter as coordenadas do campo no ecrã
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
     D*======================================================
     D*
     D*  Incorporação de Funções em C++
     D*
     D*======================================================
     D $Random         PR            10i 0 EXTPROC('random')
     D $GetEnv         PR              *   EXTPROC('getenv')
     D   var                           *   VALUE OPTIONS(*string)
     D
     D $WrtStOut       PR                  EXTPROC('QtmhWrStout')
     D   DtaVar                   65535A   options(*varsize)
     D   DtaVarLen                   10I 0 const
     D   ErrorCode                 8000A   options(*varsize)
     D
     D $ErrDS          DS                  qualified
     D   bytesProv                   10i 0 inz(0)
     D   bytesAvail                  10i 0 inz(0)
     D
