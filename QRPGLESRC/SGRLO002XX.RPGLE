     D*=======================================================
     D* Dat.Cria...: 2012-05-01
     D* Autor......: Luis Galhoz
     D* Biblioteca.: SPRDO010
     D* Objectivo..: Estrutrua das Actas
     D*=======================================================
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
     D*==> Obter núermo de linhas do Bloco de texto
     D $getQtdeLinhas  PR             5S 0
     D    NumRegisto                  5S 0 Const
     D
     D*==> Obter Titulo do Texto
     D $getLineHeader  PR           300A
     D    NumRegisto                  5S 0 Const
     D
     D*==> Parametros recebidos
     D WRamo           S              2S 0
     D WApolice        S              7S 0
     D WDatInicio      S              8S 0
     D WpLinhasTxt     S               *
