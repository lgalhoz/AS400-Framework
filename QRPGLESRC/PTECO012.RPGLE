     H*====================================================================
     H* Programa...: PTECO012
     H* Descrição..: Actualzar inventário de ficheiros Ecrã e Campos
     H*
     H* Utilização.: Passar como parâmetro a biblitoteca, ficheiro de
     H*              source e nome do ficheiro de source.
     H*              Este programa altera o source incluíndo campo P-Field
     H*              Passar como opcional a acção que poderá ser:
     H*              IN -> Só inventariar Campos e formatos de Registos
     H*              RM -> Remover referência de todos os Campos P-Field,
     H*                    Campos usados pela opção Pop-Up.
     H*              AP -> Adicionar campos para usar Pop-UP
     H*              AF -> Adicionar campos P-Field no Source
     H*              AA -> Realizar todas as operações anteriores
     H*
     H* Observação:
     H*    Por defeito e se não colocar nenhum valor, então assume o valor
     H*    IN, realizando o inventário dos campos sem alterações ao Source.
     H*
     H*    A informação gerada por este programa, serve as funções usadas
     H*    para posicionar o cursor em cima dos campos com erro e alterar
     H*    o seu atributo, por exemplo vermelho (assinalando o erro).
     H*====================================================================
      /FREE
         RETURN;
      /END-FREE
