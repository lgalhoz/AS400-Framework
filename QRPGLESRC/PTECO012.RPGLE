     H*====================================================================
     H* Programa...: PTECO012
     H* Descri��o..: Actualzar invent�rio de ficheiros Ecr� e Campos
     H*
     H* Utiliza��o.: Passar como par�metro a biblitoteca, ficheiro de
     H*              source e nome do ficheiro de source.
     H*              Este programa altera o source inclu�ndo campo P-Field
     H*              Passar como opcional a ac��o que poder� ser:
     H*              IN -> S� inventariar Campos e formatos de Registos
     H*              RM -> Remover refer�ncia de todos os Campos P-Field,
     H*                    Campos usados pela op��o Pop-Up.
     H*              AP -> Adicionar campos para usar Pop-UP
     H*              AF -> Adicionar campos P-Field no Source
     H*              AA -> Realizar todas as opera��es anteriores
     H*
     H* Observa��o:
     H*    Por defeito e se n�o colocar nenhum valor, ent�o assume o valor
     H*    IN, realizando o invent�rio dos campos sem altera��es ao Source.
     H*
     H*    A informa��o gerada por este programa, serve as fun��es usadas
     H*    para posicionar o cursor em cima dos campos com erro e alterar
     H*    o seu atributo, por exemplo vermelho (assinalando o erro).
     H*====================================================================
      /FREE
         RETURN;
      /END-FREE
