     H*====================================================================
     H* Programa...: PSINP102
     H* Objectivo..: Classificar os Intervinientes do Processo de Sinistro
     H*              para o efeito tentar localizar dentro dos registos de
     H*              Intervinientes alguns já classificados, e se não
     H*              encontrar então procurar no ficheiro de Segurados e
     H*              Empresas.
     H*====================================================================
     H  ALWNULL(*USRCTL) DFTACTGRP(*NO)
     H*====================================================================
     D WNome01         S             50A
     D WNome02         S             50A
     D WIdx            S              5S 0
     D
     D*==> Função para Comparar Nomes
     D ValName         PR              N
     D    WNome01                    50A   CONST
     D    WNome02                    50A   CONST
     D    WRigor                      3S 2 CONST
     D
     D*==> Validar com que erro os nomes são iguais
     D ValPrecisao     PR             3S 2
     D    WNome01                    50A   CONST
     D    WNome02                    50A   CONST
     D    WRigor                      3S 2 CONST
     D
     D NamTC           DS                  QUALIFIED DIM(50)
     D    WNome01                    50A   INZ(*BLANKS)
     D    WNome02                    50A   INZ(*BLANKS)
     D*====================================================================
      /FREE

       EXSR FillListNames;
       DOW (WIdx > *ZEROS);
          WNome01 = NamTC(WIdx).WNome01;
          WNome02 = NamTC(WIdx).WNome02;
          IF ValName(WNome01:WNome02:0.5);
             WNome01 = *BLANKS;
          ENDIF;
          WIdx -= 1;
       ENDDO;

       *INLR = *OFF;
       RETURN;

       //==========================================================
       // Subrotina.: FillListNames
       // Objectivo.: Encher lista com os nomes
       //==========================================================
       BEGSR FillListNames;
          WIdx = 1;
          NamTC(WIdx).WNome01 = 'MECANOTUBO-CONSTRUÇÃO E ESTRUTURAS,S    ';
          NamTC(WIdx).Wnome02 = 'SALITUR                                 ';

          WIdx += 1;
          NamTC(WIdx).WNome01 = 'CONSTRUÇOES ROCHAFIEL,LDA               ';
          NamTC(WIdx).Wnome02 = 'Demo LUIS PINTO DA ROCHA                ';

          WIdx += 1;
          NamTC(WIdx).WNome01 = 'GELPEIXE-ALIMENTOS CONGELADOS,SA        ';
          NamTC(WIdx).Wnome02 = 'A IDENTIFICAR                           ';

          WIdx += 1;
          NamTC(WIdx).WNome01 = 'GRUPO SOLIDARIEDADE MUSICAL/DESP TALAÍDE';
          NamTC(WIdx).Wnome02 = 'GRUPO SOLIDARIEDADE MUSICA E DESP.TALAID';

          WIdx += 1;
          NamTC(WIdx).WNome01 = 'ANA PAULA COSTA CONCEIÇÃO               ';
          NamTC(WIdx).Wnome02 = 'ANA PAULA TEIXEIRA LOURENÇO             ';

          WIdx += 1;
          NamTC(WIdx).WNome01 = 'GELPEIXE-ALIMENTOS CONGELADOS,SA        ';
          NamTC(WIdx).Wnome02 = 'PETROGAL                                ';
       ENDSR;
      /END-FREE
      /COPY SOAPSERV/QRPGLESRC,RSINP100
