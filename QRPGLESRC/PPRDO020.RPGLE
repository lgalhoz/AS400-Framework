     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO020
     H* Descrição.: Marcar Apólices dos Corretor com o código de Anuladas
     H* Observação: O Transporte da SD.1.AP para a SD.1.APM, é realizado
     H*             através do programa LIB.SD/SD087.
     H*==================================================================
     FGBASEGRL  IF   E           K DISK    PREFIX(BAS_)
     FSD1AP1    UF   F  320    16AIDISK    KEYLOC(8) EXTFILE(WApolices) USROPN
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D R#SD1AP       E DS                  EXTNAME('SD.1.AP') QUALIFIED
     D K#SD1AP         DS                  QUALIFIED
     D   APOL                  1     10A
     d   APNM                  4     10S 0
     D   COMP                 11     13S 0
     D   RAMO                 14     16S 0
     D
     D*==> Tradução de Ramos
     D RMCMP           S              2  0 DIM(22) CTDATA PERRCD(1)
     D RMCOR           S              3  0 DIM(22) ALT(RMCMP)
     D*==> Tradução de Ramos
     D MSCMP           S              2  0 DIM(15) CTDATA PERRCD(1)
     D MSCOR           S              2    DIM(15) ALT(MSCMP)
     D
     D*==> Campos de Trabalho
     D WApolices       S             21A
     D WLibProd        S             10A
     D WIdx            S              3S 0
     C*========================================================
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //------------------------------------
       // Indicar qual o ficheiro a alterar
       //------------------------------------
       WLibProd  = 'QTEMP';  // Modo de Real
       WApolices = %TRIM(WLibProd)+'/SD.1.AP';
       OPEN SD1AP1;

       //------------------------------------
       // Localizar Apólices Anuladas
       //------------------------------------
       SETLL *LOVAL GBASEGRL;
       READ GBASEGRL;
       DOW (NOT %EOF(GBASEGRL));
          IF (BAS_SIT = 3);
             FOR WIdx = 1 By 1 TO 21;
                IF (BAS_RAMO = RMCMP(WIdx));
                   K#SD1AP.RAMO = RMCOR(WIdx);
                   LEAVE;
                ENDIF;
             ENDFOR;
             K#SD1AP.APNM = BAS_APOL;
             K#SD1AP.COMP = 54;
             CHAIN K#SD1AP SD1AP1 R#SD1AP;
             IF (%FOUND(SD1AP1));
                IF (R#SD1AP.CDSUP = ' ') AND (R#SD1AP.CODSU = ' ');
                   FOR WIdx = 1 By 1 TO 15;
                      IF (BAS_MALT = MSCMP(WIdx));
                         R#SD1AP.CODSU = MSCOR(WIdx);
                         R#SD1AP.CDSUP = 'S';
                         R#SD1AP.DTSUP = BAS_DALT;         //  AAMMDD
                         R#SD1AP.NUMUL += 1;
                         R#SD1AP.UTIUL = 'AUTOM_CI';
                         R#SD1AP.DTULT = (S#DIA * 10000) + (S#MES * 100) +
                                         (S#ANO - 2000);
                         R#SD1AP.HORUL = S#TIME; // (Hora de hoje HHMMSS)
                         UPDATE SD1AP1 R#SD1AP;
                         LEAVE;
                      ENDIF;
                   ENDFOR;
                ENDIF;
             ENDIF;
          ENDIF;
          READ GBASEGRL;
       ENDDO;

       CLOSE SD1AP1;
       *INLR = *ON;
       RETURN;

      /END-FREE
** Conversão dos Ramos (Lusitânia vs Corretor)
01080  Incêndio e Elementos da Natureza
02014  Acidentes Pessoais
03130  Responsabilidade Civil Pessoal
04133  Responsabilidade Civil Geral
05342  MultiRiscos Habitação Residentia / Condominio
06160  Perda de Lucros
07092  Dinheiro em Trânsito
08093  Roubo Residências
09094  Roubo Estabelecimentos
10016  Viagem
15012  Acidentes de Trabalho
17346  Equipamento Electrónico
18096  Avaria de Máquinas
19345  Obras e Montagens
50310  Automóvel
51010  Ocupanetes
55343  Multiriscos estabelecimentos
65095  Cristais
67000  Bagagem  (Sem correspondência com o Corretor)
68071  Transportes Marítimo e Terrestre
69073  Transportes Postal
70072  Transportes Aéreo
** Conversão dos motivos de Anulação
6102  1 Anulação a pedido do Tomador do Seguro
8814  2     "     pela Extinção do Risco por Sinistro
8517  3     "     por caducidade pela Condições próprias
8307  4     "     por emissão inválida
8703  5     "     por excesso de Sinistralidade
8103  6     "     por falsas declarações
6201  7     "     por falta de Pagamento
8901  8     "     por falta de pagamento devolução de Cheque
8617  9     "     por fim do Período de Suspensão
8401 10     "     por iniciativa da Seguradora
6400 11     "     por outros motivos
6304 12     "     por substituição
8209 13     "     por transferência para a Lusitânia
6515 14     "     por venda do Objecto Seguro
9101 15     "     da alteração por falta de pagamento
