     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO020
     H* Descri��o.: Marcar Ap�lices dos Corretor com o c�digo de Anuladas
     H* Observa��o: O Transporte da SD.1.AP para a SD.1.APM, � realizado
     H*             atrav�s do programa LIB.SD/SD087.
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
     D*==> Tradu��o de Ramos
     D RMCMP           S              2  0 DIM(22) CTDATA PERRCD(1)
     D RMCOR           S              3  0 DIM(22) ALT(RMCMP)
     D*==> Tradu��o de Ramos
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
       // Se n�o estiver autorizado ent�o sai
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
       // Localizar Ap�lices Anuladas
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
** Convers�o dos Ramos (Lusit�nia vs Corretor)
01080  Inc�ndio e Elementos da Natureza
02014  Acidentes Pessoais
03130  Responsabilidade Civil Pessoal
04133  Responsabilidade Civil Geral
05342  MultiRiscos Habita��o Residentia / Condominio
06160  Perda de Lucros
07092  Dinheiro em Tr�nsito
08093  Roubo Resid�ncias
09094  Roubo Estabelecimentos
10016  Viagem
15012  Acidentes de Trabalho
17346  Equipamento Electr�nico
18096  Avaria de M�quinas
19345  Obras e Montagens
50310  Autom�vel
51010  Ocupanetes
55343  Multiriscos estabelecimentos
65095  Cristais
67000  Bagagem  (Sem correspond�ncia com o Corretor)
68071  Transportes Mar�timo e Terrestre
69073  Transportes Postal
70072  Transportes A�reo
** Convers�o dos motivos de Anula��o
6102  1 Anula��o a pedido do Tomador do Seguro
8814  2     "     pela Extin��o do Risco por Sinistro
8517  3     "     por caducidade pela Condi��es pr�prias
8307  4     "     por emiss�o inv�lida
8703  5     "     por excesso de Sinistralidade
8103  6     "     por falsas declara��es
6201  7     "     por falta de Pagamento
8901  8     "     por falta de pagamento devolu��o de Cheque
8617  9     "     por fim do Per�odo de Suspens�o
8401 10     "     por iniciativa da Seguradora
6400 11     "     por outros motivos
6304 12     "     por substitui��o
8209 13     "     por transfer�ncia para a Lusit�nia
6515 14     "     por venda do Objecto Seguro
9101 15     "     da altera��o por falta de pagamento
