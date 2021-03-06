     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PBASO004
     H* Descri��o.: Carregar as Ap�lices filhas da Frota REN
     H*
     H*===========================================================
     FFTMPP001  IF   E           K DISK    PREFIX(INT_)
     FGREGAPOL  UF A E           K DISK    PREFIX(GRE_)
     FGREGAPO2  UF A E           K DISK    PREFIX(GRE_)
     FFPRDT001  UF A E           K DISK    PREFIX(PRD_)
     FGBASEDIA  UF   E           K DISK    PREFIX(OUT_)
     F                                     EXTFILE('QS36F/GBASEDIA')
     F*===========================================================
     D
     D*---------------------------------------------
     D* Invocar um comando de AS/400 externo:
     D*---------------------------------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Declara��o das Vari�veis
     D WNumSeq         S              5S 0
     D WCodSegurado    S              5S 0
     D WNomSegurado    S             40A
     D WArgumento      S             11A
     D WMatricula      S             15A
     D WNumApolice     S              9S 0
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WLibProd        S             10A   INZ('LGALHOZ')
     C*===================================================================
      /FREE

         EXSR FillDiario; // Colocar Ap�lices Auto a Alterar
         // EXSR ChgTipoAlt; // Implementar correc��es

         *INLR = *ON;
         RETURN;

         //==============================================================
         // Subrotina..: FillDiario
         // Objectivo..: Adicionar registo ao Di�rio de Autom�vel
         //==============================================================
         BEGSR FillDiario;
            WLibProd = 'QS36F';
            SETLL *LOVAL FTMPP001;
            READ FTMPP001;
            DOW (NOT %EOF);
               CHAIN(N) (INT_RRMAE:INT_INTAPMAE) GBASEDIA;
               IF (NOT %FOUND());
                  MONITOR;
                     WStdCmd = 'CPYF FROMFILE(QS36F/GBASEGRA) TOFILE('+
                                %TRIM(WLibProd)+'/GBASEDIA) MBROPT(*ADD) +
                                INCREL((*IF RAMO *EQ 50) (*AND APOL *EQ ' +
                                %CHAR(INT_APOLICE) + '))';
                     WStdLen = %LEN(WStdCmd);
                     shell(WStdCmd:WStdLen);
                  ON-ERROR;
                  ENDMON;
               ENDIF;

               MONITOR;
                  CHAIN (INT_RAMO:INT_APOLICE) GBASEDIA;
                  IF (%FOUND());
                     OUT_SIT    = 2;    // Situa��o
                     OUT_TALT   = 2;    // Tipo de Altera��o
                     OUT_DTALT  = 10715;// Data de Altera��o
                     OUT_PR1    = INT_PR1; // Pr�mio de RC
                     OUT_ANOCON = INT_ANOCON; // Ano Constru��o
                     OUT_PR2    = INT_PR2; // Pr�mio de DP
                     OUT_PRT    = INT_PRT; // Pr�mio Total
                     OUT_FRANQ  = INT_FRANQ; // Franquia
                     OUT_MARC1  = INT_MARC1;   // Marca
                     OUT_MATR1  = INT_MATR1;   // Matricula
                     OUT_AC1    = INT_AC1;   // Ano Constru��o
                     OUT_CL     = INT_CL;    // Cilindrada
                     OUT_PB     = INT_PB;    // Peso Bruto
                     OUT_NLUG1  = INT_NLUG1;    // N�mero de Lugares
                     OUT_CATV   = INT_CATV;    // Categoria da Viatura
                     OUT_DTMATR = INT_DTMATR; // Data da Matricula
                     OUT_DSCESP = INT_DSCESP;   // Percentagem de desconto especial
                     OUT_TCALVG = INT_TCALVG;    // Tipo de calculo de viagem
                     OUT_CAPDPI = INT_CAPFPI; // Capital inicial
                     OUT_MODELO = INT_MODELO;   // Modelo do Veiculo 1
                     OUT_CILIND = INT_CILIND;    // Cilindrada
                     OUT_CAPDP  = INT_CAPDP;  // Capital de danos pr�prios
                     OUT_VLNOVO = INT_VLNOVO;  // Valor em Novo
                     OUT_RRMAE  = INT_RRMAE;   // Ramo da m�e
                     OUT_APMAE  = INT_APMAE;   // Ap�lice da M�e
                     OUT_CRHIP  = INT_CRHIP;   // Credor Hipotec�rio

                     UPDATE BASEDIFA %FIELDS(OUT_DSCESP:OUT_TB:OUT_VIAG);
                  ENDIF;
               ON-ERROR;
               ENDMON;
               READ FTMPP001;
            ENDDO;
         ENDSR;

         //==============================================================
         // Subrotina..: ChgTipoAlt
         // Objectivo..: Alterar o Tipo de Altera��o para 2, para n�o
         //              sair a Acta nos Continuados.
         //==============================================================
         BEGSR ChgTipoAlt;
            SETLL *LOVAL FTMPP001;
            READ FTMPP001;
            DOW (NOT %EOF);
               MONITOR;
                  CHAIN (INT_RAMO:INT_APOLICE) GBASEDIA;
                  IF (%FOUND());
                     OUT_TALT = 2;
                     UPDATE BASEDIFA %FIELDS(OUT_TALT);
                  ENDIF;
               ON-ERROR;
               ENDMON;
               READ FTMPP001;
            ENDDO;
         ENDSR;

         //==============================================================
         // Subrotina..: UpdGregapol
         // Objectivo..: Criar um registo no ficheiro Gregapol
         //==============================================================
         BEGSR UpdGregapol;
            //--------------------------------------------
            // Actualizar registo principal
            //--------------------------------------------
            GRE_ACTIV  = *BLANKS;
            GRE_SIT    = 1;
            GRE_RR     = 50;      // Ap�lice
            GRE_APOL   = WNumApolice;
            GRE_TIPO   = 1;
            GRE_DTINI  = 150701;
            GRE_DTFIM  = *ZEROS;
            GRE_NSEG   = WCodSegurados;
            GRE_SEG    = WNomSegurado;
            GRE_MATR1  = WMatricula;
            GRE_DTREG  = 150617;
            GRE_DTANU  = *ZEROS;
            GRE_DTPRO  = *ZEROS;
            GRE_FILLER = *BLANKS;
            WRITE GREGAPOL;

            WNumSeq    = 1;
            WArgumento = *BLANKS;
            EXSR UpdGregapo2;
            WNumSeq   += 1;
            WArgumento = %EDITW(WCodSegurado:'0     ');
            EXSR UpdGregapo2;
            WNumSeq   += 1;
            WArgumento = '1';
            EXSR UpdGregapo2;
            WNumSeq   += 1;
            WArgumento = '0617';
            EXSR UpdGregapo2;
            WNumSeq   += 1;
            WArgumento = WMatricula;
            EXSR UpdGregapo2;
         ENDSR;

         //==============================================================
         // Subrotina..: UpdGregapol
         // Objectivo..: Actualizar o destalhe do registo
         //==============================================================
         BEGSR UpdGregapo2;
            GRE_ID    = WNumSeq;
            GRE_ANO   = 15;
            GRE_ARGUM = WArgumento;
            GRE_RR    = 50;
            GRE_APOL  = *ZEROS; // Ap�lice
            WRITE GREGAPO2;
         ENDSR;
      /END-FREE
