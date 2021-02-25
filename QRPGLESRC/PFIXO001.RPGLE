     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PFIXO001
     H* Descrição.: Sincronizar alterações.
     H* Observação: Actualmente existem dois programas de sincronização
     H*             da informação entre a Companhia e o Corretor:
     H*    [x] (LIB.SAT/SDCS10) -> É executado na facturação do Carregamentos
     H*    [x] (LIB.SD/SDC67G)  -> É executado na facturação dos Continuados
     H*===========================================================
     FGBASEGRA  UF A E           K DISK    PREFIX(AU_)
     FGBASEGRO  UF A E           K DISK    PREFIX(OR_)
     FSD1AP     UF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP1')
     FSD1APM    UF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP2')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010

     D R#SDAP        E DS                  EXTNAME('SD.1.AP') QUALIFIED
     D R#SDKEY         DS                  QUALIFIED
     D   ApolKey               1     16
     D     NumApolice          1     10
     D       Alfa              1      3
     D       Numero            4     10  0
     D     Companhia          11     13  0
     D     CodRamo            14     16  0
     D
     D*==> Estruturas de dados e Templates
     D CVT#Ramo        DS                  QUALIFIED DIM(21)
     D   Comp                         3S 0
     D   Corr                         3S 0
     D
     D*==> Campo de Trabalho
     D WIdx            S              2S 0
     D WCodChg         S              5S 0
     D WCodRamo        S              3S 0
     D WNumSegur       S              8S 0
     D WCapTotal       S             13S 2
     D WRenova         S              6S 0
     D WMatricula      S             15A
     D WMarca          S             15A
     D
     D
     D
     D
     D
     D
     D*==> Constantes
     D RAM#Comp        C                   '00100200300400500600700800901001501+
     D                                      7018019050051055065068069070'
     D RAM#Corr        C                   '08001413013334216009209309401601234+
     D                                      6096345310010343095071073072'
     D*===========================================================
      /FREE
        //------------------------------------------------------
        // Inicializar campos de Trabalho
        //------------------------------------------------------
        EXSR InzData;

        //------------------------------------------------------
        // Sincronizar alterações do Ramo Automóvel
        //------------------------------------------------------
        SETLL *LOVAL GBASEGRA;
        READ GBASEGRA;
        DOW NOT %EOF(GBASEGRA);
           EXSR CvtCodRamo;
           R#SDKEY.Alfa      = *BLANKS;
           R#SDKEY.Numero    = AU_APOL;
           R#SDKEY.Companhia = 54;
           R#SDKEY.CodRamo   = WCodRamo;
           CHAIN R#SDKEY SD1AP R#SDAP;
           IF (%FOUND(SD1AP));
              WCodChg = *ZEROS;
           ENDIF;
           READ GBASEGRA;
        ENDDO;

        *INLR = *ON;

       //============================================================
       // Subrotina.: ConvBasico
       // Descrição.: Converter os dados a um primeiro nível
       //============================================================
       BEGSR InzData;
          FOR WIdx = 1 by 1 to 21;
             CVT#Ramo(WIdx).Comp = %DEC(%SUBST(RAM#Comp:(WIdx-1)*3+1:3):3:0);
             CVT#Ramo(WIdx).Corr = %DEC(%SUBST(RAM#Corr:(WIdx-1)*3+1:3):3:0);
          ENDFOR;
       ENDSR;

       //============================================================
       // Subrotina.: CvtCodRamo
       // Descrição.: Converter o Código Ramo
       //============================================================
       BEGSR CvtCodRamo;
          WCodRamo = *ZEROS;
          FOR WIdx = 1 by 1 to 21;
             IF (CVT#Ramo(WIdx).Comp = AU_RAMO);
                WCodRamo = CVT#Ramo(WIdx).Corr;
             ENDIF;
          ENDFOR;
       ENDSR;

       //============================================================
       // Subrotina.: ConvBasico
       // Descrição.: Converter os dados a um primeiro nível
       //------------------------------------------------------------
       // INFORMAÇÃO A CONVERTER A PARTIR DA LUSITÂNIA
       //  [B] = Código Fraccionamento
       //  [C] = Mediadores
       //  [D] = Descrição Variável
       //  [E] = Capital total
       //  [F] = Renovação DD/MM
       //  [G] = Comissão
       //  [H] = Ano Veiculo
       //============================================================
       BEGSR ConvBasico;
          //--------------------------------------------------
          // [A] = Código de Segurado
          //--------------------------------------------------
          IF (WCodSegur <> R#SDAP.CSEG);
             WIsToChg = *ON;
             R#SDAP.CSEG = WCodSegur;
          ENDIF;

          //--------------------------------------------------
          // [I] = Matricula
          //--------------------------------------------------
          IF (%TRIM(WMatricula) <> %TRIM(R#SDAP.MATRI));
             WIsToChg = *ON;
          ENDIF;







       ENDSR;

       //============================================================
       // Subrotina.: ActCapitais
       // Descrição.: Actualizar Capitais da Apólice
       //------------------------------------------------------------
       // INFORMAÇÃO A CONVERTER A PARTIR DA LUSITÂNIA
       //  [A] = Código de Segurado
       //  [B] = Código Fraccionamento
       //  [C] = Mediadores
       //  [D] = Descrição Variável
       //  [E] = Capital total
       //  [F] = Renovação DD/MM
       //  [G] = Comissão
       //  [H] = Ano Veiculo
       //  [I] = Matricula
       //============================================================
       BEGSR ActCapitais;
          IF (WMes1 = R#SDAP.MMCON);










          R#SDAP.DTULT = *ZEROS; // S(6,0) Data Ult.altera. S(6,0)
          R#SDAP.RI3   = *ZEROS; // P(3,0) Cód.Corresp.3
          R#SDAP.RJ3   = *ZEROS; // P(5,5) %Corresp.3
          R#SDAP.COMTO = *ZEROS; // P(5,3) Comissão Total
          R#SDAP.CDCOM = *ZEROS; // A(1)   Comissão de Excepção
          R#SDAP.NUMUL += 1;     // S(3,0) Nº.Ultima alteração
          R#SDAP.HORUL = *ZEROS; // S(6,0) Hora da última alteração
          R#SDAP.UTIUL = *ZEROS; // A(8)   Último utilizador
       ENDSR;

       //============================================================
       // Subrotina.:
       // Descrição.:
       //============================================================
       //============================================================
       // Subrotina.:
       // Descrição.:
       //============================================================
       //============================================================
       // Subrotina.:
       // Descrição.:
       //============================================================
       //============================================================
       // Subrotina.:
       // Descrição.:
       //============================================================
       //============================================================
       // Subrotina.:
       // Descrição.:
       //============================================================



      /END-FREE
