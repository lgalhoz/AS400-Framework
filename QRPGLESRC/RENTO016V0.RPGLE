     H NOMAIN
     H*=======================================================
     H* OBSERVAÇÃO
     H*-------------------------------------------------------
     H* Alterar esta rotina para passar a devolver o código
     H* da mensagem de erro, e passar a receber o NIF
     H* e informação a Validar (Penhoras ou Lista Negra)
     H*-------------------------------------------------------
     H* Exemplo:
     H*     WMsgError = $CheckNIF(NUNIF);
     H*     WStatus = $IsPenhora(NUNIF);
     H*     WStatus = $IsLstNegra);
     H*=======================================================
     FFENTP01001IF   E           K DISK    USROPN PREFIX(ENT_)
     F*=======================================================
      /Copy QRPGLESRC,SENTO010

     D*==> Campos de Trabalho
     D
     P*=======================================================
     P* Processo..: $CheckNIF
     P* Descrição.: Validar se um NIF consta da base de dados
     C*=======================================================
     P $CheckNIF       B                   EXPORT
     D                 PI             7A
     D    NumNIF                      9S 0 CONST
     D
     D*==> Decomposição do NIF
     D  DS#NIF         DS                  QUALIFIED
     D   NIF                   1      9S 0
     D   D1                    1      1S 0
     D   D2                    2      2S 0
     D   D3                    3      3S 0
     D   D4                    4      4S 0
     D   D5                    5      5S 0
     D   D6                    6      6S 0
     D   D7                    7      7S 0
     D   D8                    8      8S 0
     D   D9                    9      9S 0
     D
     D*==> Campos de Trabalho
     D WNumNIF         S              9S 0
     D WTotal          S              3S 0
     D WMsgErro        S              7A
     D*=======================================================
      /FREE
       WNumNIF  = NUMNIF;
       WMsgErro = *BLANKS;
       IF (WNumNIF = *ZEROS);
          WMsgErro = 'XXCSDSD0'; // O NIF não pode ser zero
       ELSEIF (WNumNIF < 100000000);
          WMsgErro = 'XXCSDSD0'; // O NIF não pode ser inferior a 100000000
       ELSE;
          EXSR ValidNIF;
          IF (WMsgErro = *BLANKS);
            OPEN FENTP01001;
            CHAIN (WNumNIF:99999999) RENTP010;
            IF (%FOUND());
               WMsgErro = 'XCVSDFSF'; // Penhora Activa
            ENDIF;
            CLOSE FENTP01001;
          ENDIF;
       ENDIF;

       //-----------------------------------------------
       // Devolver Resultado da Operação
       //-----------------------------------------------
       IF (WMsgErro <> *BLANKS);
          RETURN *OFF;  // Ocorreram erros (substituir)
       ELSE;
          RETURN *ON;   // Não consta da base de dados
       ENDIF;

       //=============================================================
       // Subrotina..: ValidNIF
       // Objectivo..: Validar se o NIF é válido
       //=============================================================
       BEGSR ValidNIF;
         MONITOR;
            DS#NIF.NIF = WNumNIF;
            WTotal = DS#NIF.D9*1 + DS#NIF.D8*2 + DS#NIF.D7*3 + DS#NIF.D6*4 +
                     DS#NIF.D5*5 + DS#NIF.D4*6 + DS#NIF.D3*7 + DS#NIF.D2*8 +
                     DS#NIF.D1*9;
            IF ((11-%REM(WTotal:11)) >= 10);
               WMsgErro = *BLANKS;
            ELSE;
               WMsgErro = 'XXXXXXX'; // Estrutura do NIF com erros
            ENDIF;
         ON-ERROR;
            WMsgErro = 'ZXZZZZZ'; // NIF Com erros
         ENDMON;
       ENDSR;
      /END-FREE
     P $CheckNIF       E
     P*=======================================================
     P* Processo..: $IsPenhora
     P* Descrição.: Validar se o NIF tem uma Penhora activa
     P*=======================================================
     P $IsPenhora      B                   EXPORT
     D                 PI              N
     D    NumNIF                      9S 0 CONST
     D
     D*=======================================================
      /FREE
         RETURN *ON;
      /END-FREE
     P $IsPenhora      E

     P*=======================================================
     P* Processo..: $IsLstNegra
     P* Descrição.: Validar se o NIF está na lista negra
     P*=======================================================
     P $IsLstNegra     B                   EXPORT
     D                 PI              N
     D    NumNIF                      9S 0 CONST
     D
     D*=======================================================
      /FREE
         RETURN *ON;
      /END-FREE
     P $IsLstNegra     E

     P*=======================================================
     P* Processo..: $CheckNIB
     P* Descrição.: Validar se o NIF está na lista negra
     P*=======================================================
     P $CheckNIB       B                   EXPORT
     D                 PI             7A
     D    NumNIB                     21S 0 CONST
     D
     D*==> Campos de Trabalho
     D DSDIV           DS
     D   WNumComp              1     21S 0
     D   WNumNIB               1     19S 0
     D   WRestNIB             20     21S 0
     D
     D WNumAlfa        S             19A
     D WSoma           S              4S 0
     D
     D*==> Elementos para validar digito de controlo da ADC
     D KteFactor       DS                   QUALIFIED DIM(19)
     D  Val                           3S 0
     D WIdx            S              2S 0
     D KteDivisor      C                   97
     D KteAcerto       C                   98
     D*=======================================================
      /FREE
       WNumComp    = NumNIB;
       EXSR FillFactores;
       WNumAlfa = %TRIM(%EDITW(WNumNIB:'0                   '));
       WSoma = *ZEROS;
       WIdx  = 1;
       DOW (WIdx < 20);
          WSoma += %DEC(%SUBST(WnumAlfa:WIdx:1):1:0) * KteFactor(WIdx).Val;
          WIdx += 1;
       ENDDO;
       IF (WRestNIB <> (KteAcerto - %REM(WSoma:KteDivisor)));
          RETURN 'COB0006';
       ELSE;
          RETURN *BLANKS;
       ENDIF;
       //=====================================================
       // Subrotina..: FillFactores
       // Objectivo..: Carregar Factores com dados
       //=====================================================
       BEGSR FillFactores;
          KteFactor(01).Val = 73;
          KteFactor(02).Val = 17;
          KteFactor(03).Val = 89;
          KteFactor(04).Val = 38;
          KteFactor(05).Val = 62;
          KteFactor(06).Val = 45;
          KteFactor(07).Val = 53;
          KteFactor(08).Val = 15;
          KteFactor(09).Val = 50;
          KteFactor(10).Val = 05;
          KteFactor(11).Val = 49;
          KteFactor(12).Val = 34;
          KteFactor(13).Val = 81;
          KteFactor(14).Val = 76;
          KteFactor(15).Val = 27;
          KteFactor(16).Val = 90;
          KteFactor(17).Val = 09;
          KteFactor(18).Val = 30;
          KteFactor(19).Val = 03;
       ENDSR;
      /END-FREE
     P $CheckNIB       E

     P*=======================================================
     P* Processo..: $CheckADC
     P* Descrição.: Validar se o NIF está na lista negra
     P*=======================================================
     P $CheckADC       B                   EXPORT
     D                 PI             7A
     D    NumADC                     11S 0 CONST
     D
     D*==> Campos de Trabalho
     D DSDIV           DS
     D   WNumComp              1     15S 0
     D   WHeader               1      6S 0
     D   WNumADC               7     15S 0
     D     WPartADC            7     13S 0
     D     WRestADC           14     15S 0
     D
     D WRestADCbak     S              2S 0
     D WNumAlfa        S             15A
     D WSoma           S              4S 0
     D
     D*==> Elementos para validar digito de controlo da ADC
     D KteFactor       DS                   QUALIFIED DIM(15)
     D  Val                           3S 0
     D WIdx            S              2S 0
     D KteFixo         C                   102706
     D KteDivisor      C                   97
     D KteAcerto       C                   98
     D*=======================================================
      /FREE
       WHeader     = KteFixo;
       WNumADC     = NumADC;
       WRestADCbak = WRestADC;
       WNumADC     = WPartADC;
       EXSR FillFactores;
       WNumAlfa = %TRIM(%EDITW(WNumComp:'0               '));
       WSoma = *ZEROS;
       WIdx  = 1;
       DOW (WIdx < 16);
          WSoma += %DEC(%SUBST(WnumAlfa:WIdx:1):1:0) * KteFactor(WIdx).Val;
          WIdx += 1;
       ENDDO;
       IF (WRestADCBak <> (KteAcerto - %REM(WSoma:KteDivisor)));
          RETURN 'COB0004';
       ELSE;
          RETURN *BLANKS;
       ENDIF;
       //=====================================================
       // Subrotina..: FillFactores
       // Objectivo..: Carregar Factores com dados
       //=====================================================
       BEGSR FillFactores;
          KteFactor(01).Val = 62;
          KteFactor(02).Val = 45;
          KteFactor(03).Val = 53;
          KteFactor(04).Val = 15;
          KteFactor(05).Val = 50;
          KteFactor(06).Val = 05;
          KteFactor(07).Val = 49;
          KteFactor(08).Val = 34;
          KteFactor(09).Val = 81;
          KteFactor(10).Val = 76;
          KteFactor(11).Val = 27;
          KteFactor(12).Val = 90;
          KteFactor(13).Val = 09;
          KteFactor(14).Val = 30;
          KteFactor(15).Val = 03;
       ENDSR;
      /END-FREE
     P $CheckADC       E
