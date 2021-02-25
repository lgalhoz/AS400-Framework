     H FIXNBR(*ZONED)
     H ALWNULL(*USRCTL) DFTACTGRP(*NO)
     H*=====================================================================
     H* Programa...: TRSF22
     H* Objectivo..: Carregar Interface de Lesados para integrar nos
     H*              Sistemas de Informação da Lusitania.
     H*
     H* Observações: Este programa é invocado com os seguintes parametros
     H*              CALL PGM(TRSF22) PARMS(<Data Encerra> <Data Altera>)
     H*                <Data Encerra> = Data de encerramento inicial ou
     H*                                 processos abertos se não existir
     H*                                 valores nesta data.
     H*               <Data Altera>   = Seleccionar registos a partir desta
     H*                                 data, ou processos alterados a
     H*                                 partir desta data.
     H*=====================================================================
     FFSINP105  IF   E           K DISK
     FFSINP102  UF   E           K DISK
     FFSINP103  UF   E           K DISK
     FFSINP104  UF   E           K DISK
     D*===================================================================
     D*---------------------------------------------
     D* Invocar um comando de AS/400 externo:
     D*---------------------------------------------
     D WNRPROC         S                   LIKE(NRPROC)
     D WIcCPRS         S               N
     D
      /FREE
        //-----------------------------------------------------
        // Formatar o valor de defeito para a data de inicio
        //-----------------------------------------------------
        EXSR ExtractDC;
        EXSR ExtractDM;
        EXSR ExtractDM_DC;

        *INLR = *ON;

        //==============================================================
        // Subrotina.: ExtractDC
        // Descrição.: Extraier registos de Danos Corporais.
        //==============================================================
        BEGSR ExtractDC;
           WNrProc = *ZEROS;
           SETLL (*LOVAL) RSINP102;
           READ RSINP102;
           DOW (NOT %EOF());

              //-----------------------------------------------------------
              // Calcular percentagem de Responsabilidade
              //-----------------------------------------------------------
              SIN10223 = *ZEROS; // Outro formatar valor por defeito
              IF (NRPROC >= '1390000');
                 //-----------------------------------------------------------
                 // Validar se está ao Abrigo do Decreto Lei
                 //-----------------------------------------------------------
                 IF (NRPROC <> WNrProc);
                    CHAIN (NRPROC) RSINP105;
                    IF (SIN10508 = -1);
                       WIcCPRS = *ON;
                    ELSE;
                       WIcCPRS = *OFF;
                    ENDIF;
                    WNrProc = NRPROC;
                 ENDIF;
                 %NullInd(SIN10223) = *OFF;

                 IF (WIcCPRS = *OFF);  // Não está ao abrigo do Decreto Lei 2006
                    SIN10223= 100;
                 ELSEIF (SIN10201 = 'TERCEIRO'); // Calcular por oposição ao Segurado
                    SIN10223= 100 - SIN10217;
                 ELSEIF (SIN10201 = 'SEGURADO'); // Calcular para o Segurado
                    SIN10223= SIN10217;
                 ENDIF;

                 UPDATE RSINP102 %FIELDS(SIN10223);
              ENDIF;
              READ RSINP102;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: ExtractDM
        // Descrição.: Extraier registos de Danos Materiais.
        //==============================================================
        BEGSR ExtractDM;
           WNrProc = *ZEROS;
           SETLL *LOVAL RSINP103;
           READ RSINP103;
           DOW (NOT %EOF());
              //-----------------------------------------------------------
              // Calcular percentagem de Responsabilidade
              //-----------------------------------------------------------
              SIN10332 = *ZEROS; // Outro formatar valor por defeito
              IF (NRPROC >= '1390000');
                 //-----------------------------------------------------------
                 // Validar se está ao Abrigo do Decreto Lei
                 //-----------------------------------------------------------
                 IF (NRPROC <> WNrProc);
                    CHAIN (NRPROC) RSINP105;
                    IF (SIN10508 = -1);
                       WIcCPRS = *ON;
                    ELSE;
                       WIcCPRS = *OFF;
                    ENDIF;
                    WNrProc = NRPROC;
                 ENDIF;

                 IF (WIcCPRS = *OFF);  // Não está ao abrigo do Decreto Lei 2006
                    SIN10332 = 100;
                 ELSEIF (SIN10301 = 'TERCEIRO'); // Calcular por oposição ao Segurado
                    SIN10332 = 100 - SIN10328;
                 ELSEIF (SIN10301 = 'SEGURADO'); // Calcular para o Segurado
                    SIN10332 = SIN10328;
                 ENDIF;
                 %NullInd(SIN10332) = *OFF;

                 UPDATE RSINP103 %FIELDS(SIN10332);
              ENDIF;
              READ RSINP103;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: ExtractDM_DC
        // Descrição.: Extrair registos de Danos Materiais e Corporais.
        //==============================================================
        BEGSR ExtractDM_DC;
           WNrProc = *ZEROS;
           SETLL (*LOVAL) RSINP104;
           READ RSINP104;
           DOW (NOT %EOF());
              //-----------------------------------------------------------
              // Calcular percentagem de Responsabilidade
              //-----------------------------------------------------------
              SIN10436 = *ZEROS; // Outro formatar valor por defeito
              IF (NRPROC >= '1390000');
                 //-----------------------------------------------------------
                 // Validar se está ao Abrigo do Decreto Lei
                 //-----------------------------------------------------------
                 IF (NRPROC <> WNrProc);
                    CHAIN (NRPROC) RSINP105;
                    IF (SIN10508 = -1);
                       WIcCPRS = *ON;
                    ELSE;
                       WIcCPRS = *OFF;
                    ENDIF;
                    WNrProc = NRPROC;
                 ENDIF;

                 IF (WIcCPRS = *OFF);  // Não está ao abrigo do Decreto Lei 2006
                    SIN10436 = 100;
                 ELSEIF (SIN10401 = 'TERCEIRO'); // Calcular por oposição ao Segurado
                    SIN10436 = 100 - SIN10425;
                 ELSEIF (SIN10401 = 'SEGURADO'); // Calcular para o Segurado
                    SIN10436 = SIN10425;
                 ENDIF;
                 %NullInd(SIN10436) = *OFF;

                 UPDATE RSINP104 %FIELDS(SIN10436);
              ENDIF;
              READ RSINP104;
           ENDDO;
        ENDSR;
      /END-FREE
