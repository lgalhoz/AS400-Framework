     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*=============================================================
     FGSINPRO   IF   E           K DISK    EXTFILE('G.SINPRO')
     F                                     PREFIX(PRO_)
     FGSININT   IF   E           K DISK    EXTFILE('G.SININT')
     F                                     PREFIX(INT_)
     FFSINP102  UF A E           K DISK    PREFIX(D_)
     FFSINP103  UF A E           K DISK    PREFIX(D_)
     FFSINP104  UF A E           K DISK    PREFIX(D_)
     F*=============================================================
      /Copy QRPGLESRC,SGRLO010


     D WNuPrc          DS
     D   NUPROC                1      7A
     D   AAPROC                1      2S 0
     D   DUMMY                 3      3S 0
     D   NSEQ                  4      7S 0
     D
     D*--> Adicionar/Alterar Danos Corporais
     D AddCorporais    PR
     D   NuProcesso                   7A   CONST
     D   CodInterv                    9S 0 CONST
     D   TipInterv                    1A   CONST
     D   Nome                        40A   CONST
     D   Matricula                   20A   CONST
     D   DsTipInterv                 15A   CONST
     D   NuOrdem                      2S 0 CONST
     D
     D*--> Adicionar/Alterar Danos Materiais
     D AddMateriais    PR
     D   NuProcesso                   7A   CONST
     D   CodInterv                    9S 0 CONST
     D   TipInterv                    1A   CONST
     D   Nome                        40A   CONST
     D   Matricula                   20A   CONST
     D   DsTipInterv                 15A   CONST
     D   NuOrdem                      2S 0 CONST
     D
     D*--> Validar existência do Interveniente
     D ValInterven     PR             2S 0
     D   NuProcesso                   7A   CONST
     D   TipFile                      1A   CONST
     D   Nome                        40A   CONST
     D
     D WNuProcesso     S              7A
     D WCodInterv      S              9S 0
     D WTipInterv      S              1A
     D WTipDanos       S              1A
     D WNome           S             40A
     D WMatricula      S             20A
     D WModo           S              1S 0
     D WDsTipInterv    S             20A
     D WDtFim          S              6S 0
     D
     D KTE_Tudo        C                   'T'
     D KTE_Materiais   C                   'M'
     D KTE_Corporais   C                   'C'
     D*===========================================================
      /FREE
       //-------------------------------------------------------
       // Estabelecer a Correspondência
       //-------------------------------------------------------
         SETLL *LOVAL GSINPRO;
         READ GSINPRO;
         DOW (NOT %EOF(GSINPRO));
            MONITOR;
               WDtFim = PRO_DTFIM;
            ON-ERROR;
               WDtFim = *ZEROS;
            ENDMON;
            IF (PRO_IPROC='P') AND (PRO_ACORDO='S') AND (WDtFim=*ZEROS);
               AAPROC = PRO_AAPROC;
               DUMMY  = 9;
               NSEQ   = PRO_NSEQ;
               WNuProcesso = NuProc;
               EXSR MakeCorrespond;
            ENDIF;
            READ GSINPRO;
         ENDDO;

       RETURN;

       //===========================================================
       // SubRotina.: MakeCorrespond
       // Objectivo.: Criar Correspondência entre os dios registos
       //===========================================================
       BEGSR MakeCorrespond;
          MONITOR;
             NUPROC = WNuProcesso;
             SETLL (AAPROC:NSEQ) GSININTF;
             READE (AAPROC:NSEQ) GSININTF;
             DOW (NOT %EOF());
                IF (INT_NOMSEG <> *BLANKS);
                   WNome = INT_NOMSEG;
                ELSE;
                   WNome = INT_NOMSIN;
                ENDIF;
                WCodInterv = INT_NREG;
                WTipInterv = INT_CODINT;
                IF (INT_CODINT = 'S');
                   WDsTipInterv = 'SEGURADO';
                ELSE;
                   WDsTipInterv = 'TERCEIRO';
                ENDIF;

                //-------------------------------------------------
                // Formatar registo de danos Físicos
                //-------------------------------------------------
                WMatricula = INT_IMATR1;
                IF (INT_TPDN = 'F');
                   AddCorporais(WNuProcesso:WCodInterv:WTipInterv:
                                WNome:WMatricula:WDsTipInterv:*ZEROS);
                ELSE;
                   AddMateriais(WNuProcesso:WCodInterv:WTipInterv:
                                WNome:WMatricula:WDsTipInterv:*ZEROS);
                ENDIF;
                READE (AAPROC:NSEQ) GSININTF;
             ENDDO;
          ON-ERROR;
          ENDMON;
       ENDSR;

      /END-FREE

     P*================================================================
     P* Processo..: AddCorporais
     P* Descrição.: Actualizar Danos Corporais
     P*================================================================
     P AddCorporais    B
     D AddCorporais    PI
     D   NuProcesso                   7A   CONST
     D   CodInterv                    9S 0 CONST
     D   TipInterv                    1A   CONST
     D   Nome                        40A   CONST
     D   Matricula                   20A   CONST
     D   DsTipInterv                 15A   CONST
     D   NuOrdem                      2S 0 CONST
     D WNuOrdem        S              2S 0
     D*================================================================
      /FREE
        WNuOrdem = ValInterven(NuProcesso:KTE_Corporais:Nome);
        IF (WNuOrdem > *ZEROS);
           D_NORDEM = '0'+%CHAR(WNuOrdem);
           CHAIN (WNuProcesso:D_NORDEM) RSINP102;
           IF (%FOUND());
              IF (D_NUMINT = *ZEROS);
                 D_NUMINT    = CodInterv;
                 D_TIPINT    = TipInterv;
                 UPDATE RSINP102 %FIELDS(D_NUMINT:D_TIPINT);
                 RETURN;
              ENDIF;
           ENDIF;
        ENDIF;

        WNuOrdem = ValInterven(NuProcesso:KTE_Tudo:Nome);
        IF (WNuOrdem > *ZEROS);
           D_NORDEM = '0'+%CHAR(WNuOrdem);
           CHAIN (WNuProcesso:D_NORDEM) RSINP104;
           IF (%FOUND());
              IF (D_NUMINT = *ZEROS);
                 D_NUMINT    = CodInterv;
                 D_TIPINT    = TipInterv;
                 UPDATE RSINP104 %FIELDS(D_NUMINT:D_TIPINT);
                 RETURN;
              ENDIF;
           ENDIF;
        ENDIF;
      /END-FREE
     P AddCorporais    E

     D*================================================================
     P* Processo..: AddMateriais
     P* Descrição.: Actualizar Danos Materiais
     D*================================================================
     P AddMateriais    B
     D AddMateriais    PI
     D   NuProcesso                   7A   CONST
     D   CodInterv                    9S 0 CONST
     D   TipInterv                    1A   CONST
     D   Nome                        40A   CONST
     D   Matricula                   20A   CONST
     D   DsTipInterv                 15A   CONST
     D   NuOrdem                      2S 0 CONST
     D WNuOrdem        S              9S 0
     D WKeyCorporais   S              2S 0
     D WCrtDM_DC       S               N
     D*================================================================
      /FREE
        WNuOrdem = ValInterven(NuProcesso:KTE_Materiais:Nome);
        IF (WNuOrdem > *ZEROS);
           D_NORDEM = '0'+%CHAR(WNuOrdem);
           CHAIN (WNuProcesso:D_NORDEM) RSINP103;
           IF (%FOUND());
              IF (D_NUMINT = *ZEROS);
                 D_NUMINT    = CodInterv;
                 D_TIPINT    = TipInterv;
                 UPDATE RSINP103 %FIELDS(D_NUMINT:D_TIPINT);
                 RETURN;
              ENDIF;
           ENDIF;
        ENDIF;
      /END-FREE
     P AddMateriais    E

     D*================================================================
     P* Processo..: ValInterven
     P* Descrição.: Validar registo do Interveniente
     D*================================================================
     P ValInterven     B
     D ValInterven     PI             2S 0
     D   NuProcesso                   7A   CONST
     D   TipFile                      1A   CONST
     D   Nome                        40A   CONST
     D WNuOrdem        S              2S 0 INZ(*ZEROS)
     D*================================================================
      /FREE
          MONITOR;
             IF (TipFile = KTE_Tudo);
                SETLL (NuProcesso) RSINP104;
                READE (NuProcesso) RSINP104;
             ELSEIF (TipFile = KTE_Materiais);
                SETLL (NuProcesso) RSINP103;
                READE (NuProcesso) RSINP103;
             ELSEIF (TipFile = KTE_Corporais);
                SETLL (NuProcesso) RSINP102;
                READE (NuProcesso) RSINP102;
             ENDIF;
             DOW (NOT %EOF());
                IF $ValString(D_INTERVEN:Nome:0.5);
                   RETURN %DEC(D_NORDEM:2:0);
                ENDIF;
                IF (TipFile = KTE_Tudo);
                   READE (NuProcesso) RSINP104;
                ELSEIF (TipFile = KTE_Materiais);
                   READE (NuProcesso) RSINP103;
                ELSEIF (TipFile = KTE_Corporais);
                   READE (NuProcesso) RSINP102;
                ENDIF;
             ENDDO;
          ON-ERROR;
          ENDMON;
          RETURN *ZEROS;
      /END-FREE
     P ValInterven     E
