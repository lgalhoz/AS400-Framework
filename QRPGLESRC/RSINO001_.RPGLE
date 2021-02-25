     H ALWNULL(*USRCTL) ACTGRP(*NEW) DFTACTGRP(*NO)
     H*=============================================================
     FGSININT   IF   E           K DISK    EXTFILE('LGALHOZ/G.SININT')
     F                                     PREFIX(INT_) USROPN
     FFSINP102  UF A E           K DISK    PREFIX(D_)
     F                                     EXTFILE('LGALHOZ/FSINP102')
     FFSINP103  UF A E           K DISK    PREFIX(D_)
     F                                     EXTFILE('LGALHOZ/FSINP103')
     FFSINP104  UF A E           K DISK    PREFIX(D_)
     F                                     EXTFILE('LGALHOZ/FSINP104')
     F*=============================================================
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
     D*--> Adicionar/Alterar Danos Materiais e Corporais
     D AddMisto        PR
     D   NuProcesso                   7A   CONST
     D   CodInterv                    9S 0 CONST
     D   TipInterv                    1A   CONST
     D   Nome                        40A   CONST
     D   Matricula                   20A   CONST
     D   DsTipInterv                 15A   CONST
     D   NuOrdem                      2S 0 CONST
     D
     D*--> Obter Registo correspondente ao AS400
     D GetInterven     PR             2S 0
     D   NuProc                       7A   CONST
     D   TipFile                      1A   CONST
     D   CodInterv                    9S 0 CONST
     D   TipInterv                    1A   CONST
     D
     D*--> Obter Ultimo número disponível
     D getLast         PR             2S 0
     D   NuProc                       7A   CONST
     D   TipInterv                    1A   CONST
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
     D
     D KTE_Tudo        C                   'T'
     D KTE_Materiais   C                   'M'
     D KTE_Corporais   C                   'C'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNuProcesso
     C                   PARM                    WModo
     C                   PARM                    WCodInterv
     C                   PARM                    WTipInterv
     C                   PARM                    WTipDanos
     C                   PARM                    WNome
     C                   PARM                    WMatricula
      /FREE

       //-------------------------------------------------------
       // Criar ou alterar o registo de Interveninente
       //-------------------------------------------------------
       MONITOR;
          IF (WModo = 1);
             //------------------------------------------------
             // Criação de todos os Intervenientes, só criar
             // se não existir ainda nenhum.
             //------------------------------------------------
             IF (GetLast(WNuProcesso:'T') = 2);
               EXSR AddAllInterven;
             ENDIF;
          ELSE;
             //------------------------------------------------
             // Criação de um interveiente, só cria ou altera
             // se não estiver na presença de registos antigos
             //------------------------------------------------
             IF (GetLast(WNuProcesso:'T') > *ZEROS);
                EXSR AddInterven;
             ENDIF;
          ENDIF;
       ON-ERROR;
       ENDMON;

       *INLR = *ON;

       //===========================================================
       // SubRotina.: AddAllInterven
       // Objectivo.: Adicionar Todos os Intervenientes
       //===========================================================
       BEGSR AddAllInterven;
          MONITOR;
             NUPROC = WNuProcesso;
             OPEN GSININT;
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
                // ForCompr registo de danos Físicos
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
             CLOSE GSININT;
          ON-ERROR;
          ENDMON;
       ENDSR;

       //==============================================================
       // SubRotina.: AddInterven
       // Objectivo.: Validar Intervenientes para Adicionar/Actualizar
       // Observação: Criar por exemplo registos em DM_DC.
       //==============================================================
       BEGSR AddInterven;
          IF (WTipInterv = 'S');
             WDsTipInterv = 'SEGURADO';
          ELSE;
             WDsTipInterv = 'TERCEIRO';
          ENDIF;

          IF (WTipDanos = 'F');
             AddCorporais(WNuProcesso:WCodInterv:WTipInterv:
                          WNome:WMatricula:WDsTipInterv:*ZEROS);
          ELSE;
             AddMateriais(WNuProcesso:WCodInterv:WTipInterv:
                          WNome:WMatricula:WDsTipInterv:*ZEROS);
          ENDIF;
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
     D WKeyDC          S              9S 0
     D WKeyDM          S              9S 0
     D WKeyDMDC        S              9S 0
     D WIO#DMDC        S              1S 0
     D KTE#Add         C                   1
     D KTE#Upd         C                   2
     D*================================================================
      /FREE
       //------------------------------------------------------------
       // Validar necessidade de inserir/alterar o registo de DM_DC
       //------------------------------------------------------------
       WIO#DMDC = *ZEROS;
       WKeyDMDC = *ZEROS;
       WKeyDC   = GetInterven(NuProcesso:KTE_Corporais:CodInterv:TipInterv);
       IF (WKeyDC < *ZEROS);
         *INLR = *ON;
         RETURN;
       ENDIF;

       D_NORDEM = '0'+%CHAR(WKeyDC);
       CHAIN (NuProcesso:D_NORDEM) RSINP104;
       IF (%FOUND());
          WKeyDMDC = WKeyDC;
          WIO#DMDC = KTE#Upd;
       ELSE;
          WKeyDM = ValInterven(NuProcesso:KTE_Materiais:Nome);
          IF (WKeyDM > *ZEROS);
             WIO#DMDC = KTE#Add;
             WKEYDMDC = *ZEROS;
          ENDIF;
       ENDIF;

       //------------------------------------------------------------
       // Criar ou alterar o registo de DC
       //------------------------------------------------------------
        IF (WKeyDC > *ZEROS);
           D_NORDEM = '0'+%CHAR(WkeyDC);
           CHAIN (WNuProcesso:D_NORDEM) RSINP102;
           D_INTERVEN = WNome;
           UPDATE RSINP102 %FIELDS(D_INTERVEN);
           IF (WIO#DMDC = KTE#Upd);
              CHAIN (WNuProcesso:D_NORDEM) RSINP104;
              D_INTERVEN = WNome;
              UPDATE RSINP104 %FIELDS(D_INTERVEN);
           ENDIF;
        ELSEIF (WKeyDC = *ZEROS);
           WKeyDC     = GetLast(WNuProcesso:TipInterv);
           D_NORDEM   = '0'+%CHAR(WKeyDC);
           D_NRPROC   = NuProcesso;
           D_SIN10201 = DsTipInterv;
           D_INTERVEN = Nome;
           D_NUMINT   = CodInterv;
           D_TIPINT   = TipInterv;

           //----------------------------------------
           // ForCompr valores por defeito
           //----------------------------------------
           D_SIN10212 = '00'; // Informações P7
           D_SIN10213 = '00'; // Informações P8
           D_SIN10214 = '00'; // Informações P9
           D_SIN10215 = '00'; // Informações P10
           D_SIN10216 = '00'; // Informações P11

           //----------------------------------------
           // forCompr as datas com valores nulos
           //----------------------------------------
           %NullInd(D_SIN10202) = *ON; // Data Pedido Indemnizatório
           %NullInd(D_SIN10203) = *ON; // Data Informação Necessidade   Exames
           %NullInd(D_SIN10204) = *ON; // Recepção dos Exames
           %NullInd(D_SIN10205) = *ON; // Disponibilização dos Exames
           %NullInd(D_DATAALTA) = *ON; // Emissão do Relatório dealta   da Clinica
           %NullInd(D_DATAQUANT)= *ON; // Data de Quantificação
           %NullInd(D_SIN10206) = *ON; // Comunicação da assunção de   Responsabilidade
           %NullInd(D_SIN10208) = *ON; // Assunção ou não de Responsa  bilidade
           %NullInd(D_SIN10209) = *ON; // Data das Informações adicio  nais
           %NullInd(D_SIN10210) = *ON; // Comunicação final (Campo de  scontinuado)
           %NullInd(D_SIN10211) = *ON; // Data do ultimo pagamento
           %NullInd(D_SIN10218) = *ON; // Envio Recibo Quitação
           %NullInd(D_SIN10219) = *ON; // Recepção Recibo Quitação ou   Factura da Ofi
           %NullInd(D_PRENCERISP)= *ON;// Pre Encerramento Isp
           %NullInd(D_SIN10221) = *ON; // Encerramento Interviniente
           D_SIN10222 = %DATE;  // (DATALTERA)
           WRITE RSINP102;
        ENDIF;

        //----------------------------------------------------
        // Adicionar registo de DM_DC
        //----------------------------------------------------
        IF (WIO#DMDC = KTE#Add);
           D_NORDEM   = '0'+%CHAR(WKeyDM);
           CHAIN (NuProcesso:D_NORDEM) RSINP103;
           IF (%FOUND());
              AddMisto(NuProcesso:D_NumInt:D_TipInt:D_INTERVEN:
                       D_Matricula:D_SIN10301:WKeyDC);
              D_INTERVEN = '(#DLT#)'+%TRIM(D_INTERVEN);
              UPDATE RSINP103 %FIELDS(D_INTERVEN);
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
     D WKeyDC          S              9S 0
     D WKeyDM          S              9S 0
     D WKeyDMDC        S              9S 0
     D WIO#DMDC        S              1S 0
     D KTE#Add         C                   1
     D KTE#Upd         C                   2
     D*================================================================
      /FREE
       //------------------------------------------------------------
       // Validar necessidade de inserir/alterar o registo de DM_DC
       //------------------------------------------------------------
       WIO#DMDC = *ZEROS;
       WKeyDMDC = *ZEROS;
       WKeyDM   = GetInterven(NuProcesso:KTE_Materiais:CodInterv:TipInterv);
       IF (WKeyDM < *ZEROS);
         *INLR = *ON;
         RETURN;
       ENDIF;

       WKeyDMDC = GetInterven(NuProcesso:KTE_Tudo:CodInterv:TipInterv);
       IF (WKeyDMDC = *ZEROS);
          WKeyDC = ValInterven(NuProcesso:KTE_Corporais:Nome);
          IF (WKeyDC > *ZEROS);
             WIO#DMDC = KTE#Add;
             WKEYDMDC = *ZEROS;
          ENDIF;
       ELSE;
          WIO#DMDC = KTE#Upd;
       ENDIF;

        //----------------------------------------------------
        // Adicionar registo de DM_DC
        //----------------------------------------------------
        IF (WIO#DMDC = KTE#Add);
           AddMisto(NuProcesso:CodInterv:TipInterv:Nome:
                    Matricula:DsTipInterv:WKeyDC);
           D_NORDEM   = '0'+%CHAR(WKeyDM);
           CHAIN (NuProcesso:D_NORDEM) RSINP103;
           IF (%FOUND());
              D_INTERVEN = '(#DLT#)'+%TRIM(D_INTERVEN);
              UPDATE RSINP103 %FIELDS(D_INTERVEN);
           ENDIF;
        ELSEIF (WIO#DMDC = KTE#Upd);
           AddMisto(NuProcesso:CodInterv:TipInterv:Nome:
                    Matricula:DsTipInterv:WKeyDC);
        ELSEIF (WKeyDM > *ZEROS);
           D_NORDEM = '0'+%CHAR(WKeyDM);
           CHAIN (NuProcesso:D_NOrdem) RSINP103;
           D_INTERVEN = Nome;
           D_Matricula = Matricula;
           UPDATE RSINP103 %FIELDS(D_MATRICULA:D_INTERVEN);
        ELSEIF (WKeyDM = *ZEROS);
           //-------------------------------------------------
           // Gerar o Registo
           //-------------------------------------------------
           WKeyDM      = GetLast(WNuProcesso:TipInterv);
           D_NORDEM    = '0'+%CHAR(WKeyDM);
           D_NRPROC    = NuProcesso;
           D_SIN10301  = DsTipInterv;
           D_MATRICULA = Matricula;
           D_INTERVEN  = Nome;
           D_NUMINT    = CodInterv;
           D_TIPINT    = TipInterv;

           //----------------------------------------
           // ForCompr valores por defeito
           //----------------------------------------
           D_SIN10305 = 'N';   // Direcção Efectiva da Reparação (S/N  )
           D_SIN10307 = 'N';   // Necessidade de desmontagem do Veicu  lo (S/N)
           D_SIN10322 = '00';  // Informações P1
           D_SIN10323 = '00';  // Informações P2
           D_SIN10324 = '00';  // Informações P3
           D_SIN10325 = '00';  // Informações P4
           D_SIN10326 = '00';  // Informações P5
           D_SIN10327 = '00';  // Informações P6

           //----------------------------------------
           // forCompr as datas com valores nulos
           //----------------------------------------
           %NullInd(D_SIN10303) = *ON;  // Data 1º.Contacto
           %NullInd(D_SIN10306) = *ON;  // Data disponibilidade da of  icina e Segurado
           %NullInd(D_SIN10308) = *ON;  // Pedido de Autorização de d  esmontagem
           %NullInd(D_SIN10309) = *ON;  // Autorização de desmontagem
           %NullInd(D_SIN10310) = *ON;  // Conclusão da Peritagem
           %NullInd(D_SIN10311) = *ON;  // Disponibilização do Relató  rio de Peritagem
           %NullInd(D_SIN10312) = *ON;  // Assunção ou não de Respons  abilidade
           %NullInd(D_SIN10313) = *ON;  // Apresentação Informações a  dicionais pelo I
           %NullInd(D_SIN10314) = *ON;  // Decisaão final P/Segurador
           %NullInd(D_SIN10317) = *ON;  // Envio Recibo Quitação
           %NullInd(D_SIN10318) = *ON;  // Recepção Recibo Quitação o  uFactura da Ofic
           %NullInd(D_SIN10319) = *ON;  // Ultimo pagamento Indemniza  ção ao Intervini
           %NullInd(D_SIN10321) = *ON;  // Recepção documentos Veicul  o(se Salvado)
           %NullInd(D_PRENCERISP)= *ON; // Pre Encerramento Isp
           %NullInd(D_SIN10330) = *ON;  // Encerramento Interviniente
           D_SIN10331 = %DATE;  // (DATALTERA)
           WRITE RSINP103;
        ENDIF;
      /END-FREE
     P AddMateriais    E

     D*================================================================
     P* Processo..: AddMisto
     P* Descrição.: Actualizar Danos Materiais
     D*================================================================
     P AddMisto        B
     D AddMisto        PI
     D   NuProcesso                   7A   CONST
     D   CodInterv                    9S 0 CONST
     D   TipInterv                    1A   CONST
     D   Nome                        40A   CONST
     D   Matricula                   20A   CONST
     D   DsTipInterv                 15A   CONST
     D   NuOrdem                      2S 0 CONST
     D WKeyDMDC        S              9S 0
     D*================================================================
      /FREE
        WKeyDMDC = GetInterven(NuProcesso:KTE_Tudo:CodInterv:TipInterv);
        IF (WKeyDMDC > *ZEROS);
           D_NORDEM = '0'+%CHAR(WKeyDMDC);
           CHAIN (NuProcesso:D_NORDEM) RSINP104;
           D_Matricula = Matricula;
           D_INTERVEN = Nome;
           UPDATE RSINP104 %FIELDS(D_MATRICULA:D_INTERVEN);
           CHAIN (NuProcesso:D_NORDEM) RSINP103;
           IF (%FOUND());
              D_INTERVEN = Nome;
              D_Matricula = Matricula;
              UPDATE RSINP103 %FIELDS(D_MATRICULA:D_INTERVEN);
           ENDIF;
        ELSEIF (WKeyDMDC = *ZEROS);
           D_NORDEM = '0'+%CHAR(NuOrdem);
           D_NRPROC    = NuProcesso;
           D_SIN10401  = DsTipInterv;
           D_INTERVEN  = Nome;
           D_NUMINT    = CodInterv;
           D_TIPINT    = TipInterv;

           //----------------------------------------
           // ForCompr valores por defeito
           //----------------------------------------
           D_SIN10415 = '00'; // Informações P12
           D_SIN10416 = '00'; // Informações P13
           D_SIN10417 = '00'; // Informações P14
           D_SIN10418 = '00'; // Informações P15
           D_SIN10419 = '00'; // Informações P16
           D_SIN10420 = '00'; // Informações P17
           D_SIN10421 = '00'; // Informações P18
           D_SIN10405 = 'N';  //  Nova deilgência para autorização de   regula
           D_SIN10410 = 'N';  //  Direcção Efectiva da Reparação (S/N  )
           D_SIN10411 = 'N';  //  Necessidade de desmontagem do Veicu  lo (S/N
           D_SIN10422 = 'N';  //  Impossibilidade de Contacto (S/N)
           D_SIN10425 = *ZEROS; // Percentagem Responsabilidade
           D_POSI_SEG = *BLANKS;// Posição do segurado
           D_SIN10426 = *BLANKS;// Tipo documento de Quitação
           D_SIN10427 = *BLANKS;// Entidade Credora
           D_SIN10430 = 'N'; // Aquisição de Salvado (S/N)
           D_SIN10432 = 'N'; // Lucros cessantes (S/N)
           D_DANOSCARGA='N'; // Danos à Carga
           D_SIN10433 = *BLANKS;// Interveniente Encerrado

           //----------------------------------------
           // forCompr as datas com valores nulos
           //----------------------------------------
           %NullInd(D_SIN10402) = *ON; // Data 1º Contacto para autorização Reg.  DM
           %NullInd(D_SIN10403) = *ON; // Assunção ou não de Responsabilidade
           %NullInd(D_SIN10404) = *ON; // Data da autor.Reg. DM
           %NullInd(D_SIN10406) = *ON; // Data das Inform.adicionais
           %NullInd(D_SIN10407) = *ON; // Data da comunicação Final
           %NullInd(D_SIN10408) = *ON; // Data do último Pagamento
           %NullInd(D_SIN10409) = *ON; // Data 1º.Contacto
           %NullInd(D_SIN10412) = *ON; // Data disponibilidade da oficina e Segu  rado
           %NullInd(D_SIN10413) = *ON; // Conclusão da Peritagem
           %NullInd(D_SIN10414) = *ON; // Disponibilização do Relatório de Perit  agem
           %NullInd(D_SIN10423) = *ON; // Pedido de Autorização de desmontagem
           %NullInd(D_SIN10424) = *ON; // Autorização de desmontagem
           %NullInd(D_SIN10428) = *ON; // Envio Recibo Quitação
           %NullInd(D_SIN10429) = *ON; // Recepção Recibo Quitação ouFactura da   Ofic
           %NullInd(D_SIN10431) = *ON; // Recepção documentos Veiculo(se Salvado  )
           %NullInd(D_PRENCERISP) = *ON; // Pre Encerramento Isp
           %NullInd(D_SIN10434) = *ON; // Encerramento Interviniente
           D_SIN10435 = %DATE; // Data de Alteracao
           WRITE RSINP104;
        ENDIF;
      /END-FREE
     P AddMisto        E

     P*================================================================
     P* Processo..: GetInterven
     P* Descrição.: Obter o Código do Interveniente gerado no MS.Access
     P*================================================================
     P GetInterven     B
     D GetInterven     PI             2S 0
     D   NuProcesso                   7A   CONST
     D   TipFile                      1A   CONST
     D   CodInterv                    9S 0 CONST
     D   TipInterv                    1A   CONST
     D*================================================================
      /FREE
          MONITOR;
             IF (TipFile = KTE_Tudo);
                SETLL (NuProcesso) RSINP104;
                READE(N) (NuProcesso) RSINP104;
             ELSEIF (TipFile = KTE_Materiais);
                SETLL (NuProcesso) RSINP103;
                READE(N) (NuProcesso) RSINP103;
             ELSEIF (TipFile = KTE_Corporais);
                SETLL (NuProcesso) RSINP102;
                READE(N) (NuProcesso) RSINP102;
             ENDIF;
             DOW (NOT %EOF());
                IF (D_NUMINT=CodInterv) AND (D_TIPINT=TipInterv);
                   RETURN %DEC(D_NORDEM:2:0);
                ELSEIF (D_NUMINT=*ZEROS) OR (D_TIPINT=*BLANKS);
                   RETURN -1;
                ENDIF;
                IF (TipFile = KTE_Tudo);
                   READE(N) (NuProcesso) RSINP104;
                ELSEIF (TipFile = KTE_Materiais);
                   READE(N) (NuProcesso) RSINP103;
                ELSEIF (TipFile = KTE_Corporais);
                   READE(N) (NuProcesso) RSINP102;
                ENDIF;
             ENDDO;
          ON-ERROR;
          ENDMON;
          RETURN *ZEROS;
      /END-FREE
     P GetInterven     E

     D*================================================================
     P* Processo..: GetLast
     P* Descrição.: Obter o último código livre
     D*================================================================
     P GetLast         B
     D GetLast         PI             2S 0
     D   NuProcesso                   7A   CONST
     D   TipInterv                    1A   CONST
     D WNuOrdem        S              2S 0 INZ(*ZEROS)
     D*================================================================
      /FREE
          IF (TipInterv = 'S');
            RETURN 1;
          ELSE;
             MONITOR;
                WNuOrdem = 1;
                SETLL (NuProcesso) RSINP103;
                READE(N) (NuProcesso) RSINP103;
                DOW (NOT %EOF());
                   IF (D_NUMINT=*ZEROS) OR (D_TIPINT=*BLANKS);
                      RETURN -1;
                   ELSEIF (WNuOrdem < %DEC(D_NORDEM:2:0));
                      WNuOrdem = %DEC(D_NORDEM:2:0);
                   ENDIF;
                   READE(N) (NuProcesso) RSINP103;
                ENDDO;

                SETLL (NuProcesso) RSINP102;
                READE(N) (NuProcesso) RSINP102;
                DOW (NOT %EOF());
                   IF (D_NUMINT=*ZEROS) OR (D_TIPINT=*BLANKS);
                      RETURN -1;
                   ELSEIF (WNuOrdem < %DEC(D_NORDEM:2:0));
                      WNuOrdem = %DEC(D_NORDEM:2:0);
                   ENDIF;
                   READE(N) (NuProcesso) RSINP102;
                ENDDO;
             ON-ERROR;
             ENDMON;
          ENDIF;
          WNuOrdem += 1;
          RETURN WNuOrdem;
      /END-FREE
     P GetLast         E

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
                READE(N) (NuProcesso) RSINP104;
             ELSEIF (TipFile = KTE_Materiais);
                SETLL (NuProcesso) RSINP103;
                READE(N) (NuProcesso) RSINP103;
             ELSEIF (TipFile = KTE_Corporais);
                SETLL (NuProcesso) RSINP102;
                READE(N) (NuProcesso) RSINP102;
             ENDIF;
             DOW (NOT %EOF());
                IF (D_INTERVEN = Nome);
                   RETURN %DEC(D_NORDEM:2:0);
                ENDIF;
                IF (TipFile = KTE_Tudo);
                   READE(N) (NuProcesso) RSINP104;
                ELSEIF (TipFile = KTE_Materiais);
                   READE(N) (NuProcesso) RSINP103;
                ELSEIF (TipFile = KTE_Corporais);
                   READE(N) (NuProcesso) RSINP102;
                ENDIF;
             ENDDO;
          ON-ERROR;
          ENDMON;
          RETURN *ZEROS;
      /END-FREE
     P ValInterven     E
