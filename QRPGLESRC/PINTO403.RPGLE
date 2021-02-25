     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*=====================================================================
     H* Programa...: PINTO023
     H*
     H*              Sistemas de Informação da Lusitania.
     H*
     H* Observações: Este programa é invocado na cadeia de geração
     H*              automática de Interfaces.
     H*=====================================================================
     FOPPERIT   UF A F  516        DISK    USROPN
     FFINTP001  UF   E           K DISK    PREFIX(INT_)
     FFINTP004  UF A E           K DISK    PREFIX(INT_)
     FFSINP10301IF   E           K DISK
     FFSINP10401IF   E           K DISK
     FFSINP105  IF   E           K DISK
     FGBASEGRA  IF   E           K DISK    PREFIX(APO_)
     FFSINP11402IF   E           K DISK    PREFIX(REL_)
     FGSINPRO   IF   E           K DISK    EXTFILE('G.SINPRO') PREFIX(PRC_)
     FGSININT   IF   E           K DISK    EXTFILE('G.SININT') PREFIX(INT_)
     FGSINPRV   IF   E           K DISK    EXTFILE('G.SINPRV') PREFIX(PRV_)
     F*===================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*---------------------------------------------
     D* Invocar um comando de AS/400 externo:
     D*---------------------------------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WIcErro         S               N
     D WTestVar        S             20A   INZ(*BLANKS)
     D WTipInterv      S              1A
     D WCodInterv      S              1S 0
     D WDatInicio      S               D
     D WDatAltera      S              8S 0 INZ(*ZEROS)
     D WDatPeritagem   S              8S 0
     D WDatRefer       S               D
     D WAaProc         S                   LIKE(REL_AAPROC)
     D WNuProc         S                   LIKE(REL_NUPROC)
     D WMsTpInt        S                   LIKE(REL_MSTPINT)
     D WMsSqInt        S                   LIKE(REL_MSSQINT)
     D WMsTpSin        S                   LIKE(REL_MSTPSIN)
     D WIcSegProc      S               N
     D WCobertura      S              3A   INZ('000')
     D WPRV_RCA        S                   LIKE(PRV_VALR1)
     D WPRV_DP1        S                   LIKE(PRV_VALR3)
     D WPRV_DP2        S                   LIKE(PRV_VALR45)
     D
     D WCdRemessa      S                   LIKE(INT_CDREMESSA)
     D WNomAS400       S             10A
     D WNomPASE        S             40A
     D WSessionID      S              5S 0
     D WDatChar        S             20A
     D
     D ProcCodif       DS
     D   ProcAno               1      2S 0
     D   PDifer                3      3S 0
     D   ProcNum               4      7S 0
     D
     D*==> Função para converter datas
     D CvtDate         PR             8S 0
     D    WDatIN                       D   CONST OPTIONS(*NULLIND)
     D
     D*==> Para converter me Maiúsuclas ou Minusculas
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyz'
     D*===================================================================
     IOPPERIT   NS  01
     I                                  1    2 0RAMO
     I                                  3    4  MODALIDADE
     I                                  5    6  SUBMODALIDADE
     I                                  7   10 0ANO
     I                                 11   15 0NUMSINISTRO
     I                                 16   25 0INTERVENIENTE
     I                                 26   33 0DATA_PEDIDO
     I                                 34   34  ENVIADA_PERITG
     I                                 35   37  TIPO_PERITAGEM
     I                                 38   47 0OFICINA_REPAR
     I                                 48   57 0PERITO
     I                                 58  107  OBSERVACOES
     I                                108  115 0DATA_ENVIO_PER
     I                                116  125 0NUM_DIAS_REPAR
     I                                126  133 0DATA_INICIO_RP
     I                                134  134  MOTIVOS_PERITA
     I                                135  135  ORDEM_REPARACA
     I                                136  143 0DATA_ORDEM_REP
     I                                144  144  PERIODO_PERITA
     I                                145  145  TIPO_VISTORIA
     I                                146  149  TIPO_SINISTRO
     I                                150  159 0VALOR_LIM_PERD
     I                                160  160 0CONDICIONAL
     I                                161  167 0APOLICE
     I                                168  170  COBERTURA
     I                                171  182  MATRICULA2
     I                                183  189  MARCA
     I                                190  190  VEICULO_IMOBIL
     I                                191  200 0CAPITSEGDTSIN
     I                                201  210 0FRANQUIA
     I                                211  260  DESCRICAODANOS
     I                                261  292  DOSSIER
     I                                293  324  ID_PERITAGEM
     I                                325  332  LOGIN
     I                                333  338  TIPO_VEICULO
     I                                339  339  TIPOINTERVENIE
     I                                340  379  MODELO
     I                                380  387 0DATA_RECEPCAO
     I                                388  395 0DATA_RESPOSTA
     I                                396  405 0VALORREPARACAO
     I                                406  406  OFICINARECOMEN
     I                                407  407  PEDAUTORIZDESM
     I                                408  415 0DATPEDAUTDESM
     I                                416  423 0DATAUTORIZDESM
     I                                424  433 0NUMDIASALUGUER
     I                                434  434  PERDA_TOTAL
     I                                435  435  AQUISICAOSALVA
     I                                436  443 0DATRECEPDOCVEI
     I                                444  451 0DATCONPERITAG
     I                                452  459 0DATDISPRELATOR
     I                                460  460  PERITAGEMCANC
     I                                461  468 0DATNOTCONCPER
     I                                469  476 0DATA_PERITAGEM
     I                                477  516  MODELODESCRIT
     I*===========================================================
      /FREE
        //-----------------------------------------------------
        // Validar se está em execução o gerador de Interfaces
        //-----------------------------------------------------
        SETLL (*HIVAL) RINTP001;
        READP RINTP001;
        IF (INT_CDPRCEXEC <> 2) OR %EOF();
           *INLR = *ON;
           RETURN;
        ENDIF;
        WCdRemessa = INT_CDREMESSA;
        WDatInicio = INT_DTINICIO;
        WDatAltera = %SUBDT(INT_DTINICIO:*YEARS)*10000+
                     %SUBDT(INT_DTINICIO:*MONTHS)*100+
                     %SUBDT(INT_DTINICIO:*DAYS);
        WIcErro = *OFF;

        EXSR CrtOpPerit;
        IF (NOT WIcErro);
           OPEN OpPerit;
           EXSR AddPeritag;
           CLOSE OpPerit;
        ENDIF;
        EXSR SendSFTP;
        *INLR = *ON;

        //==============================================================
        // Subrotina.: addPeritag
        // Descrição.: Adicionar registos com a informação sobre a
        //             Peritagem às viaturas.
        //==============================================================
        BEGSR AddPeritag;
           SETLL *LOVAL RSINP105;
           READ RSINP105;
           DOW (NOT %EOF());
              IF (%NULLIND(SIN10506)=*ON) OR
                 (SIN10506 >= WDatInicio);
                 IF (SIN10508 = -1);
                    IF (CvtDate(SIN10512) >= WDatAltera);
                       MONITOR;
                          WIcSegProc = *OFF;
                          ProcCodif  = NRPROC;
                          WAaProc    = ProcAno;
                          WNuProc    = ProcNum;
                          WDatPeritagem = *ZEROS;
                          EXSR GetCobertura; // Obter o Código de Cobertura
                          EXSR ExtractDM;
                          EXSR ExtractDM_DC;
                          IF (NOT WIcSegProc);
                             EXSR AddSegurado;
                          ENDIF;
                       ON-ERROR;
                       ENDMON;
                    ENDIF;
                 ENDIF;
              ENDIF;
              READ RSINP105;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: AddSegurado
        // Descrição.: Adicionar os dados do Segurado.
        //==============================================================
        BEGSR AddSegurado;
           //---------------------------------------
           // Obter os dados do segurado
           //---------------------------------------
           SETLL (WAaProc:WNuproc) RSINP114;
           READ RSINP114;
           DOW (NOT %EOF());
              IF (REL_AAPROC <> WAaProc) OR (WNuProc <> REL_NUPROC);
                 LEAVE;
              ENDIF;

              IF (REL_SEQUENCIA = 1);
                 WIcSegProc     = *ON;
                 INTERVENIENTE  = (900000 + REL_NUMINT);
                 COBERTURA      = WCobertura;
                 MATRICULA2     = REL_ASNMMAT;
                 MARCA          = REL_ASMARCA;
                 EXSR decCategoria;
                 MODELODESCRIT  = '9999';
                 LEAVE;
              ENDIF;
              READ RSINP114;
           ENDDO;

           //-------------------------------------------------
           // Formatar com valores por defeito os restantes
           //-------------------------------------------------
           IF (WIcSegProc);
              CHAIN ('P':WAaProc:WNuProc) GSINPROF;
              IF (%FOUND());
                 CAPITSEGDTSIN  = PRC_CAP2; // Capital Seguro
                 FRANQUIA       = PRC_FRQ2; // Franquia
                 APOLICE        = PRC_AP;   // Apólice
              ENDIF;

              PEDAUTORIZDESM = *ZEROS;
              DATPEDAUTDESM  = *ZEROS;
              DATAUTORIZDESM = *ZEROS;
              DATCONPERITAG  = *ZEROS;
              DATDISPRELATOR = *ZEROS;
              DATA_PERITAGEM = *ZEROS; // WDatPeritagem;
              EXSR ADDRecord;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: ExtractDM
        // Descrição.: Extraier registos de Danos Materiais.
        //==============================================================
        BEGSR ExtractDM;
           SETLL (NRPROC) RSINP103;
           READE (NRPROC) RSINP103;
           DOW (NOT %EOF());
              //-----------------------------------------------
              // Formatar os campos da Tabela
              //-----------------------------------------------
              IF (%XLATE(StrLo:StrUp:SIN10301) = 'SE');
                 WTipInterv = 'S';
                 WCodInterv = %INT(NORDEM);
              ELSE;
                 WTipInterv = 'T';
                 WCodInterv = %INT(NORDEM);
              ENDIF;
              WMsTpInt = SIN10301;
              WMsTpSin = 1;   // Danos Corporais
              WMsSqInt = %DEC(NORDEM:2:0);
              EXSR GetApolice;            // Obter Elementos em falta
              PEDAUTORIZDESM = SIN10307;  // Necessidade Desmontagem
              MATRICULA2     = MATRICULA;// Matricula
              DATPEDAUTDESM  = CvtDate(SIN10308); // Pedido desmontagem
              DATAUTORIZDESM = CvtDate(SIN10309); // Autorização desmontagem
              DATCONPERITAG  = CvtDate(SIN10310);
              DATDISPRELATOR = CvtDate(SIN10311);
              DATA_PERITAGEM = CvtDate(SIN10306);
              WDatPeritagem  = DATA_PERITAGEM;

              //-----------------------------------------------
              // Gravar o Registo
              //-----------------------------------------------
              EXSR ADDRecord;
              READE (NRPROC) RSINP103;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: ExtractDM_DC
        // Descrição.: Extrair registos de Danos Materiais e Corporais.
        //==============================================================
        BEGSR ExtractDM_DC;
           SETLL (NRPROC) RSINP104;
           READE (NRPROC) RSINP104;
           DOW (NOT %EOF());
              //-----------------------------------------------
              // Formatar os campos da Tabela
              //-----------------------------------------------
              IF (%XLATE(StrLo:StrUp:SIN10401) = 'SE');
                 WTipInterv = 'S';
                 WCodInterv = %INT(NORDEM);
              ELSE;
                 WTipInterv = 'T';
                 WCodInterv = %INT(NORDEM);
              ENDIF;
              WMsTpInt = SIN10401;
              WMsTpSin = 3;   // Danos Corporais
              WMsSqInt = %DEC(NORDEM:2:0);
              EXSR GetApolice;           // Obter Elementos em falta
              PEDAUTORIZDESM = SIN10411; // Necessidade Desmontagem
              MATRICULA2     = MATRICULA;// Matricula
              DATPEDAUTDESM  = CvtDate(SIN10423); // Pedido desmontagem
              DATAUTORIZDESM = CvtDate(SIN10424); // Autorização desmontagem
              DATCONPERITAG  = CvtDate(SIN10413);
              DATDISPRELATOR = CvtDate(SIN10414);
              DATA_PERITAGEM = CvtDate(SIN10412);
              WDatPeritagem  = DATA_PERITAGEM;

              //-----------------------------------------------
              // Gravar o Registo
              //-----------------------------------------------
              EXSR ADDRecord;
              READE (NRPROC) RSINP104;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: getApolice
        // Descrição.: Obter elementos em falta localizados no AS/400.
        //==============================================================
        BEGSR getApolice;
           APOLICE        = *ZEROS;   // Apólice
           MARCA          = *BLANKS;   // Marca
           CAPITSEGDTSIN  = *ZEROS;    // Capital Seguro
           FRANQUIA       = *ZEROS;  // Franquia
           MODELODESCRIT  = '9999';
           COBERTURA      = WCobertura; // Pode ter mais que uma
           TIPO_VEICULO   = 'PEAO';
           INTERVENIENTE  = *ZEROS;

           CHAIN ('P':WAaProc:WNuProc) GSINPROF;
           IF (%FOUND());
              CAPITSEGDTSIN  = PRC_CAP2; // Capital Seguro
              FRANQUIA       = PRC_FRQ2; // Franquia
              APOLICE        = PRC_AP;   // Apólice

              CHAIN (PRC_RM:PRC_AP) BASEGRFA;
              IF (%FOUND());
                 IF (APO_CAPDPI > *ZEROS);
                    TIPO_SINISTRO  = 'S006'; // Responsabilidade Civil Geral
                 ELSE;
                    TIPO_SINISTRO  = 'S011'; // Responsabilidade Civil Geral
                 ENDIF;
              ENDIF;

              CHAIN (WAaProc:WNuProc:WMsTpInt:WMsSqInt:WMsTpSin) RSINP114;
              IF (%FOUND());
                 IF (REL_SEQUENCIA = 1);
                    WIcSegProc = *ON;
                 ENDIF;
                 INTERVENIENTE  = (900000 + REL_NUMINT);
                 MARCA          = REL_ASMARCA;   // Marca
                 EXSR decCategoria;
              ENDIF;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: decCategoria
        // Descrição.: Descodificar a Categoria da Viatura.
        //==============================================================
        BEGSR decCategoria;
           IF (REL_ASCATV = 1); // Ligeiro
              TIPO_VEICULO   = 'AUT_01';
           ELSEIF (REL_ASCATV = 2); // Caminheta
              TIPO_VEICULO   = 'AUTO_01';
           ELSEIF (REL_ASCATV = 3); // Pesado
              TIPO_VEICULO   = 'AUT_04';
           ELSEIF (REL_ASCATV = 4); // Motociclo
              TIPO_VEICULO   = 'AUT_02';
           ELSEIF (REL_ASCATV = 5); // Diversos Viatura
              TIPO_VEICULO   = 'AUT_01';
           ELSEIF (REL_ASCATV = 6); // JEEP
              TIPO_VEICULO   = 'AUT_01';
           ELSEIF (REL_ASCATV = 7); // Carrinha Familiar
              TIPO_VEICULO   = 'AUT_01';
           ELSE;
              IF (REL_ASNMMAT <> *BLANKS);
                 TIPO_VEICULO   = 'AUT_01';
              ELSE;
                 TIPO_VEICULO   = 'PEAO';
              ENDIF;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: addRecord
        // Descrição.: Adicionar um novo registo à Tabela de Lesados.
        //==============================================================
        BEGSR AddRecord;

           //-------------------------------------------
           // Formatar os campos a enviar - comuns
           //-------------------------------------------
           ANO            = 2000 + WAaProc;  // Ano do Processo
           NUMSINISTRO    = 90000 + WNuProc; // Número Processo
           DATA_PEDIDO    = DATA_PERITAGEM;
           PERITAGEMCANC  = ' ';      // DFT

           //-------------------------------------------
           // Formatar valores por defeito
           //-------------------------------------------
           RAMO           = 31;       // Ramo Automóvel (Cod. Lusitania)
           MODALIDADE     = '00';     // Modalidade (Cod. Lusitania)
           SUBMODALIDADE  = '00';     // Sub-Modalidade (Cod.Lusitania)
           ENVIADA_PERITG = 'S';      // DFT
           TIPO_PERITAGEM = 'TRD';    // Tradicional
           OFICINA_REPAR  = *ZEROS;   // DFT
           PERITO         = *ZEROS;   // DFT
           OBSERVACOES    = '.';      // DFT
           DATA_ENVIO_PER = *ZEROS;   // Não existe informação electronica
           NUM_DIAS_REPAR = *ZEROS;   //  "    "        "           "
           DATA_INICIO_RP = *ZEROS;   //  "    "        "           "
           MOTIVOS_PERITA = *BLANKS;  //  "    "        "           "
           ORDEM_REPARACA = *BLANKS;  //  "    "        "           "
           DATA_ORDEM_REP = *ZEROS;   //  "    "        "           "
           PERIODO_PERITA = '0';      // Indiferente
           TIPO_VISTORIA  = '1';      // Oficina
           VALOR_LIM_PERD = *ZEROS;   // Não existe informação electronica
           CONDICIONAL    = 1;        // DFT
           VEICULO_IMOBIL = *BLANKS;  // Não existe informação electronica
           DESCRICAODANOS = '.';      // DFT
           DOSSIER        = *BLANKS;  // Não existe informação electronica
           ID_PERITAGEM   = *BLANKS;  // DFT
           LOGIN          = 'JMTAG1'; // DFT
           TIPOINTERVENIE = 'L';      // DFT
           MODELO         = '9999';   // DFT
           DATA_RECEPCAO  = *ZEROS;   // Não existe informação electronica
           DATA_RESPOSTA  = *ZEROS;   //  "    "         "         "
           VALORREPARACAO = *ZEROS;   //  "    "         "         "
           OFICINARECOMEN = 'N';      // DFT
           NUMDIASALUGUER = *ZEROS;   // Não existe informação electronica
           PERDA_TOTAL    = 'N';      // DFT
           AQUISICAOSALVA = 'N';      // DFT
           DATRECEPDOCVEI = *ZEROS;   // Não existe informação electronica
           DATNOTCONCPER  = *ZEROS;   //  "     "       "          "

           //----------------------------------------
           // Só grava se existirem Interveninentes
           //----------------------------------------
           IF ((INTERVENIENTE  > *ZEROS) AND (DATA_PEDIDO > *ZEROS));
              EXCEPT AddRec;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: CrtOpPerit
        // Descrição.: Construir o ficheiro Interface de Lesados.
        //==============================================================
        BEGSR CrtOpPerit;
           //--------------------------------------------------------
           // Cria Script de Transferência
           //--------------------------------------------------------
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/OPPERIT)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           IF (NOT WIcErro);
              MONITOR;
                 WStdCmd = 'CRTPF FILE(QTEMP/OPPERIT) RCDLEN(516)';
                 WStdLen = %LEN(WStdCmd);
                 shell(WStdCmd:WStdLen);
              ON-ERROR;
                 WIcErro = *ON;
              ENDMON;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: GetCobertura
        // Descrição.: Obter a cobertura associada ao Sinitro.
        //==============================================================
        BEGSR GetCobertura;
           WCobertura = *BLANKS;
           WPRV_RCA = *ZEROS;
           WPRV_DP1 = *ZEROS;
           WPRV_DP2 = *ZEROS;
           SETLL (WAaProc:WNuProc) GSINPRV;
           READE (WAaProc:WNuProc) GSINPRV;
           DOW (NOT %EOF());
              WPRV_RCA += PRV_VALR1;
              WPRV_DP1 += PRV_VALR3;
              WPRV_DP2 += PRV_VALR45;
              READE (WAaProc:WNuProc) GSINPRV;
           ENDDO;

           IF (WPRV_RCA > *ZEROS); // Risco I
              WCobertura = '001';
           ENDIF;

           IF (WPRV_DP1 > *ZEROS); // Risco III
              WCobertura = '003';
           ENDIF;

           IF (WPRV_DP2 > *ZEROS); // Risco IV+V
              WCobertura = '006';
           ENDIF;
        ENDSR;

        //================================================================
        // Subrotina..: SendSFTP
        // Objectivo..: Colocar o ficheiro de Interface gerado no share
        //              da Lusitania.
        //================================================================
        BEGSR SendSFTP;
           WNomAS400 = 'OPPERIT';
           WSessionID = $SetFiles(%TRIM(WNomAS400));
           MONITOR;
              INT_NURECORD = $GetQtdeReg();
           ON-ERROR;
              INT_NURECORD = *ZEROS;
           ENDMON;
           $RmvDBSession(WSessionID);

           IF (INT_NURECORD > *ZEROS);
           MONITOR;
              WDatChar = %CHAR(%SUBDT(%DATE():*YEARS)*10000+
                               %SUBDT(%DATE():*MONTHS)*100+
                               %SUBDT(%DATE():*DAYS)) + '_' +
                         %CHAR(%SUBDT(%TIME():*HOURS)*10000+
                               %SUBDT(%TIME():*MINUTES)*100+
                               %SUBDT(%TIME():*SECONDS));
              WNomPase = %TRIM(WNomAS400)+'_'+%TRIM(WDatChar);
              WStdCmd = 'QSYS/CPYTOSTMF FROMMBR(''/qsys.lib/qtemp.lib/'+
                                     %TRIM(WNomAS400)+'.FILE/'+
                                     %TRIM(WNomAS400)+'.MBR'') +
                                     TOSTMF(''/interfaces/pendente/'+
                                     %TRIM(WNomPASE) + ''') STMFOPT(*REPLACE) +
                                     STMFCCSID(*STDASCII)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WIcErro = *ON;
           ENDMON;
           ENDIF;

           CHAIN (WCdRemessa) RINTP001;
           IF (%FOUND());
              INT_CDPRCEXEC = 3;
              INT_INT00102  = %TIMESTAMP();
              UPDATE RINTP001 %FIELDS(INT_CDPRCEXEC:INT_INT00102);
           ENDIF;
        ENDSR;

       //========================================================
       // Subrotina...: RegOutput
       // Objectivo...: Registar ficheiros gerados
       //========================================================
       BEGSR RegOutput;
          INT_CDREMESSA  = WCdRemessa;
          INT_NUSEQU     = $GetNextKey('FINTP004':'NUSEQU':
                          'CDREMESSA = ''' + %CHAR(WCdRemessa) + '''');
          INT_NOMAS400   = WNomAs400;
          INT_NOMPASE    = WNomPase;
          INT_ICENVIADO  = *ON;
          INT_DTENVIO    = %DATE();
          WRITE RINTP004;
       ENDSR;
      /END-FREE
     C*====================================================================
     OOPPERIT   EADD         ADDREC
     O                       RAMO                 2                             NÚMERO
     O                       MODALIDADE           4                             NÚMERO
     O                       SUBMODALIDADE        6                             NOME
     O                       ANO                 10                             MORADA
     O                       NUMSINISTRO         15                             LOCALI
     O                       INTERVENIENTE       25
     O                       DATA_PEDIDO         33
     O                       ENVIADA_PERITG      34
     O                       TIPO_PERITAGEM      37
     O                       OFICINA_REPAR       47
     O                       PERITO              57
     O                       OBSERVACOES        107
     O                       DATA_ENVIO_PER     115
     O                       NUM_DIAS_REPAR     125
     O                       DATA_INICIO_RP     133
     O                       MOTIVOS_PERITA     134
     O                       ORDEM_REPARACA     135
     O                       DATA_ORDEM_REP     143
     O                       PERIODO_PERITA     144
     O                       TIPO_VISTORIA      145
     O                       TIPO_SINISTRO      149
     O                       VALOR_LIM_PERD     159
     O                       CONDICIONAL        160
     O                       APOLICE            167
     O                       COBERTURA          170
     O                       MATRICULA2         182
     O                       MARCA              189
     O                       VEICULO_IMOBIL     190
     O                       CAPITSEGDTSIN      200
     O                       FRANQUIA           210
     O                       DESCRICAODANOS     260
     O                       DOSSIER            292
     O                       ID_PERITAGEM       324
     O                       LOGIN              332
     O                       TIPO_VEICULO       338
     O                       TIPOINTERVENIE     339
     O                       MODELO             379
     O                       DATA_RECEPCAO      387
     O                       DATA_RESPOSTA      395
     O                       VALORREPARACAO     405
     O                       OFICINARECOMEN     406
     O                       PEDAUTORIZDESM     407
     O                       DATPEDAUTDESM      415
     O                       DATAUTORIZDESM     423
     O                       NUMDIASALUGUER     433
     O                       PERDA_TOTAL        434
     O                       AQUISICAOSALVA     435
     O                       DATRECEPDOCVEI     443
     O                       DATCONPERITAG      451
     O                       DATDISPRELATOR     459
     O                       PERITAGEMCANC      460
     O                       DATNOTCONCPER      468
     O                       DATA_PERITAGEM     476
     O                       MODELODESCRIT      516
     P*==================================================================
     P* Processo..: CvtDate
     P* Objectivo.: Convertar um campo de um formato data em
     P*             numerico de 8 posições
     P*==================================================================
     P CvtDate         B                   EXPORT
     D CvtDate         PI             8S 0
     D    DatIN                        D   Const OPTIONS(*NULLIND)
     D
     D WRetDate        S              8S 0
     D WDatChar        S             10A
     D WNewDate        S              8A
     D*==================================================================
      /FREE
         WRetDate = *ZEROS;

         MONITOR;
            WDatChar = %CHAR(DatIN:*ISO);
            WNewDate = %SUBST(WDatChar:1:4) +
                       %SUBST(WDatChar:6:2) +
                       %SUBST(WDatChar:9:2);
            WRetDate = %INT(WNewDate);
            IF (WRetDate = 00010101);
               WRetDate = *ZEROS; // *ZEROS;
            ENDIF;
         ON-ERROR;
         ENDMON;

         RETURN WRetDate;
      /END-FREE
     P CvtDate         E
