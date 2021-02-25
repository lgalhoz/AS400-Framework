     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*=====================================================================
     H* Programa...: PINTO022
     H* Objectivo..: Carregar Interface de Lesados para integrar nos
     H*              Sistemas de Informação da Lusitania.
     H*
     H* Observações: Este programa é invocado na cadeia de geração
     H*              automática de Interfaces.
     H*=====================================================================
     FOPLESAD   UF A F  388        DISK    USROPN
     FFINTP001  UF   E           K DISK    PREFIX(INT_)
     FFINTP004  UF A E           K DISK    PREFIX(INT_)
     FFSINP10201IF   E           K DISK
     FFSINP10301IF   E           K DISK
     FFSINP10401IF   E           K DISK
     FFSINP105  IF   E           K DISK
     FFSINP11402UF   E           K DISK    PREFIX(REL_)
     FGSINPRO   IF   E           K DISK    EXTFILE('G.SINPRO') PREFIX(PRC_)
     FGSININT   IF   E           K DISK    EXTFILE('G.SININT') PREFIX(INT_)
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
     D P_DatInicio     S              8S 0
     D P_DatAltera     S              8S 0
     D WDatInicio      S               D
     D WDatAltera      S              8S 0 INZ(*ZEROS)
     D WDatRefer       S               D
     D WDataPart       S                   LIKE(DATAPART)
     D WAaProc         S                   LIKE(REL_AAPROC)
     D WNuProc         S                   LIKE(REL_NUPROC)
     D WMsTpInt        S                   LIKE(REL_MSTPINT)
     D WMsSqInt        S                   LIKE(REL_MSSQINT)
     D WMsTpSin        S                   LIKE(REL_MSTPSIN)
     D WIcSegProc      S               N
     D WICCPRS         S               N
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
     I*===================================================================
     IOPLESAD   NS  01
     I                                  1    2 0RAMO
     I                                  3    4  MODALIDADE
     I                                  5    6  SUBMODALIDADE
     I                                  7   10 0ANO
     I                                 11   15 0NUMSINISTRO
     I                                 16   25 0LESADO
     I                                 26   26  ESTCIVDANOS
     I                                 27   38  MATRICULA2
     I                                 39   58  MARCA
     I                                 59   68 0CONDLESADO
     I                                 69   75  APOLCONGENERE
     I                                 76   76  PESSOA_A_CARGO
     I                                 77   97  NIB
     I                                 98   98  RECLAMACAO_JUD
     I                                 99  138  MODELO
     I                                139  141 0RESPONSAB
     I                                142  151 0NUM_DIAS_ALUGU
     I                                152  152  IMPOSS_CONTACT
     I                                153  160 0DAT_PRIM_CONTA
     I                                161  161 0MEIOS_CONTACTO
     I                                162  169 0DAT_PEDIDO_PAR
     I                                170  177 0DAT_RECEP_PART
     I                                178  178  PENALIZCAO
     I                                179  186 0COM_ASSUNC_RES
     I                                187  187  POSICAO_SEGURA
     I                                188  195 0COM_DESACORDO
     I                                196  203 0COM_POS_FINAL
     I                                204  204  PAGAM_OFICINA
     I                                205  212 0DAT_PAG_INDEM
     I                                213  220 0DAT_ENTR_DESP
     I                                221  230 0SEQUENCIA
     I                                231  231  DAN_A_CARGA
     I                                232  239  LOGIN
     I                                240  247 0DATA
     I                                248  255 0DAT_DESPESA
     I                                256  263 0DAT_RECL_VALOR
     I                                264  264  EPS
     I                                265  272 0DAT_PED_INDEM
     I                                273  280 0DAT_SOL_EXAME
     I                                281  288 0DAT_REC_EXAME
     I                                289  296 0DAT_DIS_EXAME
     I                                297  304 0DAT_EMIS_ALTA
     I                                305  312 0DAT_DC_QUANT
     I                                313  313  ACEIT_PROP_PRO
     I                                314  321 0DAT_ASS_RES_CO
     I                                322  329 0DAT_PRIM_PED
     I                                330  337 0DAT_PED_REG
     I                                338  345 0DAT_ULTIM_PED
     I                                346  346  ASSUNC_RESPO
     I                                347  347  NECESS_EXAME
     I                                348  355 0DAT_PAG_INDEMN
     I                                356  363 0COM_ASS_RESP
     I                                364  371 0DAT_ENCERRA
     I                                372  373  MOT_ENCERRA
     I                                374  375 0FSANC
     I                                376  383 0DRECUP
     I                                384  385 0FSATD
     I                                386  388 0FSARESP
     C*===========================================================
      /FREE
        //-----------------------------------------------------
        // Validar se está em execução o gerador de Interfaces
        //-----------------------------------------------------
        SETLL (*HIVAL) RINTP001;
        READP RINTP001;
        IF (INT_CDPRCEXEC <> 1) OR %EOF();
           *INLR = *ON;
           RETURN;
        ENDIF;
        WCdRemessa = INT_CDREMESSA;
        WDatInicio = INT_DTINICIO;
        WDatAltera = %SUBDT(INT_DTINICIO:*YEARS)*10000+
                     %SUBDT(INT_DTINICIO:*MONTHS)*100+
                     %SUBDT(INT_DTINICIO:*DAYS);
        WIcErro = *OFF;

        EXSR CrtOPLesad;    // Criar ficheiro temporário na QTEMP
        IF (NOT WIcErro);
           OPEN OPLesad;
           EXSR AddLesados; // Carregar o ficheiro com os Intervenientes
           CLOSE OPLesad;
        ENDIF;
        EXSR SendSFTP;
        *INLR = *ON;

        //==============================================================
        // Subrotina.: CrtOPLesad
        // Descrição.: Construir o ficheiro Interface de Lesados.
        //==============================================================
        BEGSR CrtOPLesad;
           //--------------------------------------------------------
           // Cria Script de Transferência
           //--------------------------------------------------------
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/OPLESAD)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           IF (NOT WIcErro);
              MONITOR;
                 WStdCmd = 'CRTPF FILE(QTEMP/OPLESAD) RCDLEN(388)';
                 WStdLen = %LEN(WStdCmd);
                 shell(WStdCmd:WStdLen);
              ON-ERROR;
                 WIcErro = *ON;
              ENDMON;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: addLesados
        // Descrição.: Adicionar registos de Lesados.
        //             Para o efeito selecciona todos os registos
        //             encerrados a seguir à data inserida como
        //             Parâmetro, junto com os processos ainda abertos.
        //==============================================================
        BEGSR AddLesados;
           SETLL *LOVAL RSINP105;
           READ RSINP105;
           DOW (NOT %EOF());
              IF (%NULLIND(SIN10506)=*ON) OR
                 (SIN10506 >= WDatInicio);
                 IF (SIN10508 = -1) OR ((SIN10508 <> -1) AND
                      (NRPROC >= '1390000'));
                    IF (CvtDate(SIN10512) >= WDatAltera);
                       MONITOR;
                          IF (%NULLIND(SIN10506)=*ON);
                             DAT_ENCERRA    = *ZEROS;
                             MOT_ENCERRA    = *BLANKS;
                          ELSE;
                             DAT_ENCERRA    = CvtDate(SIN10506);
                             MOT_ENCERRA    = '02';
                          ENDIF;
                          WIcSegProc = *OFF;
                          ProcCodif  = NRPROC;
                          WAaProc    = ProcAno;
                          WNuProc    = ProcNum;
                          WDataPart  = DATAPART;
                          EXSR ExtractDC;
                          EXSR ExtractDM;
                          EXSR ExtractDM_DC;

                          //----------------------------------
                          // Processar o registo do Segurado
                          //----------------------------------
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
        // Descrição.: Adicionar o Segurado
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
                 MARCA          = REL_ASMARCA;          // Marca
                 MATRICULA2     = REL_ASNMMAT;          // Matricula
                 APOLCONGENERE  = REL_ASNUAPO;          // Apólice Congenere
                 CONDLESADO     = (900000 + REL_NUMINT);// Numero Interno
                 LESADO         = CONDLESADO;           // Sequ.Lesado
                 SEQUENCIA      = REL_SEQUENCIA;        // Sequ.Lesado
                 REL_DATENVIO  = %DATE();
                 REL_HORENVIO  = %TIME();
                 REL_QTDENVIO  = REL_QTDENVIO + 1;
                 REL_SEQLUSI   = SEQUENCIA;
                 UPDATE RSINP114 %FIELDS(REL_DATENVIO:REL_HORENVIO:
                                        REL_QTDENVIO:REL_SEQLUSI);
                 LEAVE;
              ENDIF;
              READ RSINP114;
           ENDDO;

           //-------------------------------------------------
           // Formatar com valores por defeito os restantes
           //-------------------------------------------------
           IF (WIcSegProc);
              ESTCIVDANOS    = '1';          // Sem Informação
              MODELO         = *BLANKS;
              RESPONSAB      = *ZEROS;
              FSARESP = 100;
              IMPOSS_CONTACT = 'N';
              DAT_PRIM_CONTA = *ZEROS;
              COM_ASSUNC_RES = *ZEROS;
              COM_DESACORDO  = *ZEROS;
              COM_POS_FINAL  = *ZEROS;
              PAGAM_OFICINA  = 'F';
              RECLAMACAO_JUD = 'N';
              DAT_PAG_INDEM  = *ZEROS;
              DAN_A_CARGA    = 'N';
              DAT_PED_INDEM  = *ZEROS;
              DAT_SOL_EXAME  = *ZEROS;
              DAT_REC_EXAME  = *ZEROS;
              DAT_DIS_EXAME  = *ZEROS;
              DAT_EMIS_ALTA  = *ZEROS;
              DAT_DC_QUANT   = *ZEROS;
              ACEIT_PROP_PRO = *ZEROS;
              DAT_ASS_RES_CO = *ZEROS;
              DAT_PRIM_PED   = *ZEROS;
              DAT_PED_REG    = *ZEROS;
              DAT_ULTIM_PED  = *ZEROS;
              ASSUNC_RESPO   = 'N';
              NECESS_EXAME   = 'N';
              DAT_PAG_INDEMN = *ZEROS;
              COM_ASS_RESP   = *ZEROS;
              // DAT_ENCERRA = *ZEROS;
              // DATAPART    = %DATE(19000101:*ISO); // Forçar valor
              DATAPART       = WDataPart;
              POSI_SEG       = '0%';                 // Forçar valor

              //-----------------------------------------------
              // Gravar o Registo
              //-----------------------------------------------
              EXSR ADDRecord;
           ELSE;
              EVAL LESADO = *ZEROS; // Erro
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: ExtractDC
        // Descrição.: Extraier registos de Danos Corporais.
        //==============================================================
        BEGSR ExtractDC;
           SETLL (NRPROC) RSINP102;
           READE (NRPROC) RSINP102;
           DOW (NOT %EOF());
              WTipInterv = 'X';
              WCodInterv = *ZEROS;
              WMsTpInt = SIN10201;
              WMsTpSin = 2;   // Danos Corporais
              WMsSqInt = %DEC(NORDEM:2:0);
              EXSR GetApolice;              // Obter Elementos em falta
              //-----------------------------------------------
              // Formatar os campos da Tabela
              //-----------------------------------------------
              MATRICULA2     = *BLANKS;      // ### Não Aplicável em DC
              MARCA          = *BLANKS;      // ### Não Aplicável em DC
              ESTCIVDANOS    = '2';          // Danos Corporais
              APOLCONGENERE  = *BLANKS;      // ### Não Aplicável em DC
              MODELO         = *BLANKS;      // ### Não aplicável em DC
              RESPONSAB      = SIN10217;     // Perentagem de Responsabilida

              //-----------------------------------------------------------
              // Calcular percentagem de Responsabilidade
              //-----------------------------------------------------------
              FSARESP = *ZEROS; // Outro formatar valor por defeito
              IF (NRPROC >= '1390000');
                 FSARESP = SIN10223;
              // IF (WIcCPRS = *OFF);  // Não está ao abrigo do Decreto Lei 2006
              //    FSARESP = 100;
              // ELSEIF (SIN10201 = 'TERCEIRO'); // Calcular por oposição ao Segurado
              //    FSARESP = 100 - SIN10217;
              // ELSEIF (SIN10201 = 'SEGURADO'); // Calcular para o Segurado
              //    FSARESP = SIN10217;
              // ENDIF;
              ENDIF;

              IMPOSS_CONTACT = 'N';
              DAT_PRIM_CONTA = *ZEROS;
              COM_ASSUNC_RES = CvtDate(SIN10206);
              COM_DESACORDO  = CvtDate(SIN10209);
              COM_POS_FINAL  = CvtDate(SIN10210);
              PAGAM_OFICINA  = 'F';          // ### Não aplicável em DC
              DAT_PAG_INDEM  = CvtDate(SIN10218);
              DAN_A_CARGA    = 'N';          // ### Não Aplicável em DC
              DAT_PED_INDEM  = CvtDate(SIN10202);
              DAT_SOL_EXAME  = CvtDate(SIN10203);
              DAT_REC_EXAME  = CvtDate(SIN10204);
              DAT_DIS_EXAME  = CvtDate(SIN10205);
              DAT_EMIS_ALTA  = CvtDate(DataAlta);
              DAT_DC_QUANT   = CvtDate(DataQuant);
              ACEIT_PROP_PRO = SIN10207;
              DAT_ASS_RES_CO = CvtDate(SIN10208);
              DAT_PRIM_PED   = *ZEROS;      // ### Não Aplicável em DC
              DAT_PED_REG    = *ZEROS;      // ### Não Aplicável em DC
              DAT_ULTIM_PED  = *ZEROS;      // ### Não Aplicável em DC
              IF (SIN10217 > *ZEROS); // SIN10217
                 ASSUNC_RESPO   = 'S';
              ELSE;
                  IF (cvtDate(SIN10211) > *ZEROS);
                     ASSUNC_RESPO   = 'S';
                  ELSE;
                     ASSUNC_RESPO   = 'N';
                  ENDIF;
              ENDIF;
              IF (CvtDate(SIN10203) > *ZEROS);
                 NECESS_EXAME   = 'S';
              ELSE;
                 NECESS_EXAME   = 'N';
              ENDIF;
              DAT_PAG_INDEMN = CvtDate(SIN10218);
              COM_ASS_RESP   = CvtDate(SIN10206);
              // DAT_ENCERRA    = CvtDate(SIN10221);

              //-----------------------------------------------
              // Gravar o Registo
              //-----------------------------------------------
              EXSR ADDRecord;
              READE (NRPROC) RSINP102;
           ENDDO;
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
              ESTCIVDANOS    = '1';          // Danos Materiais
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
              EXSR GetApolice;              // Obter Elementos em falta
              MATRICULA2     = MATRICULA;
              RESPONSAB      = SIN10328;    // Percentagem de Responsabilida

              //-----------------------------------------------------------
              // Calcular percentagem de Responsabilidade
              //-----------------------------------------------------------
              FSARESP = *ZEROS; // Outro formatar valor por defeito
              IF (NRPROC >= '1390000');
                 FSARESP = SIN10332;
              // IF (WIcCPRS = *OFF);  // Não está ao abrigo do Decreto Lei 2006
              //    FSARESP = 100;
              // ELSEIF (SIN10301 = 'TERCEIRO'); // Calcular por oposição ao Segurado
              //    FSARESP = 100 - SIN10328;
              // ELSEIF (SIN10301 = 'SEGURADO'); // Calcular para o Segurado
              //    FSARESP = SIN10328;
              // ENDIF;
              ENDIF;

              IMPOSS_CONTACT = SIN10304;
              DAT_PRIM_CONTA = CvtDate(SIN10303);
              COM_ASSUNC_RES = CvtDate(SIN10312);
              COM_DESACORDO  = CvtDate(SIN10313);
              COM_POS_FINAL  = CvtDate(SIN10314);
              IF (%TRIM(SIN10316) = 'OFICINA') OR
                 (%TRIM(SIN10316) = 'CONGENERE');
                  PAGAM_OFICINA  = 'S';
                  DAT_PAG_INDEM  = 99991231;
              ELSE;
                  DAT_PAG_INDEM  = CvtDate(SIN10317); // Recibo de Quitação
                  PAGAM_OFICINA  = 'N';
              ENDIF;
              IF (SIN10328 > *ZEROS);  // SIN10328
                  ASSUNC_RESPO   = 'S';
              ELSE;
                  IF (cvtDate(SIN10319) > *ZEROS);
                     ASSUNC_RESPO   = 'S';
                  ELSE;
                     ASSUNC_RESPO   = 'N';
                  ENDIF;
              ENDIF;
              DAN_A_CARGA    = DanosCarga;
              DAT_PED_INDEM  = *ZEROS;       // ### Não Aplicável em DM
              DAT_SOL_EXAME  = *ZEROS;       // ### Não Aplicável em DM
              DAT_REC_EXAME  = *ZEROS;       // ### Não Aplicável em DM
              DAT_DIS_EXAME  = *ZEROS;       // ### Não Aplicável em DM
              DAT_EMIS_ALTA  = *ZEROS;       // ### Não Aplicável em DM
              DAT_DC_QUANT   = *ZEROS;       // ### Não Aplicável em DM
              ACEIT_PROP_PRO = 'N';          // ### Não Aplicável em DM
              DAT_ASS_RES_CO = *ZEROS;       // ### Não Aplicável em DM
              DAT_PRIM_PED   = *ZEROS;       // ### Não Aplicável em DM
              DAT_PED_REG    = *ZEROS;       // ### Não Aplicável em DM
              DAT_ULTIM_PED  = *ZEROS;       // ### Não Aplicável em DM
              NECESS_EXAME   = 'N';          // ### Não Aplicável em DM
              COM_ASS_RESP   = *ZEROS;       // ### Não Aplicável em DM
              DAT_PAG_INDEMN = *ZEROS;       // ### Não Aplicável em DM
              // DAT_ENCERRA    = CvtDate(SIN10330);

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
              MATRICULA2     = MATRICULA;
              ESTCIVDANOS    = '3';         // Danos Corporais e Materiais
              WMsTpInt = SIN10401;
              WMsTpSin = 3;   // Danos Corporais
              WMsSqInt = %DEC(NORDEM:2:0);
              EXSR GetApolice;              // Obter Elementos em falta
              RESPONSAB      = SIN10425;    // Percentagem de Responsabilid a

              //-----------------------------------------------------------
              // Calcular percentagem de Responsabilidade
              //-----------------------------------------------------------
              FSARESP = *ZEROS; // Outro formatar valor por defeito
              IF (NRPROC >= '1390000');
                 FSARESP = SIN10436;
              // IF (WIcCPRS = *OFF);  // Não está ao abrigo do Decreto Lei 2006
              //    FSARESP = 100;
              // ELSEIF (SIN10401 = 'TERCEIRO'); // Calcular por oposição ao Segurado
              //    FSARESP = 100 - SIN10425;
              // ELSEIF (SIN10401 = 'SEGURADO'); // Calcular para o Segurado
              //    FSARESP = SIN10425;
              // ENDIF;
              ENDIF;

              IMPOSS_CONTACT = SIN10422;
              DAT_PRIM_CONTA = CvtDate(SIN10409);
              COM_ASSUNC_RES = CvtDate(SIN10403);
              COM_DESACORDO  = CvtDate(SIN10406);
              COM_POS_FINAL  = CvtDate(SIN10407);
              IF (%TRIM(SIN10427) = 'OFICINA') OR
                 (%TRIM(SIN10427) = 'CONGENERE');
                  PAGAM_OFICINA  = 'S';
                  DAT_PAG_INDEM  = 99991231;
              ELSE;
                  DAT_PAG_INDEM  = CvtDate(SIN10428); // Recibo de Quitação
                  PAGAM_OFICINA  = 'N';
              ENDIF;
              IF (SIN10425 > *ZEROS);
                  ASSUNC_RESPO   = 'S';
              ELSE;
                  IF (cvtDate(SIN10408) > *ZEROS);
                     ASSUNC_RESPO   = 'S';
                  ELSE;
                     ASSUNC_RESPO   = 'N';
                  ENDIF;
              ENDIF;

              DAT_PAG_INDEM  = CvtDate(SIN10428);
              DAN_A_CARGA    = DanosCarga;
              DAT_PED_INDEM  = *ZEROS;       // ### Não Aplicável em DM_DC
              DAT_SOL_EXAME  = *ZEROS;       // ### Não Aplicável em DM_DC
              DAT_REC_EXAME  = *ZEROS;       // ### Não Aplicável em DM_DC
              DAT_DIS_EXAME  = *ZEROS;       // ### Não Aplicável em DM_DC
              DAT_EMIS_ALTA  = *ZEROS;       // ### Não Aplicável em DM_DC
              DAT_DC_QUANT   = *ZEROS;       // ### Não Aplicável em DM_DC
              ACEIT_PROP_PRO = 'N';          // ### Não Aplicável em DM_DC
              DAT_ASS_RES_CO = *ZEROS;       // ### Não Aplicável em DM_DC
              DAT_PRIM_PED   = cvtDate(SIN10402);
              DAT_PED_REG    = cvtDate(SIN10404);
              DAT_ULTIM_PED  = *ZEROS;       // ### Não Aplicável em DM_DC
              NECESS_EXAME   = 'N';          // ### Não Aplicável em DM_DC
              DAT_PAG_INDEMN = *ZEROS;       // ### Não Aplicável em DM_DC
              COM_ASS_RESP   = *ZEROS;       // ### Não Aplicável em DM_DC
              // DAT_ENCERRA    = CvtDate(SIN10434);

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
           MARCA          = *BLANKS; // Marca da Viatura
           APOLCONGENERE  = *BLANKS; // Apólice Congenere
           RECLAMACAO_JUD = *BLANKS; // Reclamação Judicial
           MODELO         = *BLANKS; // Modelo da Viatura
           CONDLESADO     = *ZEROS;
           LESADO         = *ZEROS;
           SEQUENCIA      = *ZEROS;

           //------------------------------------------
           // Obter os dados da Apólice partindo do
           // Processo de Sinistro.
           //------------------------------------------
           CHAIN ('P':WAaProc:WNuProc) GSINPROF;
           IF (%FOUND());
              MODELO         = *BLANKS;   // Modelo da Viatura
              IF (PRC_AJUDIC = 'S');
                 RECLAMACAO_JUD = 'S';
              ELSE;
                 RECLAMACAO_JUD = 'N';
              ENDIF;

              //---------------------------------------
              // Obter a Apólices do Processo
              //---------------------------------------
              CHAIN (WAaProc:WNuProc:WMsTpInt:WMsSqInt:WMsTpSin) RSINP114;
              IF (%FOUND());
                 //-------------------------------------------
                 // Campos FSA
                 //-------------------------------------------
                 FSANC  = *ZEROS;
                 DRECUP = *ZEROS;
                 FSATD  = *ZEROS;
                 CHAIN (REL_AAPROC:REL_NUPROC:REL_ASTPINT:REL_ASSQINT) GSININTF;
                 IF (%FOUND());
                    FSANC  = INT_FSANC;
                    DRECUP = INT_DRECUP;
                    FSATD  = INT_FSATD;
                 ENDIF;

                 IF (REL_SEQUENCIA = 1);
                    WIcSegProc = *ON;
                 ENDIF;

                 MARCA          = REL_ASMARCA;          // Marca
                 APOLCONGENERE  = REL_ASNUAPO;          // Apólice Congenere
                 CONDLESADO     = (900000 + REL_NUMINT);// Numero Interno
                 LESADO         = CONDLESADO;
                 SEQUENCIA      = REL_SEQUENCIA;        // Sequ. do Lesado
                 REL_DATENVIO  = %DATE();
                 REL_HORENVIO  = %TIME();
                 REL_QTDENVIO  = REL_QTDENVIO + 1;
                 REL_SEQLUSI   = SEQUENCIA;
                 UPDATE RSINP114 %FIELDS(REL_DATENVIO:REL_HORENVIO:
                                     REL_QTDENVIO:REL_SEQLUSI);
              ELSE;
                 LESADO = *ZEROS;     // Erro
              ENDIF;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: addRecord
        // Descrição.: Adicionar um novo registo à Tabela de Lesados.
        //==============================================================
        BEGSR AddRecord;
           ProcCodif = NRPROC;

           //-------------------------------------------
           // Formatar os campos a enviar - comuns
           //-------------------------------------------
           ANO            = 2000 + WAaProc;  // Ano do Processo
           NUMSINISTRO    = 90000 + WNuProc; // Número Processo
           DAT_RECEP_PART = CvtDate(DATAPART);

           //-------------------------------------------
           // Posição do Segurado
           //-------------------------------------------
           POSI_SEG = %XLATE(StrLo:StrUp:POSI_SEG);
           IF (POSI_SEG = '0%');
              POSICAO_SEGURA = '1';
           ELSEIF (POSI_SEG = '100%');
              POSICAO_SEGURA = '2';
           ELSEIF (%SUBST(POSI_SEG:1:1) = 'R'); // Resp. Repart.
              POSICAO_SEGURA = '3';
           ELSEIF (%SUBST(POSI_SEG:1:1) = 'N'); // Não Manifest
              POSICAO_SEGURA = '4';
           ELSEIF (%SUBST(POSI_SEG:1:1) = 'S'); // Sem informação
              POSICAO_SEGURA = '0';
           ELSEIF (POSI_SEG = '-');
              POSICAO_SEGURA = '0';
           ENDIF;

           //-------------------------------------------
           // Formatar valores por defeito
           //-------------------------------------------
           RAMO           = 31;       // Ramo Automóvel (Cod. Lusitania)
           MODALIDADE     = '00';     // Modalidade (Cod. Lusitania)
           SUBMODALIDADE  = '00';     // Sub-Modalidade (Cod.Lusitania)
           PESSOA_A_CARGO = 'N';      // Pessoa a Cargo
           NIB            = *BLANKS;  // Dados bancários
           NUM_DIAS_ALUGU = *ZEROS;   // Total de dias de Aluguer
           MEIOS_CONTACTO = *ZEROS;
           DAT_PEDIDO_PAR = *ZEROS;
           PENALIZCAO     = 'N';
           DAT_ENTR_DESP  = *ZEROS;
           LOGIN          = 'JMATAG1';
           DATA           = *ZEROS;
           DAT_DESPESA    = *ZEROS;
           DAT_RECL_VALOR = *ZEROS;
           EPS            = 'N';


           //--------------------------------------------
           // Só grava se o Condutor Lesado tiver dados
           //--------------------------------------------
           IF (CONDLESADO > *ZEROS);
              // Aplicar regras de excepção
              IF (ASSUNC_RESPO = 'S');
                 IF (SEQUENCIA = 1);
                    RESPONSAB = 100;
                 ELSE;
                    RESPONSAB = 0;
                 ENDIF;
              ELSE;
                 IF (SEQUENCIA = 1);
                    RESPONSAB = 0;
                 ELSE;
                    RESPONSAB = 100;
                 ENDIF;
              ENDIF;
              EXCEPT ADDRec;
           ENDIF;
        ENDSR;

        //================================================================
        // Subrotina..: SendSFTP
        // Objectivo..: Colocar o ficheiro de Interface gerado no share
        //              da Lusitania.
        //================================================================
        BEGSR SendSFTP;
           WNomAS400 = 'OPLESAD';
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
              EXSR RegOutput;
           ON-ERROR;
              WIcErro = *ON;
           ENDMON;
           ENDIF;

           CHAIN (WCdRemessa) RINTP001;
           IF (%FOUND());
              INT_CDPRCEXEC = 2;
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
     OOPLESAD   EADD         ADDREC
     O                       RAMO                 2                             NÚMERO
     O                       MODALIDADE           4                             NÚMERO
     O                       SUBMODALIDADE        6                             NOME
     O                       ANO                 10                             MORADA
     O                       NUMSINISTRO         15                             LOCALI
     O                       LESADO              25                             CÓDIGO
     O                       ESTCIVDANOS         26                             CÓDIGO
     O                       MATRICULA2          38                             CÓDIGO
     O                       MARCA               58
     O                       CONDLESADO          68
     O                       APOLCONGENERE       75
     O                       PESSOA_A_CARGO      76
     O                       NIB                 97
     O                       RECLAMACAO_JUD      98
     O                       MODELO             138
     O                       RESPONSAB          141
     O                       NUM_DIAS_ALUGU     151
     O                       IMPOSS_CONTACT     152
     O                       DAT_PRIM_CONTA     160
     O                       MEIOS_CONTACTO     161
     O                       DAT_PEDIDO_PAR     169
     O                       DAT_RECEP_PART     177
     O                       PENALIZCAO         178
     O                       COM_ASSUNC_RES     186
     O                       POSICAO_SEGURA     187
     O                       COM_DESACORDO      195
     O                       COM_POS_FINAL      203
     O                       PAGAM_OFICINA      204
     O                       DAT_PAG_INDEM      212
     O                       DAT_ENTR_DESP      220
     O                       SEQUENCIA          230
     O                       DAN_A_CARGA        231
     O                       LOGIN              239
     O                       DATA               247
     O                       DAT_DESPESA        255
     O                       DAT_RECL_VALOR     263
     O                       EPS                264
     O                       DAT_PED_INDEM      272
     O                       DAT_SOL_EXAME      280
     O                       DAT_REC_EXAME      288
     O                       DAT_DIS_EXAME      296
     O                       DAT_EMIS_ALTA      304
     O                       DAT_DC_QUANT       312
     O                       ACEIT_PROP_PRO     313
     O                       DAT_ASS_RES_CO     321
     O                       DAT_PRIM_PED       329
     O                       DAT_PED_REG        337
     O                       DAT_ULTIM_PED      345
     O                       ASSUNC_RESPO       346
     O                       NECESS_EXAME       347
     O                       DAT_PAG_INDEMN     355
     O                       COM_ASS_RESP       363
     O                       DAT_ENCERRA        371
     O                       MOT_ENCERRA        373
     O                       FSANC              375
     O                       DRECUP             383
     O                       FSATD              385
     O                       FSARESP            388
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
         WRetDate = 99991231; // *ZEROS;

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
