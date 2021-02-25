     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*=====================================================================
     H* Programa...: PINTO026
     H* Objectivo..: Carregar Interface de Motivos para integrar nos
     H*              Sistemas de Informação da Lusitania.
     H*
     H* Observações: Este programa é invocado na cadeia de geração
     H*              automática de Interfaces.
     H*=====================================================================
     FOPPRAZO   UF A F   31        DISK    USROPN
     FFINTP001  UF   E           K DISK    PREFIX(INT_)
     FFINTP004  UF A E           K DISK    PREFIX(INT_)
     FFSINP10201IF   E           K DISK
     FFSINP10301IF   E           K DISK
     FFSINP10401IF   E           K DISK
     FFSINP105  IF   E           K DISK
     FFSINP11402UF   E           K DISK    PREFIX(REL_)
     FGSINPRO   IF   E           K DISK    EXTFILE('G.SINPRO') PREFIX(PRC_)
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
     D*==> Função para converter datas
     D CvtDate         PR             8S 0
     D    WDatIN                       D   CONST OPTIONS(*NULLIND)
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
     D WDatAltera      S              8S 0
     D WDatRefer       S               D
     D WAaProc         S                   LIKE(REL_AAPROC)
     D WNuProc         S                   LIKE(REL_NUPROC)
     D WMsTpInt        S                   LIKE(REL_MSTPINT)
     D WMsSqInt        S                   LIKE(REL_MSSQINT)
     D WMsTpSin        S                   LIKE(REL_MSTPSIN)
     D WIcSegProc      S               N
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
     D*==> Para converter me Maiúsuclas ou Minusculas
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyz'
     D*===================================================================
     IOPPRAZO   NS  01
     I                                  1    2 0LRAMO
     I                                  3    4  LMOD
     I                                  5    6  LSMOD
     I                                  7   10 0LANO
     I                                 11   15 0LNPROC
     I                                 16   25 0LESADO
     I                                 26   29  PRAZO
     I                                 30   31  CODIGO
     I*===========================================================
      /FREE
        //-----------------------------------------------------
        // Validar se está em execução o gerador de Interfaces
        //-----------------------------------------------------
        SETLL (*HIVAL) RINTP001;
        READP RINTP001;
        IF (INT_CDPRCEXEC <> 5) OR %EOF();
           *INLR = *ON;
           RETURN;
        ENDIF;
        WCdRemessa = INT_CDREMESSA;
        WDatInicio = INT_DTINICIO;
        WDatAltera = %SUBDT(INT_DTINICIO:*YEARS)*10000+
                     %SUBDT(INT_DTINICIO:*MONTHS)*100+
                     %SUBDT(INT_DTINICIO:*DAYS);
        WIcErro = *OFF;

        EXSR CrtOPPrazo;
        IF (NOT WIcErro);
           OPEN OPPrazo;
           EXSR AddPrazos;
           CLOSE OPPrazo;
        ENDIF;
        EXSR SendSFTP;
        *INLR = *ON;

        //==============================================================
        // Subrotina.: addPrazos
        // Descrição.: Adicionar registos de Prazos.
        //             Para o efeito selecciona todos os registos
        //             encerrados a seguir à data inserida como
        //             Parâmetro, junto com os processos ainda abertos.
        //==============================================================
        BEGSR AddPrazos;
           SETLL *LOVAL RSINP105;
           READ RSINP105;
           DOW (NOT %EOF());
              IF (%NULLIND(SIN10506)=*ON) OR
                 (SIN10506 >= WDatInicio);
                 IF (SIN10508 = -1);
                    IF (CvtDate(SIN10512) >= WDatAltera);
                       MONITOR;
                          ProcCodif  = NRPROC;
                          WAaProc    = ProcAno;
                          WNuProc    = ProcNum;
                          EXSR ExtractDC;
                          EXSR ExtractDM;
                          EXSR ExtractDM_DC;
                       ON-ERROR;
                       ENDMON;
                    ENDIF;
                 ENDIF;
              ENDIF;
              READ RSINP105;
           ENDDO;
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
              IF (%TRIM(SIN10212) <> '00');
                 PRAZO  = 'P7';
                 CODIGO = %TRIM(SIN10212);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10213) <> '00');
                 PRAZO  = 'P8';
                 CODIGO = %TRIM(SIN10213);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10214) <> '00');
                 PRAZO  = 'P9';
                 CODIGO = %TRIM(SIN10214);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10215) <> '00');
                 PRAZO  = 'P10';
                 CODIGO = %TRIM(SIN10215);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10216) <> '00');
                 PRAZO  = 'P11';
                 CODIGO = %TRIM(SIN10216);
                 EXSR ADDRecord;
              ENDIF;
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
              //-----------------------------------------------
              // Formatar os campos da Tabela
              //-----------------------------------------------
              IF (%TRIM(SIN10322) <> '00');
                 PRAZO  = 'P1';
                 CODIGO = %TRIM(SIN10322);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10323) <> '00');
                 PRAZO  = 'P2';
                 CODIGO = %TRIM(SIN10323);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10324) <> '00');
                 PRAZO  = 'P3';
                 CODIGO = %TRIM(SIN10324);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10325) <> '00');
                 PRAZO  = 'P4';
                 CODIGO = %TRIM(SIN10325);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10326) <> '00');
                 PRAZO  = 'P5';
                 CODIGO = %TRIM(SIN10326);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10327) <> '00');
                 PRAZO  = 'P6';
                 CODIGO = %TRIM(SIN10327);
                 EXSR ADDRecord;
              ENDIF;
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
              EXSR getApolice;
              //-----------------------------------------------
              // Formatar os campos da Tabela
              //-----------------------------------------------
              IF (%TRIM(SIN10415) <> '00');
                 PRAZO  = 'P12';
                 CODIGO = %TRIM(SIN10415);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10416) <> '00');
                 PRAZO  = 'P13';
                 CODIGO = %TRIM(SIN10416);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10417) <> '00');
                 PRAZO  = 'P14';
                 CODIGO = %TRIM(SIN10417);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10418) <> '00');
                 PRAZO  = 'P15';
                 CODIGO = %TRIM(SIN10418);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10419) <> '00');
                 PRAZO  = 'P16';
                 CODIGO = %TRIM(SIN10419);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10420) <> '00');
                 PRAZO  = 'P17';
                 CODIGO = %TRIM(SIN10420);
                 EXSR ADDRecord;
              ENDIF;
              IF (%TRIM(SIN10421) <> '00');
                 PRAZO  = 'P18';
                 CODIGO = %TRIM(SIN10421);
                 EXSR ADDRecord;
              ENDIF;
              READE (NRPROC) RSINP104;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: getApolice
        // Descrição.: Obter elementos em falta localizados no AS/400.
        //==============================================================
        BEGSR getApolice;
           LESADO  = *ZEROS;

           //---------------------------------------
           // Obter a Apólices do Processo
           //---------------------------------------
           CHAIN (WAaProc:WNuProc:WMsTpInt:WMsSqInt:WMsTpSin) RSINP114;
           IF (%FOUND());
              IF (REL_NUMINT > *ZEROS);
                 LESADO = (900000 + REL_NUMINT);// Numero Interno
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
           LANO           = 2000 + WAaProc;  // Ano do Processo
           LNPROC         = 90000 + WNuProc; // Número Processo

           //-------------------------------------------
           // Formatar valores por defeito
           //-------------------------------------------
           LRAMO          = 31;       // Ramo Automóvel (Cod. Lusitania)
           LMOD           = '00';     // Modalidade (Cod. Lusitania)
           LSMOD          = '00';     // Sub-Modalidade (Cod.Lusitania)

           //--------------------------------------------
           // Só grava se o Condutor Lesado tiver dados
           //--------------------------------------------
           IF (LESADO > *ZEROS);
              EXCEPT ADDRec;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: CrtOPPrazo
        // Descrição.: Construir o ficheiro Interface de Prazos.
        //==============================================================
        BEGSR CrtOPPrazo;
           //--------------------------------------------------------
           // Cria Script de Transferência
           //--------------------------------------------------------
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/OPPRAZO)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           IF (NOT WIcErro);
              MONITOR;
                 WStdCmd = 'CRTPF FILE(QTEMP/OPPRAZO) RCDLEN(31)';
                 WStdLen = %LEN(WStdCmd);
                 shell(WStdCmd:WStdLen);
              ON-ERROR;
                 WIcErro = *ON;
              ENDMON;
           ENDIF;
        ENDSR;
        //================================================================
        // Subrotina..: SendSFTP
        // Objectivo..: Colocar o ficheiro de Interface gerado no share
        //              da Lusitania.
        //================================================================
        BEGSR SendSFTP;
           WNomAS400 = 'OPPRAZO';
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
     OOPPRAZO   EADD         ADDREC
     O                       LRAMO                2                             NÚMERO
     O                       LMOD                 4                             NÚMERO
     O                       LSMOD                6                             NOME
     O                       LANO                10                             LOCALI
     O                       LNPROC              15                             CÓDIGO
     O                       LESADO              25                             CÓDIGO
     O                       PRAZO               29                             CÓDIGO
     O                       CODIGO              31
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
