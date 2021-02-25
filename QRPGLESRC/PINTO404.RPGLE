     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*=====================================================================
     H* Programa...: PINTO024
     H* Objectivo..: Carregar Interface de Pessoas ou Empresas para
     H*              Integrar nos Sistemas de InformaÁ„o da Lusitania.
     H*
     H* ObservaÁıes: Este programa È invocado na cadeia de geraÁ„o
     H*              autom·tica de Interfaces.
     H*=====================================================================
     FFSINP114  IF   E           K DISK
     FFSINP105  IF   E           K DISK
     FGFSEG     IF   E           K DISK
     FOPLSEG    UF A F  211        DISK    USROPN
     FFINTP001  UF   E           K DISK    PREFIX(INT_)
     FFINTP004  UF A E           K DISK    PREFIX(INT_)
     F*==============================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WIcErro         S               N
     D WDatInicio      S               D
     D WDatAltera      S              8S 0
     D WDatTransf      S              8S 0
     D P_DatInicio     S              8S 0
     D P_DatAltera     S              8S 0
     D WDatRefer       S               D
     D WAaProc         S                   LIKE(AAPROC)
     D WNuProc         S                   LIKE(NUPROC)
     D WIcExiste       S               N
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
     D LstSegurado     DS                  QUALIFIED DIM(5000)
     D   CodSeg                       5S 0
     D WIdxList        S              5S 0
     D WIdxAct         S              5S 0
     D
     D*==> FunÁ„o para converter datas
     D CvtDate         PR             8S 0
     D    WDatIN                       D   CONST OPTIONS(*NULLIND)
     D
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D StrPor          C                   '¿¡»…ÃÕ“”Ÿ⁄«√’‡·ËÈÏÌÚÛ˘˙Á„ıƒÀœ÷‹¬ ‘€+
     D                                      `¥Œœ‚ÍÓÙ˚‰ÎÔˆ¸∫™≈'
     D StrInt          C                   'AAEEIIOOUUCAOaaeeiioouucaoAEIOUAEOU+
     D                                        IIaeiouaeiou..A'
     I********************************************************************
     I* Interface (Pessoas para a Lusitania)                             *
     I********************************************************************
     IOPLSEG    NS
     I                                  1    6 0LNSEG
     I                                  7   56  LNOME
     I                                 57   58  LTITUL
     I                                 59   59  LTIPO
     I                                 60   65  LPROF
     I                                 66  105  LMORA
     I                                106  135  LLOCAL
     I                                136  139  LCPOST
     I                                140  142  LSPOST
     I                                187  194 0LDTTRF
     I                                195  203 0LNIF
     I                                204  211 0LDTNAS
     C****************************************************
      /FREE
        //-----------------------------------------------------
        // Validar se est· em execuÁ„o o gerador de Interfaces
        //-----------------------------------------------------
        SETLL (*HIVAL) RINTP001;
        READP RINTP001;
        IF (INT_CDPRCEXEC <> 3) OR %EOF();
           *INLR = *ON;
           RETURN;
        ENDIF;
        WCdRemessa = INT_CDREMESSA;
        WDatInicio = INT_DTINICIO;
        WDatAltera = %SUBDT(INT_DTINICIO:*YEARS)*10000+
                     %SUBDT(INT_DTINICIO:*MONTHS)*100+
                     %SUBDT(INT_DTINICIO:*DAYS);
        WIcErro = *OFF;

        EXSR CrtOPLSeg;
        IF (NOT WIcErro);
           OPEN OPLSEG;
           EXSR AddPessoas;
           CLOSE OPLSEG;
        ENDIF;
        EXSR SendSFTP;
        *INLR = *ON;

        //==============================================================
        // Subrotina.: addPessoas
        // DescriÁ„o.: Adicionar registos com a informaÁ„o sobre os
        //             Intervenientes do Sinistro.
        //==============================================================
        BEGSR AddPessoas;
           WIdxList = *ZEROS;
           SETLL *LOVAL RSINP105;
           READ RSINP105;
           DOW (NOT %EOF());
              IF (%NULLIND(SIN10506)=*ON) OR
                 (SIN10506 >= WDatInicio);
                 IF (SIN10508 = -1) OR ((SIN10508 <> -1) AND
                      (NRPROC >= '1390000'));
                    IF (CvtDate(SIN10512) >= WDatAltera);
                       MONITOR;
                          ProcCodif = NRPROC;
                          WAaProc   = ProcAno;
                          WNuProc   = ProcNum;
                          EXSR ExtInterv;
                       ON-ERROR;
                       ENDMON;
                    ENDIF;
                 ENDIF;
              ENDIF;
              READ RSINP105;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: ExtInterv
        // DescriÁ„o.: Extrair dados dos Interveninentes
        //==============================================================
        BEGSR ExtInterv;
           SETLL (WAaProc:WNuProc) RSINP114;
           READ RSINP114;
           DOW (NOT %EOF());
              IF (AAPROC <> WAaProc) OR (NUPROC <> WNuProc);
                 LEAVE;
              ENDIF;

              //---------------------------------
              // Obter o CÛdigo do Interveniente
              //---------------------------------
              WIcExiste = *OFF;
              IF (NUMINT > *ZEROS);
                 WIdxAct = 1;
                 DOW (WIdxAct < WIdxList);
                    IF (LstSegurado(WIdxAct).CodSeg = NUMINT);
                       WIcExiste = *ON;
                       LEAVE;
                    ENDIF;
                    WIdxAct += 1;
                 ENDDO;

                 IF (NOT WIcExiste);
                    EXSR AddInterv;
                 ENDIF;
              ENDIF;
              READ RSINP114;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: AddInterv
        // DescriÁ„o.: Adicionar os dados do Interveniente.
        //==============================================================
        BEGSR AddInterv;
           //----------------------------------------
           // Inserir o registo se este n„o existir
           //----------------------------------------
           CHAIN (NUMINT) FSEGF1;
           IF (%FOUND());
              WIdxList += 1;
              LstSegurado(WIdxList).CodSeg = NSEG;
              LNSEG  = (NSEG + 900000);
              LNOME  = %XLATE(StrPor:StrInt:NOM);
              LTITUL = '99';
              LTIPO  = '1';  // Pessoa
              LPROF  = '999999';
              LMORA  = %XLATE(StrPor:StrInt:MORAD);
              LLOCAL = %XLATE(StrPor:StrInt:DESCP);
              MONITOR;
                 LCPOST = %SUBST(%CHAR(CPOST):1:4);
                 LSPOST = %SUBST(%CHAR(CPOST):5:3);
              ON-ERROR;
                 LCPOST = '0000';
                 LSPOST = '000';
              ENDMON;
              LDTTRF = WDatTransf;
              LNIF   = NIF;
              LDTNAS  = DTNASC;
              EXCEPT ROPLSEG;
            ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: CrtOPLSeg
        // DescriÁ„o.: Construir o ficheiro Interface de Intervenientes
        //==============================================================
        BEGSR CrtOPLSeg;
           //--------------------------------------------------------
           // Cria Script de TransferÍncia
           //--------------------------------------------------------
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/OPLSEG)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           IF (NOT WIcErro);
              MONITOR;
                 WStdCmd = 'CRTPF FILE(QTEMP/OPLSEG) RCDLEN(211)';
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
           WNomAS400 = 'OPLSEG';
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
     O*==============================================================
     OOPLSEG    EADD         ROPLSEG
     O                       LNSEG                6
     O                       LNOME               56
     O                       LTITUL              58
     O                       LTIPO               59
     O                       LPROF               65
     O                       LMORA              105
     O                       LLOCAL             135
     O                       LCPOST             139
     O                       LSPOST             142
     O                       LDTTRF             194
     O                       LNIF               203
     O                       LDTNAS             211
     P*==================================================================
     P* Processo..: CvtDate
     P* Objectivo.: Convertar um campo de um formato data em
     P*             numerico de 8 posiÁıes
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
