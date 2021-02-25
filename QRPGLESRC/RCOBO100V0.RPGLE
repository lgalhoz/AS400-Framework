     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RCOBO100
     H* Descrição.: Gerar Ficheiro de Cabeçalho e Actualizar IDD's
     H*==================================================================
     FTBIDDEF   UF A E           K DISK
     FTBIDDPF   UF   E           K DISK
     FTBIDDPL1  IF   E           K DISK    RENAME(TBIDDP:TBIDDPL)
     FFCOBP220  IF   E           K DISK
     FFCOBP221  IF   E           K DISK
     F*=========================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Chamada Dinâmica a funções
     D CopyFile        PR                  EXTPGM('RCOBO110')
     D   FileCode                    10P 0 Const Options(*NOPASS)
     D   MsgError                     7A   Const Options(*NOPASS)
     D
     D*==> Chamada Dinâmica a funções
     D CallDynamic     PR                  EXTPGM(WDynamic)
     D   CodFileAct                  10P 0 Const
     D   NumNIB                      21S 0 Const
     D   MnteRecibo                  13S 2 Const
     D   CodRecibo                    9S 0 Const
     D   CodADC                      11S 0 Const
     D   DatADC                       8S 0 Const
     D   CodSegura                    8S 0 Const
     D   NuseqSaved                   5S 0 Const
     D   MsgErro                      7A   Const
     D   CodFileAnt                  10P 0 Const Options(*NOPASS)
     D   QtdeRegist                   6S 0 Const Options(*NOPASS)
     D   MnteRegist                  13S 2 Const Options(*NOPASS)
     D WDynamic        S             10A
     D
     D*==> Estrutura do Nome do Ficheiro
     D                 DS
     D  WCodFile               1     10S 0
     D  WDatfile               1      8S 0
     D  WSeqFile               9     10S 0
     D
     D*==> Campos de Trabalho
     D  WNuseqSaved    S              5S 0
     D  WMsgErro       S              7A
     D  WTipInterface  S              3A
     D  WDatSelect     S              8S 0
     D  WDatActual     S              8S 0
     D  WIcSelect      S               N
     D  WSavSequ       S              2S 0
     D  WSavCodFile    S             10S 0
     D  WQtdeRegis     S              4S 0
     D  WMnteRegis     S             13S 2
     D
     D*==> Definição de constantes
     D KLimMax         C                   1
     D*=========================================================================
      /FREE
         WTipInterface = 'XML';
         IF (WTipInterface = 'TXT');
            WDynamic = 'RCOBO101';
         ELSE;
            WDynamic = 'RCOBO102';
         ENDIF;

         WQtdeRegis    = *ZEROS;
         WMnteRegis    = *ZEROS;
         WDatActual    = $DateToInt(%DATE());
         WIcSelect     = *OFF;
         WDatSelect    = *ZEROS;
         SETLL (*HIVAL) TBIDDP;
         READ TBIDDP;
         DOW (NOT %EOF(TBIDDPF));
            IF (IDSIT = '2');
               IF (NOT WIcSelect) AND (WDatActual >= IDDATA);
                  WDatSelect = IDDATA;
                  WIcSelect = *ON;
                  EXSR GetCodFile;
               ENDIF;
               IF (IDDATA = WDatSelect);
                  CHAIN (IDNADC) RCOBP220;
                  IF (%FOUND());
                     CHAIN (IDNADC:CDNIB) RCOBP221;
                     IF (%FOUND());
                        IDFENV = WCodFile;
                        IDSIT  = '3';
                        WQtdeRegis   += 1;
                        WMnteRegis   += IDVAL;
                        CallDynamic(WCodFile:IDNIB:IDVAL:IDREC:IDNADC:DATALT
                                    :IDNSEG:WNuseqSaved:WMsgErro:WSavCodFile);
                        IF (WMsgErro = *BLANKS);
                           UPDATE TBIDDP %FIELDS(IDFENV:IDSIT);
                        ENDIF;

                        //--------------------------------------
                        // Limitar os registos por XML
                        //--------------------------------------
                        IF (WQtdeRegis >= KLimMax);
                           LEAVE;
                        ENDIF;
                     ENDIF;
                  ENDIF;
               ENDIF;
            ENDIF;
            READ TBIDDP;
         ENDDO;

         //--------------------------------------------
         // Se encontrou registo entrão gravar resumo
         //--------------------------------------------
         IF (WIcSelect);
            CallDynamic(WCodFile:*ZEROS:*ZEROS:*ZEROS:*ZEROS:*ZEROS:
                        *ZEROS:WNuseqSaved:WMsgErro:WSavCodFile:
                        WQtdeRegis:WMnteRegis);
            CHAIN (WCodFile) TBIDDEF;
            IF (%FOUND());
                TETREG = WQtdeRegis; // Quantidade Enviado
                UPDATE TBIDDE %FIELDS(TETREG);
                EXSR SendToShare;
            ENDIF;
         ENDIF;

         *INLR = *ON;
         RETURN;

       //=============================================================
       // Subrotina.: GetCodFile
       // Objectivo.: Obter o Código para o ficheiro
       //=============================================================
       BEGSR GetCodFile;
          //--------------------------------------------
          // Localizar o último registo
          //--------------------------------------------
          WDatFile = WDatSelect;
          WSeqFile = 1;
          CHAIN (WCodFile) TBIDDE;
          DOW (%FOUND(TBIDDEF));
             WSeqFile += 1;
             CHAIN (WCodFile) TBIDDE;
          ENDDO;
          WDatFile = WDatSelect;

          //----------------------------------------------
          // Actualizar informação do Ficheiro
          //----------------------------------------------
          TEFENV = WCodFile;   // Código do Ficheiro
          TEDATE = $cvtData(WDatSelect:'YYMD':'DMYY'); // Data do registo
          TEDATR = *ZEROS;     // Data transferência
          TEDATP = *ZEROS;     // Data do Pagamento
          TETREG = WQtdeRegis; // Quantidade Enviado
          TEREGA = *ZEROS;     // Quantidade Aceite
          TEVALR = *ZEROS;     // Montante Enviado
          TEVALP = *ZEROS;     // Montante Pago
          TETIPF = 'XML';      // Formato do Ficheiro
          WRITE TBIDDE;
       ENDSR;

       //=============================================================
       // Subrotina.: SendToShare
       // Objectivo.: Colocar o ficheiro no Share
       //=============================================================
       BEGSR SendToShare;
          CopyFile(WCodFile:WMsgErro);
          IF (WMsgErro <> *BLANKS);
             $ShowDialog(WMsgErro);
          ENDIF;
       ENDSR;
      /END-FREE
