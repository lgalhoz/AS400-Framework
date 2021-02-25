     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO010
     H* Descrição.: Carregar ficheiro de Penhoras.
     H*===========================================================
     FFINTP010  UF A E           K DISK    PREFIX(INT_)
     FFINTP01001IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP010:RINTP01001)
     FFINTP011  UF A E           K DISK    PREFIX(INT_)
     FFINTP013  UF A E           K DISK    PREFIX(INT_)
     FFINTP01301IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP013:RINTP01301)
     FFENTP010  UF A E           K DISK    PREFIX(ENT_)
     FFENTP01001IF   E           K DISK    PREFIX(ENT_)
     F                                     RENAME(RENTP010:RENTP01001)
     FFINTT021  IF   E           K DISK    PREFIX(TMP_)
     F                                     EXTFILE('QTEMP/INTFILE')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D WNomFile        S             40A
     D WNomPasta       S             40A
     D WMsgErro        S              7A
     D WCdFile         S              7S 0
     D WNuSequ         S              5S 0
     D WCdRegis        S              9S 0
     D WIsFirst        S               N
     D WIcAlterou      S               N
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNomFile
     C                   PARM                    WNomPasta
     C                   PARM                    WMsgErro
      /FREE
        //-----------------------------------------------------------------
        // Validar se o Ficheiro ja foi Processado
        //-----------------------------------------------------------------
        CHAIN (WNomFile) RINTP01001;
        IF (%FOUND(FINTP010));
           RETURN;
        ENDIF;
        WIsFirst = *ON;
        EXSR ReadInterface;
        EXSR Desactivar;

        *INLR = *ON;

       //=================================================================
       // SubRotina..: ReadInterface
       // Objectivo..: Ler o ficheiro de Interface recebido
       //=================================================================
       BEGSR ReadInterface;
          SETLL *LOVAL RINTT021;
          READ FINTT021;
          DOW (NOT %EOF(FINTT021));
             //-----------------------------------
             // Registar Cabeçalho do Registo
             //-----------------------------------
             IF (WIsFirst);
                WIsFirst = *OFF;
                EXSR RegisFile;
             ENDIF;

             //-----------------------------------------
             // Validar se o registo já está processado
             //-----------------------------------------
             CHAIN (TMP_NUMNIF) RINTP01301;
             IF (NOT %FOUND());
                //--------------------------------------
                // Actualizar Registo de Controlo
                //--------------------------------------
                INT_CDFILE  = WCdFile;
                INT_NUSEQU  = WNuSequ;
                INT_TPREGIS = 02; // NIF's Inválidos
                WRITE RINTP011;

                //--------------------------------------
                // Actualizar Registo de Detalhe
                //--------------------------------------
                INT_CDFILE  = WCdFile;
                INT_NUSEQU  = WNuSequ;
                INT_DATFILE = TMP_DATFILE;
                INT_NUMNIF  = TMP_NUMNIF;
                WRITE RINTP013;
                WNuSequ    += 1;

                //------------------------------------------
                // Actualizar Ficheiro de Penhoras e NIF's
                //------------------------------------------
                WIcAlterou = *OFF;
                SETLL (TMP_NUMNIF) RENTP01001;
                READE (TMP_NUMNIF) RENTP01001;
                DOW (NOT %EOF());
                   IF (ENT_DTTERMO <> 99999999);
                      IF (ENT_DTINICIO > TMP_DATFILE);
                         CHAIN (ENT_CDREGIS) RENTP010;
                         ENT_DTINICIO = TMP_DATFILE;
                         ENT_CDFILE   = WCdFile;
                         ENT_ENT01002 = %DATE();
                         UPDATE RENTP010 %FIELDS(ENT_DTINICIO:ENT_ENT01002:
                                                 ENT_CDFILE);
                         WIcAlterou = *ON;
                         LEAVE;
                      ENDIF;
                   ENDIF;
                   READE (TMP_NUMNIF) RENTP01001;
                ENDDO;
                IF (NOT WIcAlterou);
                   ENT_CDREGIS  = WCdRegis;
                   ENT_NUNIF    = TMP_NUMNIF;
                   ENT_NOME     = *BLANKS;
                   ENT_DTINICIO = TMP_DATFILE;
                   ENT_DTTERMO  = 99999999;
                   ENT_TPREGIS  = 'NI'; // NIF's Inválidos
                   ENT_CDFILE   = WCdFile;
                   ENT_ENT01001 = %DATE();
                   ENT_ENT01002 = %DATE();
                   WRITE RENTP010;
                   WCdRegis += 1;
                   WIcAlterou = *ON;
                ENDIF;
             ENDIF;

             //--------------------------------------------------
             // Actualizar registo (Indicação de ainda existir)
             //--------------------------------------------------
             IF (NOT WIcAlterou);
                CHAIN (TMP_NUMNIF:TMP_DATFILE) RENTP01001;
                IF (%FOUND());
                   CHAIN (ENT_CDREGIS) RENTP010;
                   ENT_ENT01002 = %DATE();
                   UPDATE RENTP010 %FIELDS(ENT_DTINICIO:ENT_ENT01002);
                ENDIF;
             ENDIF;
             READ RINTT021;
          ENDDO;
       ENDSR;

       //=================================================================
       // SubRotina..: Desactivar
       // Objectivo..: Desactivar os registos que não encontrou
       //=================================================================
       BEGSR Desactivar;
          SETLL *LOVAL RENTP01001;
          READ RENTP01001;
          DOW (NOT %EOF());
             IF (ENT_TPREGIS = 'NI') AND (ENT_DTTERMO = 99999999) AND
               (ENT_ENT01002 <> %DATE());
               CHAIN (ENT_CDREGIS) RENTP010;
               ENT_DTTERMO  = TMP_DATFILE;
               ENT_ENT01002 = %DATE();
               UPDATE RENTP010 %FIELDS(ENT_DTTERMO:ENT_ENT01002);
             ENDIF;
             READ RENTP01001;
          ENDDO;
       ENDSR;

       //=================================================================
       // SubRotina..: RegisFile
       // Objectivo..: Registar o Ficheiro
       //=================================================================
       BEGSR RegisFile;
          WNuSequ     = 1;
          WCdRegis    = $GetNextKey('FENTP010':'CDREGIS');
          WCdFile     = $GetNextKey('FINTP010':'CDFILE');
          INT_CDFILE  = WCdFile;
          INT_NOMFILE = WNomFile;
          INT_DTFILE  = %DATE(); // ForCompr com a data do ficheiro
          INT_DTPROC  = %DATE();
          WRITE RINTP010;
          RETURN;
       ENDSR;
      /END-FREE
