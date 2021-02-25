     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO010
     H* Descrição.: Carregar ficheiro de Penhoras.
     H*
     H* Para Compilar este programa fazer:
     H*    CRTPF FILE(QTEMP/FINTT021) SRCFILE(#JMPRDSRC/QDDSSRC)
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT021)
     H*
     H* Para correr este programa em DEBUG fazer:
     H*    CALL PGM(#JMPRDOBJ/PGRLO015)
     H*===========================================================
     FGFSEG3    IF   E           K DISK    PREFIX(SEG_)
     F
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
     FFINTT021  IF   E           K DISK    PREFIX(TMP_) USROPN
     F                                     EXTFILE('QTEMP/INTFILE')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D WNomFile        S             40A
     D WCdInter        S              9S 0
     D WAction         S              1A
     D WResposta       S              1A
     D WMsgErro        S              7A
     D
     D WNomPasta       S             40A
     D WCdFile         S              7S 0
     D WNuSequ         S              5S 0
     D WCdRegis        S              9S 0
     D WNomSeg         S             40A
     D WDatRecolha     S               D
     D WIsFirst        S               N
     D WIcAlterou      S               N
     D WIcExiste       S               N
     D
     D*==> Declaração de Constantes
     D TIP#NIFInval    C                   'LN'
     D ORG#Automatica  C                   'A'
     D ACT#Validar     C                   'V'
     D ACT#Processar   C                   'P'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNomFile
     C                   PARM                    WCdInter
     C                   PARM                    WAction
     C                   PARM                    WResposta
     C                   PARM                    WMsgErro
      /FREE
        //-----------------------------------------------------------------
        // Validar se o Ficheiro ja foi Processado
        //-----------------------------------------------------------------
        CHAIN (WNomFile) RINTP01001;
        IF (%FOUND());
           WMsgErro = 'INT0001'; // O ficheiro já foi processado
           WResposta = 'N'; // Não é para processar
           RETURN;
        ELSEIF (WAction = ACT#Validar);
           WResposta = 'S'; // É para processar
           WMsgErro = *BLANKS;
           RETURN;
        ENDIF;

        WIsFirst = *ON;
        EXSR ReadInterface;
        IF (TMP_DATFILE > *ZEROS);
           EXSR Desactivar;
        ENDIF;

        *INLR = *ON;

       //=================================================================
       // SubRotina..: ReadInterface
       // Objectivo..: Ler o ficheiro de Interface recebido
       //=================================================================
       BEGSR ReadInterface;
          OPEN FINTT021;
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
             WIcExiste  = *OFF;
             WIcAlterou = *OFF;
             CHAIN (TMP_NUMNIF) RINTP01301;
             IF (NOT %FOUND());
                //--------------------------------------
                // Actualizar Registo de Controlo
                //--------------------------------------
                INT_CDFILE  = WCdFile;
                INT_NUSEQU  = WNuSequ;
                INT_TPREGIS = 02; // Lista Negra
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
                // Localizar o registo do Segurado
                //------------------------------------------
                WNomSeg = *BLANKS;
                CHAIN (TMP_NUMNIF) GFSEG3;
                IF (%FOUND(GFSEG3));
                   WNomSeg = SEG_NOM;
                ENDIF;

                //------------------------------------------
                // Actualizar Ficheiro de NIF's inválidos
                //------------------------------------------
                SETLL (TMP_NUMNIF) RENTP01001;
                READE (TMP_NUMNIF) RENTP01001;
                DOW (NOT %EOF());
                   IF (ENT_TPREGIS = TIP#NIFInval);
                      IF (ENT_DTTERMO = 99999999);
                         IF (ENT_DTINICIO > TMP_DATFILE);
                            CHAIN (ENT_CDREGIS) RENTP010;
                            ENT_CDFILE   = WCdFile;
                            ENT_CDORIGEM = ORG#Automatica;
                            ENT_DTINICIO = TMP_DATFILE;
                            ENT_NOME     = WNomSeg;
                            ENT_ENT01003 = WDatRecolha;
                            ENT_ENT01005 = 'AUTO-CI';
                            ENT_ENT01007 = %DATE();
                            UPDATE RENTP010 %FIELDS(ENT_DTINICIO:ENT_ENT01003:
                                                    ENT_CDFILE:ENT_CDORIGEM:
                                                    ENT_NOME:ENT_ENT01005:
                                                    ENT_ENT01007);
                         ELSE;
                            CHAIN (ENT_CDREGIS) RENTP010;
                            ENT_NOME     = WNomSeg;
                            ENT_ENT01002 = %DATE('9999-12-31');
                            ENT_ENT01003 = WDatRecolha;
                            ENT_CDORIGEM = ORG#Automatica;
                            ENT_ENT01005 = 'AUTO-CI';
                            ENT_ENT01007 = %DATE();
                            UPDATE RENTP010 %FIELDS(ENT_ENT01003:ENT_CDORIGEM:
                                                    ENT_NOME:ENT_ENT01005:
                                                    ENT_ENT01007);
                         ENDIF;
                         WIcAlterou = *ON;
                         LEAVE;
                      ENDIF;
                   ENDIF;
                   READE (TMP_NUMNIF) RENTP01001;
                ENDDO;
                IF (NOT WIcAlterou);
                   ENT_CDREGIS  = WCdRegis;
                   ENT_NUNIF    = TMP_NUMNIF;
                   ENT_DTINICIO = TMP_DATFILE;
                   ENT_DTTERMO  = 99999999;
                   ENT_TPREGIS  = TIP#NIFInval;
                   ENT_CDFILE   = WCdFile;
                   ENT_NOME     = WNomSeg;
                   ENT_CDORIGEM = ORG#Automatica;
                   ENT_ENT01001 = WDatRecolha;
                   ENT_ENT01002 = %DATE('9999-12-31');
                   ENT_ENT01003 = WDatRecolha;
                   ENT_ENT01004 = 'AUTO-CI';
                   ENT_ENT01005 = 'AUTO-CI';
                   ENT_ENT01006 = %DATE();
                   ENT_ENT01007 = %DATE();
                   WRITE RENTP010;
                   WCdRegis += 1;
                   WIcAlterou = *ON;
                ENDIF;
             ELSE;
                WIcExiste = *ON;
             ENDIF;

             //--------------------------------------------------
             // Actualizar registo (Indicação de ainda existir)
             //--------------------------------------------------
             IF (NOT WIcAlterou);
                SETLL (TMP_NUMNIF) RENTP01001;
                READE (TMP_NUMNIF) RENTP01001;
                DOW (NOT %EOF());
                   IF (WIcExiste AND ENT_DTTERMO = 99999999) AND
                      (ENT_TPREGIS = TIP#NIFInval) AND
                      (ENT_CDORIGEM = ORG#Automatica);
                         CHAIN (ENT_CDREGIS) RENTP010;
                         CHAIN (TMP_NUMNIF) GFSEG3;
                         IF (%FOUND(GFSEG3));
                            ENT_NOME = SEG_NOM;
                         ENDIF;
                         ENT_ENT01003 = WDatRecolha;
                         ENT_CDORIGEM = ORG#Automatica;
                         ENT_ENT01005 = 'AUTO-CI';
                         ENT_ENT01007 = %DATE();
                         UPDATE RENTP010 %FIELDS(ENT_DTINICIO:ENT_ENT01003:
                                                 ENT_CDORIGEM:ENT_NOME:
                                                 ENT_ENT01005:ENT_ENT01007);
                      LEAVE;
                   ENDIF;
                   READE (TMP_NUMNIF) RENTP01001;
                ENDDO;
             ENDIF;
             READ RINTT021;
          ENDDO;
          CLOSE FINTT021;
       ENDSR;

       //=================================================================
       // SubRotina..: Desactivar
       // Objectivo..: Desactivar os registos que não encontrou
       //=================================================================
       BEGSR Desactivar;
          SETLL *LOVAL RENTP01001;
          READ RENTP01001;
          DOW (NOT %EOF());
             IF (ENT_TPREGIS = TIP#NIFInval) AND (ENT_DTTERMO = 99999999) AND
              (ENT_ENT01003 <> WDatRecolha) AND (ENT_CDORIGEM = ORG#Automatica);
               CHAIN (ENT_CDREGIS) RENTP010;
               ENT_DTTERMO  = TMP_DATFILE;
               ENT_ENT01003 = WDatRecolha;
               ENT_ENT01005 = 'AUTO-CI';
               ENT_ENT01007 = %DATE();
               UPDATE RENTP010 %FIELDS(ENT_DTTERMO:ENT_ENT01003:
                                       ENT_ENT01005:ENT_ENT01007);
             ENDIF;
             READ RENTP01001;
          ENDDO;
       ENDSR;

       //=================================================================
       // SubRotina..: RegisFile
       // Objectivo..: Registar o Ficheiro
       //=================================================================
       BEGSR RegisFile;
          WDatRecolha = %DATE(TMP_DATFILE);
          WNuSequ     = 1;
          WCdRegis    = $GetNextKey('FENTP010':'CDREGIS');
          WCdFile     = $GetNextKey('FINTP010':'CDFILE');
          INT_CDFILE  = WCdFile;
          INT_NOMFILE = WNomFile;
          INT_DTFILE  = WDatRecolha;        // ForCompr com a data do ficheiro
          INT_DTPROC  = WDatRecolha;
          WRITE RINTP010;
       ENDSR;
      /END-FREE
