     H OPTION(*SrcStmt : *NoDebugIO) BNDDIR('QC2LE':'JMDIR')
     H DFTACTGRP(*NO)
     H*==========================================================
     H* Programa..: PCOBT010
     H* Descrição.: Este programa é reponsável pela sincronização
     H* dos dados entre a antiga e actual estrutura da ADC.
     H*==========================================================
     FTBADCPF   IF   E           K DISK    PREFIX(OLD_)
     FTBADCAF   IF   E           K DISK    PREFIX(OLD_)
     FFCOBP220  UF A E           K DISK    PREFIX(NEW_)
     FFCOBP221  UF A E           K DISK    PREFIX(NEW_)
     FFCOBP222  UF A E           K DISK    PREFIX(NEW_)
     F*==========================================================
      /COPY QRPGLESRC,SGRLO016
      /COPY QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalhoexto
     D WCodADC         S              9S 0
     D WNumADC         S             11S 0
     D WCodNIB         S             21S 0
     D WNumNIB         S              3S 0
     D WNumOpera       S              3S 0
     D*==========================================================
      /FREE
        //---------------------------------------------------
        // Obter o último código de ADC
        //---------------------------------------------------
        WCodADC = $GetNextKey('FCOBP220':'CDNADC');

        //---------------------------------------------------
        // Actualizar a nova Estrutrua de Dados
        //---------------------------------------------------
        SETLL *HIVAL TBADCP;
        READ TBADCP;
        DOW (NOT %EOF(TBADCPF));
           WNumADC = OLD_TPNADC;
           CHAIN (WNumADC) RCOBP220;
           IF (NOT %FOUND(FCOBP220));
              EXSR AddNewADC;
              WCodADC += 1;
           ELSE;
           // EXSR ActAccoes;
           ENDIF;
           READ TBADCP;
        ENDDO;
        *INLR = *ON;

        //=========================================================
        // Subrotina.: AddNewADC
        // Objectivo.: Adicionar o registo de um nova ADC
        //=========================================================
        BEGSR AddNewADC;
           //-------------------------------------------------
           // Actualizar informação da ADC
           //-------------------------------------------------
           WNumNIB     = *ZEROS;
           WCodNIB     = *ZEROS;
           NEW_CDNADC  = WCodADC;
           NEW_NUNADC  = OLD_TPNADC;
           NEW_CDRAMO  = OLD_TPRAMO;
           NEW_NUAPOL  = OLD_TPAPOL;
           NEW_SITADC  = OLD_TPSIT;
           NEW_CDUSER  = OLD_TPUSER;
           NEW_DTAPROV = OLD_TPDATA;
           NEW_CDNIB   = 1;
           WRITE RCOBP220;

           //-------------------------------------------------
           // Actualizar informação do NIB e Operação
           //-------------------------------------------------
           // EXSR AddAccoes;
           SETLL (WNumADC) TBADCA;
           READE (WNumADC) TBADCA;
           DOW (NOT %EOF(TBADCAF));
              IF (WCodNIB <> OLD_TANIB);
                 WNumOpera   = *ZEROS;
                 WNumNIB   += 1;
                 NEW_CDNADC  = WCodADC;
                 WCodNIB     = OLD_TANIB;
                 NEW_NUNADC  = WNumADC;
                 NEW_CDNIB   = WNumNIB;
                 NEW_DATALT  = OLD_TADATA;
                 NEW_NUNIB   = WCodNIB;
                 NEW_CDMOV   = OLD_TATMOV;
                 NEW_CDSIT   = OLD_TASIT;
                 NEW_CDUSER  = OLD_TAUSER;
                 NEW_DTAPROV = OLD_TADTAP;
                 NEW_NUOPERA = 1;
                 WRITE RCOBP221;
              ENDIF;

              WNumOpera  += 1;
              NEW_CDNADC  = WCodADC;
              NEW_NUNADC  = WNumADC;
              NEW_CDNIB   = WNumNIB;
              NEW_NUOPERA = WNumOpera;
              NEW_DATALT  = OLD_TADATA;
              NEW_NOFILE  = OLD_TAFENV;
              NEW_CDRET   = OLD_TACRET;
              NEW_CDSIT   = OLD_TASIT;
              NEW_CDUSER  = OLD_TAUSER;
              NEW_DTAPROV = OLD_TADTAP;
              WRITE RCOBP222;

              IF (WNumOpera > 1);
                 CHAIN (WCodADC:WNumNIB) RCOBP221;
                 NEW_DATALT  = OLD_TADATA;
                 NEW_NUOPERA = WNumOpera;
                 NEW_CDMOV   = OLD_TATMOV;
                 NEW_CDSIT   = OLD_TASIT;
                 NEW_CDUSER  = OLD_TAUSER;
                 NEW_DTAPROV = OLD_TADTAP;
                 UPDATE RCOBP221 %FIELDS(NEW_NUOPERA:NEW_DATALT:
                            NEW_CDMOV:NEW_CDSIT:NEW_CDUSER:NEW_DTAPROV);
              ENDIF;
              READE (WNumADC) TBADCA;
           ENDDO;

           //-----------------------------------------------
           // Actualizar NIB se existirem mais que um
           //-----------------------------------------------
           IF (WNumNIB > 1);
              CHAIN (WCodADC) RCOBP220;
              NEW_CDNIB   = WNumNib;
              UPDATE RCOBP220 %FIELDS(NEW_CDNIB);
           ENDIF;
        ENDSR;
      /END-FREE
