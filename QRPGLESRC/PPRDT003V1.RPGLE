     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *=======================================================
      * Autor......: Luis Galhoz
      * Programa...: PPRDT003
      * Objectivo..: Actualizar indice Apólices Transporte
      *=======================================================
     FFPRDT001  UF A E           K DISK    PREFIX(IDX_)
     FGBASEGRT  IF   E           K DISK    PREFIX(REC_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D WCodSessao      S              5S 0 INZ(*ZEROS)
     D
     C*========================================================
      /FREE
         //------------------------------------------------------
         // Actualizar o adicionar novos Registos
         //------------------------------------------------------
         SETLL (*LOVAL) GBASEGRT;
         READ GBASEGRT;
         DOW (NOT %EOF());
            IF (REC_RAMO >= 68 AND REC_RAMO <= 70);
              EXSR UpdTransporte;
            ENDIF;
            READ GBASEGRT;
         ENDDO;

         *INLR = *ON;
         RETURN;

         //=======================================================================
         // Subrotina...: UpdTransporte
         // Objectivo...: Actualziar ficheiro de Indice
         //=======================================================================
         BEGSR UpdTransporte;
            CHAIN (REC_RAMO:REC_APOL) FPRDT001;
            IF (NOT %FOUND());
               IDX_CDRAMO   = REC_RAMO;
               IDX_NUAPOL   = REC_APOL;
               IDX_NUSEG    = REC_NSEG;
               EXSR GetSegurado;
               IDX_MATRIC   = *BLANKS;
               IDX_STATUS   = 1;
               IDX_LCRISK   = 'AUTOM';
               IDX_CDMED1   = REC_MED1;
               IDX_CDJMM1   = REC_JMM1;
               IDX_CDJMM2   = REC_JMM2;
               IDX_ICCARR   = 'N';
               IDX_TPACTA   = 'X';
               IDX_CDPROD   = REC_RAMO;
               IDX_PRD00101 = $IntToDate(20000101);
               IDX_PRD00102 = $IntToDate(99991231);
               WRITE RPRDP001;
            ENDIF;
         ENDSR;

         //=======================================================================
         // Subrotina...: GetSegurado
         // Objectivo...: Obter o registo do Segurado
         //=======================================================================
         BEGSR GetSegurado;
            CHAIN (REC_NSEG) GFSEG;
            IF (%FOUND());
               IDX_NOSEG   = SEG_NOM;
               IDX_APELIDO = SEG_APELID;
               IDX_NUNIF   = SEG_NIF;
            ELSE;
               IDX_NOSEG   = *BLANKS;
               IDX_APELIDO = *BLANKS;
               IDX_NUNIF   = *ZEROS;
            ENDIF;
         ENDSR;
      /END-FREE
