     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *=======================================================
      * Autor......: Luis Galhoz
      * Programa...: PPRDT002
      * Objectivo..: Actualizar indice Apólices Viagem
      *=======================================================
     FFPRDT001  UF A E           K DISK    PREFIX(IDX_)
     FGCOBGR    IF   E           K DISK    PREFIX(REC_)
     FPESSVG    IF   E           K DISK    PREFIX(PES_)
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
        SETLL (*LOVAL) GCOBGR;
        READ GCOBGR;
        DOW (NOT %EOF());
           IF (REC_GCRAMO = 10);
              EXSR UpdViagem;
           ENDIF;
           READ GCOBGR;
        ENDDO;

         *INLR = *ON;
         RETURN;

         //=======================================================================
         // Subrotina...: UpdViagem
         // Objectivo...: Actualziar ficheiro de Indice com Apólices Viagem
         //=======================================================================
         BEGSR UpdViagem;
            CHAIN (REC_GCRAMO:REC_GCAPOL) FPRDT001;
            IF (NOT %FOUND());
               IDX_CDRAMO   = REC_GCRAMO;
               IDX_NUAPOL   = REC_GCAPOL;
               IDX_NUSEG    = REC_GCNSEG;
               EXSR GetSegurado;
               IDX_MATRIC   = *BLANKS;
               IDX_STATUS   = 1;
               IDX_LCRISK   = 'AUTOM';
               IDX_CDMED1   = REC_GCMED1;
               IDX_CDJMM1   = REC_GCJMM1;
               IDX_CDJMM2   = REC_GCJMM2;
               IDX_ICCARR   = 'N';
               IDX_TPACTA   = 'X';
               IDX_CDPROD   = REC_GCRAMO;
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
            CHAIN (REC_GCNSEG) GFSEG;
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
