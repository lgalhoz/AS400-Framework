     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *=======================================================
      * Autor......: Luis Galhoz
      * Programa...: PPRDT001
      * Objectivo..: Actualizar ficheiro físico com as apólices
      *=======================================================
     FFPRDT001  UF A E           K DISK    PREFIX(IDX_)
     FGBASEDIA  IF   E           K DISK    EXTFILE('GBASEDIA')
     F                                     PREFIX(GRA_)
     FGBASEDIO  IF   E           K DISK    EXTFILE('GBASEDIO')
     F                                     PREFIX(GRO_)
     FGBASEGRA  IF   E           K DISK    EXTFILE('GBASEGRA')
     F                                     PREFIX(GRA_)
     FGBASEGRO  IF   E           K DISK    EXTFILE('GBASEGRO')
     F                                     PREFIX(GRO_)
     FGFSEG     IF   E           K DISK    EXTFILE('GFSEG')
     F                                     PREFIX(SEG_)
     F*==========================================================
     D WIcCriar        S               N
     D WIcCarr         S                   LIKE(IDX_ICCARR)
     D WCdMED1         S                   LIKE(IDX_CDMED1)
     D WCdJMM1         S                   LIKE(IDX_CDJMM1)
     D WCdJMM2         S                   LIKE(IDX_CDJMM2)
     D WStatus         S                   LIKE(IDX_STATUS)
     D WMatric         S                   LIKE(IDX_MATRIC)
     D WLcRisk         S                   LIKE(IDX_LCRISK)
     C*========================================================
      /FREE
         //------------------------------------------------------
         // Actualizar o adicionar novos Registos
         //------------------------------------------------------
         SETLL (*LOVAL) GBASEGRA;
         READ GBASEGRA;
         DOW (NOT %EOF());
            EXSR UpdAutomovel;
            WIcCarr = 'N';
            READ GBASEGRA;
         ENDDO;
         SETLL (*LOVAL) GBASEDIA;
         READ GBASEDIA;
         DOW (NOT %EOF());
            EXSR UpdAutomovel;
            WIcCarr = 'S';
            READ GBASEDIA;
         ENDDO;

         //------------------------------------------------------
         // Marcar com o valor 'S' o indicador de Alterada/Nova
         // ou então adicionar um novo registo
         //------------------------------------------------------
         SETLL (*LOVAL) GBASEGRO;
         READ GBASEGRO;
         DOW (NOT %EOF());
            WIcCarr = 'N';
            EXSR UpdOutros;
            READ GBASEGRO;
         ENDDO;
         SETLL (*LOVAL) GBASEDIO;
         READ GBASEDIO;
         DOW (NOT %EOF());
            WIcCarr = 'S';
            EXSR UpdOutros;
            READ GBASEDIO;
         ENDDO;

         *INLR = *ON;
         RETURN;

         //=======================================================================
         // Subrotina...: UpdAutomovel
         // Objectivo...: Actualizar o Registo ou Adicionar se não Existir
         //=======================================================================
         BEGSR UpdAutomovel;
            WCdMED1 = GRA_MED1;
            WStatus = GRA_SIT;
            WLcRisk = *BLANKS;
            WMatric = GRA_MATR1;
            WCdJMM1 = GRA_JMM1;
            WCdJMM2 = GRA_JMM2;
            CHAIN (GRA_RAMO:GRA_APOL) RPRDP001;
            IF (%FOUND());
               IDX_MATRIC  = WMatric;
               IDX_ICCARR = WIcCarr;
               IDX_CDMED1  = WCdMed1;
               IDX_CDJMM1  = WCdJMM1;
               IDX_CDJMM2  = WCdJMM2;
               IDX_STATUS  = WStatus;
               IDX_LCRISK  = WLcRisk;
               UPDATE RPRDP001 %FIELDS(IDX_STATUS:IDX_ICCARR:IDX_CDJMM1
                                      :IDX_CDJMM2
                                      :IDX_CDMED1:IDX_LCRISK:IDX_MATRIC);
            ELSE;
               IDX_CDRAMO  = GRA_RAMO;
               IDX_NUAPOL  = GRA_APOL;
               IDX_NUSEG   = GRA_NSEG;
               IDX_NOSEG   = GRA_SEG;
               IDX_STATUS  = WStatus;
               IDX_MATRIC  = WMatric;
               IDX_LCRISK  = WLcRisk;
               IDX_ICCARR  = WIcCarr;
               IDX_CDMED1  = WCdMed1;
               IDX_CDJMM1  = WCdJMM1;
               IDX_CDJMM2  = WCdJMM2;
               IDX_APELIDO = *BLANKS;
               IDX_NUNIF   = *ZEROS;
               CHAIN (IDX_NUSEG) GFSEG;
               IF (%FOUND());
                  IDX_APELIDO = SEG_APELID;
                  IDX_NUNIF   = SEG_NIF;
               ENDIF;
               WRITE  RPRDP001;
            ENDIF;
         ENDSR;

         //=======================================================================
         // Subrotina...: UpdOutros
         // Objectivo...: Actualizar o Registo ou Adicionar se não Existir
         //=======================================================================
         BEGSR UpdOutros;
            WCdMed1 = GRO_MED1;
            WStatus = GRO_SIT;
            WLcRisk = GRO_LRPS;
            WMatric = *BLANKS;
            WCdJMM1 = GRO_JMM1;
            WCdJMM2 = GRO_JMM2;
            CHAIN (GRO_RAMO:GRO_APOL) RPRDP001;
            IF (%FOUND());
               IDX_MATRIC  = WMatric;
               IDX_ICCARR  = WIcCarr;
               IDX_CDMED1  = WCdMed1;
               IDX_CDJMM1  = WCdJMM1;
               IDX_CDJMM2  = WCdJMM2;
               IDX_STATUS  = WStatus;
               IDX_LCRISK  = WLcRisk;
               UPDATE RPRDP001 %FIELDS(IDX_STATUS:IDX_ICCARR:IDX_CDJMM1
                                      :IDX_CDJMM2
                                      :IDX_CDMED1:IDX_LCRISK:IDX_MATRIC);
            ELSE;
               IDX_CDRAMO  = GRO_RAMO;
               IDX_NUAPOL  = GRO_APOL;
               IDX_NUSEG   = GRO_NSEG;
               IDX_NOSEG   = GRO_SEG;
               IDX_STATUS  = WStatus;
               IDX_MATRIC  = WMatric;
               IDX_LCRISK  = WLcRisk;
               IDX_ICCARR  = WIcCarr;
               IDX_CDMED1  = WCdMed1;
               IDX_CDJMM1  = WCdJMM1;
               IDX_CDJMM2  = WCdJMM2;
               IDX_APELIDO = *BLANKS;
               IDX_NUNIF   = *ZEROS;
               CHAIN (IDX_NUSEG) GFSEG;
               IF (%FOUND());
                  IDX_APELIDO = SEG_APELID;
                  IDX_NUNIF   = SEG_NIF;
               ENDIF;
               WRITE  RPRDP001;
            ENDIF;
         ENDSR;
      /END-FREE
