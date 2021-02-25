     H*====================================================================
     H* Programa...: PSINT001
     H* Objectivo..: Actualizar Inventário de Todos os Processos de Sinistro
     H*              
     H*
     H*====================================================================
     H  ALWNULL(*USRCTL) DFTACTGRP(*NO)
     H*====================================================================
     FGSINPRO   IP   E           K DISK    EXTFILE('G.SINPRO') PREFIX(PRO_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FFSINP001  UF A E           K DISK    PREFIX(SIN_)
     F*====================================================================
     D WAAProc         S                   LIKE(SIN_AAPROC)
     D WNUProc         S                   LIKE(SIN_NUPROC)
     D
     D*====================================================================
      /FREE
       SIN_STPROC = PRO_SPROC;        // situação do Processo
       SIN_TPPROC = PRO_IPROC;        // Tipo de Processo
       SIN_AAPROC = 2000+PRO_AAPROC;  // Ano do Processo
       SIN_NUPROC = PRO_NSEQ;         // Sequencia
       WAAProc = SIN_AAPROC;
       WNUProc = SIN_NUPROC;

       MONITOR;
          SIN_AARELA = %DEC(%SUBST(PRO_NPARTI:1:2):2:0) + 2000;
          SIN_NURELA = %DEC(%SUBST(PRO_NPARTI:3:4):4:0);
       ON-ERROR;
          SIN_AARELA = *ZEROS;
          SIN_NURELA = *ZEROS;
       ENDMON;

       MONITOR;
          SIN_DTINIC = %DATE(PRO_DTINIC:*ymd);
       ON-ERROR;
          SIN_DTINIC = %DATE(00010101:*ISO);
       ENDMON;

       MONITOR;
          SIN_DTREAB = %DATE(PRO_DTRINI:*ymd);
       ON-ERROR;
          SIN_DTREAB = %DATE(00010101:*ISO);
       ENDMON;

       MONITOR;
          SIN_DTTERM  = %DATE(PRO_DTFIM:*ymd);
       ON-ERROR;
          SIN_DTTERM = %DATE(00010101:*ISO);
       ENDMON;

       SIN_CDRAMO = PRO_RM;
       SIN_NUAPOL = PRO_AP;
       SIN_NUSEG  = PRO_NSEG;
       SIN_MATRIC = PRO_MATR1;
       CHAIN (PRO_NSEG) GFSEG;
       IF (%FOUND());
          SIN_NOSEG  = SEG_NOM;
          SIN_NUNIF  = SEG_NIF;
          SIN_APELID = SEG_APELID;
       ELSE;
          SIN_NOSEG  = *BLANKS;
          SIN_NUNIF  = *ZEROS;
          SIN_APELID = *BLANKS;
       ENDIF;
       CHAIN (SIN_AAPROC:SIN_NUPROC) RSINP001;
       IF (NOT %FOUND());
          WRITE RSINP001;
       ELSE;
          UPDATE RSINP001 %FIELDS(SIN_AARELA:SIN_NURELA:
                        SIN_DTINIC:SIN_DTREAB:SIN_DTTERM:
                        SIN_STPROC:SIN_NUNIF);
       ENDIF;
       EXSR ActRelacao;


       //==============================================================
       // Subrotina..: ActRelacao
       // Objectivo..: Actualizar a Relação Cruzada entre Processos e
       //              particiapções de Sinistro.
       //==============================================================
       BEGSR ActRelacao;
          IF (SIN_NURELA <> *ZEROS);
             CHAIN (SIN_AARELA:SIN_NURELA) RSINP001;
             IF (%FOUND());
                 SIN_AARELA = WAAProc;
                 SIN_NURELA = WNUProc;
                 UPDATE RSINP001 %FIELDS(SIN_AARELA:SIN_NURELA);
             ELSE;
                 SIN_AARELA = *ZEROS;
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
