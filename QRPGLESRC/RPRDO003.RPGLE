     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO003
     H* Descrição.: Disponibilizar Histórico de Alterações das Apólices
     H* Observação: Esta rotina lista todos os ficheiros de Histórico
     H*             onde consta a apólice, e devolve o nome do ficheiro
     H*             para que o utilizador possa consultar os dados.
     H*==================================================================
     FFPRDT001  UF A E           K DISK    PREFIX(IX_)
     FGBASEDIA  IF   E           K DISK    PREFIX(AU_)
     F                                     EXTFILE('GBASEDIA')
     FGBASEDIO  IF   E           K DISK    PREFIX(OR_)
     F                                     EXTFILE('GBASEDIO')
     FGFSEG     IF   E           K DISK    PREFIX(SG_)
     F*==========================================================
     D*==========================================================
     D
     C*========================================================
      /FREE
         //------------------------------------------------------
         // Marcar com o valor 'N' o indicador de Alterada/Nova
         //------------------------------------------------------
         // SETLL (*LOVAL) RPRDP001;
         // READ RPRDP001;
         // DOW (NOT %EOF());
         //    IDX_ICCARR = 'N';
         //    UPDATE RPRDP001 %FIELDS(IDX_ICCARR);
         //    READ RPRDP001;
         // ENDDO;

         //------------------------------------------------------
         // Marcar com o valor 'S' o indicador de Alterada/Nova
         //------------------------------------------------------
         SETLL (*LOVAL) BASEDIFA;
         READ BASEDIFA;
         DOW (NOT %EOF());
            CHAIN (AU_RAMO:AU_APOL) RPRDP001;
            IF (%FOUND());
               IX_ICCARR = 'S';
               UPDATE RPRDP001 %FIELDS(IX_ICCARR);
            ELSE;
               CHAIN (AU_NSEG) GFSEG;
               IF (NOT %FOUND());
      /END-FREE
     C                   MOVEL     *BLANKS       IX_APELIDO
     C                   MOVEL     *BLANKS       IX_NUNIF
      /FREE
               ENDIF;
      /END-FREE
     C                   Z-ADD     AU_RAMO       IX_CDRAMO
     C                   Z-ADD     AU_APOL       IX_NUAPOL
     C                   Z-ADD     AU_NSEG       IX_NUSEG
     C                   MOVEL     AU_SEG        IX_NOSEG
     C                   MOVEL     SG_APELID     IX_APELIDO
     C                   Z-ADD     SG_NIF        IX_NUNIF
     C                   MOVEL     AU_MATR1      IX_MATRIC
     C                   Z-ADD     AU_SIT        IX_STATUS
     C******             MOVEL     *BLANKS       IX_LCRISK
     C                   Z-ADD     AU_MED1       IX_CDMED1
     C                   Z-ADD     AU_JMM1       IX_CDJMM1
     C                   Z-ADD     AU_JMM2       IX_CDJMM2
     C                   MOVEL     'S'           IX_ICCARR
||   C                   EVAL      IX_PRD00101 = %DATE('2000-01-01':*ISO)
||   C                   EVAL      IX_PRD00102 = %DATE('9999-12-31':*ISO)
||   C                   WRITE     RPRDP001
      /FREE
            ENDIF;
            READ BASEDIFA;
         ENDDO;

         //------------------------------------------------------
         // Marcar com o valor 'S' o indicador de Alterada/Nova
         //------------------------------------------------------
         SETLL (*LOVAL) BASEDIFO;
         READ BASEDIFO;
         DOW (NOT %EOF());
            CHAIN (OR_RAMO:OR_APOL) RPRDP001;
            IF (%FOUND());
               IX_ICCARR = 'S';
               UPDATE RPRDP001 %FIELDS(IX_ICCARR);
            ELSE;
               // Adicionar o Registo
            ENDIF;
            READ BASEDIFO;
         ENDDO;

         RETURN;
      /END-FREE
