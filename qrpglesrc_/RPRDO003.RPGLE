     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO003
     H* Descrição.: Disponibilizar Histórico de Alterações das Apólices
     H* Observação: Esta rotina lista todos os ficheiros de Histórico
     H*             onde consta a apólice, e devolve o nome do ficheiro
     H*             para que o utilizador possa consultar os dados.
     H*==================================================================
     FFPRDT001  UF A E           K DISK    PREFIX(IDX_)
     FGBASEDIA  IF   E           K DISK    PREFIX(AUT_)
     F                                     EXTFILE('LGALHOZ/GBASEDIA')
     FGBASEDIO  IF   E           K DISK    PREFIX(OR_)
     F                                     EXTFILE('LGALHOZ/GBASEDIO')
     F*==========================================================
     D*==========================================================
     D
     C*========================================================
      /FREE
         //------------------------------------------------------
         // Marcar com o valor 'N' o indicador de Alterada/Nova
         //------------------------------------------------------
         SETLL (*LOVAL) RPRDP001;
         READ RPRDP001;
         DOW (NOT %EOF());
            IDX_ICCARR = 'N';
            UPDATE RPRDP001 %FIELDS(IDX_ICCARR);
            READ RPRDP001;
         ENDDO;

         //------------------------------------------------------
         // Marcar com o valor 'S' o indicador de Alterada/Nova
         //------------------------------------------------------
         SETLL (*LOVAL) BASEDIFA;
         READ BASEDIFA;
         DOW (NOT %EOF());
            CHAIN (AUT_RAMO:AUT_APOL) RPRDP001;
            IF (%FOUND());
               IDX_ICCARR = 'S';
               UPDATE RPRDP001 %FIELDS(IDX_ICCARR);
            ELSE;
               // Adicionar o Registo
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
               IDX_ICCARR = 'S';
               UPDATE RPRDP001 %FIELDS(IDX_ICCARR);
            ELSE;
               // Adicionar o Registo
            ENDIF;
            READ BASEDIFO;
         ENDDO;

         *INLR = *ON;
         RETURN;
      /END-FREE
