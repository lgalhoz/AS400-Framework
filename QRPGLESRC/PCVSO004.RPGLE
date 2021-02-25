     H ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('JMDIR')
     H DECEDIT(',') DFTNAME(PCVSO001) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
     H*======================================================================================
     H* Autor .....: Luis Galhoz
     H* Data ......: 2013/10/30
     H* Programa ..: PCVSO004
     H* Objectivo .: Eliminar Registos da Nova Estrutrura
     H*
     H* Observação.: Este programa tem como objectivo, eliminar os registos criados na nova
     H*              Estrutura de Base de Dados.
     H*======================================================================================
     FFPRDT001  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP008  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Coberturas
     FFPRDP009  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Cláusulas e Condições
     FFPRDP010  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Notas Descritivas
     FFPRDP01003IF   E           K DISK    PREFIX(PRD_)                         Notas Descritivas
     F                                     RENAME(RPRDP010:RPRDP01003)
     FFPRDP011  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Riscos da Apólice
     FFPRDP012  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Texto das N.Descritivas
     FFPRDP016  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Características do Edificio
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO020
      /Copy QRPGLESRC,SDBSO010

     D*==> Parametros Recebidos
     D DelNotasDesc    PR              N
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D
     D*==> Parametros Recebidos
     D PCdRamo         S              2S 0
     D PNuApolice      S              7S 0
     D
     D*==> Campos de Trabalho
     D WCdRamo         S              2S 0
     D WNuApolice      S              7S 0
     D*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCdRamo
     C                   PARM                    PNuApolice
      /FREE
       //-----------------------------------------------------------------
       // Esta função só pode ser executada por um utilizador especial
       //-----------------------------------------------------------------
       IF (S#USR <> 'CALIMERO') OR ((S#USR = 'CALIMERO') AND
          ($ShowDialog('PRD0166':*BLANKS:'NS') = 'N'));
          *INLR = *ON;
          RETURN;
       ENDIF;

       //-----------------------------------------------------------------
       // Formatar parametros recebidos
       //-----------------------------------------------------------------
       MONITOR;
         WCdRamo = PCdRamo;
       ON-ERROR;
         WCdRamo = *ZEROS;
       ENDMON;
       MONITOR;
         WNuApolice = PNuApolice;
       ON-ERROR;
         WCdRamo    = *ZEROS;
         WNuapolice = *ZEROS;
       ENDMON;

       //--------------------------------------------
       // Validar se a Apólice já foi convertida
       //--------------------------------------------
       IF (WNuApolice <> *ZEROS);
          CHAIN(N) (WCdRamo:WNuApolice) RPRDP001;
          IF (NOT %FOUND());
             $ShowDialog('PRD0ZZZ');
             *INLR = *ON;
             RETURN;
          ENDIF;
          EXSR ExecDelete;
       ELSE;
          IF (S#USR <> 'CALIMERO') OR ((S#USR = 'CALIMERO') AND
             ($ShowDialog('PRD0178':*BLANKS:'NS') = 'N'));
             *INLR = *ON;
             RETURN;
          ENDIF;
          SETLL *LOVAL RPRDP009;
          READ(N) RPRDP009;
          DOW (NOT %EOF(FPRDP009));
             WCdRamo = PRD_CDRAMO;
             WNuApolice = PRD_NUAPOL;
             EXSR ExecDelete;
             READ(N) RPRDP009;
          ENDDO;
       ENDIF;

       *INLR = *ON;
       RETURN;

       //================================================================
       // Subrotina.: ExecDelete
       // Objectivo.: Proceder à Eliminação dos Registo
       //================================================================
       BEGSR ExecDelete;
          //--------------------------------------------
          // Proceder à Eliminação dos Registos
          //--------------------------------------------
          DelNotasDesc(WCdRamo:WNuApolice);

          //-----------------------------------------------
          // Eliminar Códigos de Risco
          //-----------------------------------------------
          SETLL (WCdRamo:WNuApolice) RPRDP011;
          READE (WCdRamo:WNuApolice) RPRDP011;
          DOW (NOT %EOF(FPRDP011));
             DELETE RPRDP011;
             READE (WCdRamo:WNuApolice) RPRDP011;
          ENDDO;

          //-----------------------------------------------
          // Eliminar Limites e Franquias
          //-----------------------------------------------
          SETLL (WCdRamo:WNuApolice) RPRDP009;
          READE (WCdRamo:WNuApolice) RPRDP009;
          DOW (NOT %EOF(FPRDP009));
             DELETE RPRDP009;
             READE (WCdRamo:WNuApolice) RPRDP009;
          ENDDO;

          //-----------------------------------------------
          // Eliminar Coberturas
          //-----------------------------------------------
          SETLL (WCdRamo:WNuApolice) RPRDP008;
          READE (WCdRamo:WNuApolice) RPRDP008;
          DOW (NOT %EOF(FPRDP008));
             DELETE RPRDP008;
             READE (WCdRamo:WNuApolice) RPRDP008;
          ENDDO;

          //-----------------------------------------------
          // Eliminar Características do Edificio
          //-----------------------------------------------
          SETLL (WCdRamo:WNuApolice) RPRDP016;
          READE (WCdRamo:WNuApolice) RPRDP016;
          DOW (NOT %EOF(FPRDP016));
             DELETE RPRDP016;
             READE (WCdRamo:WNuApolice) RPRDP016;
          ENDDO;

          //-----------------------------------------------
          // Repor Tipo de Acta
          //-----------------------------------------------
          CHAIN (WCdRamo:WNuApolice) RPRDP001;
          PRD_TPACTA = 'A';
          UPDATE RPRDP001 %FIELDS(PRD_TPACTA);
          COMMIT;
       ENDSR;
      /END-FREE

     P*=======================================================
     P* Processo..: DelNotasDesc
     P* Objectivo.: Eliminar as Notas Descritivas para um
     P*             Contrato.
     P*=======================================================
     P DelNotasDesc    B                   EXPORT
     D DelNotasDesc    PI              N
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D
     D*==> Campos de Trabalho
     D WCdRamo         S              2S 0 INZ(*ZEROS)
     D WNuApolice      S              7S 0 INZ(*ZEROS)
     D*=========================================================
      /FREE
        WCdRamo    = PRM_CdRamo;
        WNuApolice = PRM_NuApol;

        SETLL (WCdRamo:WNuApolice) RPRDP012;
        READE (WCdRamo:WNuApolice) RPRDP012;
        DOW (NOT %EOF(FPRDP012));
           DELETE RPRDP012;
           READE (WCdRamo:WNuApolice) RPRDP012;
        ENDDO;

        SETLL (WCdRamo:WNuApolice) RPRDP01003;
        READE (WCdRamo:WNuApolice) RPRDP01003;
        DOW (NOT %EOF(FPRDP01003));
           MONITOR;
               DELETE (WCdRamo:WNuApolice:PRD_NUSEQU) RPRDP010;
           ON-ERROR;
           ENDMON;
           READE (WCdRamo:WNuApolice) RPRDP01003;
        ENDDO;

        CHAIN(N) (WCdRamo:WNuApolice) RPRDP010;
        IF (%FOUND());
           DelNotasDesc(WCdRamo:WNuApolice);
        ENDIF;
        RETURN *ON;
      /END-FREE
     P DelNotasDesc    E
