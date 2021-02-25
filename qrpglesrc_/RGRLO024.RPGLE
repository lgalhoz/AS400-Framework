     H NOMAIN
     F*=======================================================
     F* Dat.Cria...: 2012-04-26
     F* Autor......: Luis Galhoz
     F* Objectivo..: Funções de Catálogo de Produtos
     F*=======================================================
     FFGRLP010  IF   E           K DISK    PREFIX(GRL_)
     F*=======================================================
      /Copy QRPGLESRC,SGRLO010
     P*=======================================================
     P* Processo..: $GetStyleDesc
     P* Objectivo.: Devolve a descrição do estilo
     P*=======================================================
     P $GetStyleDesc   B                   EXPORT
     D                 PI           100A
     D    CdPagina                    5S 0 Const
     D    CdStyle                     2A   Const
     D*=======================================================
      /FREE

         MONITOR;
            CHAIN (CdPagina:CdStyle) RGRLP010;
            IF (%FOUND());
               RETURN GRL_DSTIPO;
            ENDIF;
         ON-ERROR;
         ENDMON;
         RETURN *BLANKS;
      /END-FREE
     P $GetStyleDesc   E
