      *=======================================================
      * Dat.Cria...: 2011-05-12
      * Autori.....: Luis Galhoz
      * Processo...: GetChave
      * Objectivo..: Obter a chave disponível na Tabela.
      *=======================================================
     D getChave        PR             5S 0
     D  WCdIndice                     5S 0 CONST
     C                   RETURN
     C*=======================================================
     P getChave        B                   EXPORT
     D getChave        PI             5S 0
     D    WCdIndice                   5S 0 CONST
     D
     D WCdCodigo       S              5S 0
     D WRetCode        S              5S 0
     F*=====================================================================
      /FREE
           WRetCode = 0;
           IF (WCdIndice = 0);
              EXSR GetLastIndice;
           ELSE;
              EXSR GetLastCodigo;
          ENDIF;
          WRetCode += 1;

          RETURN WRetCode;
       //============================================================
       // Subrotina..: GetLastIndice
       // Objectivo..: Obter o ultimo numero disponivel
       //============================================================
       BEGSR GetLastIndice;
          exec sql
             declare C1 cursor for
             select MAX(CDINDICE) from ftblp010
             for fetch only;

          exec sql open C1;
          if (sqlcod = 0);
             exec sql fetch C1 INTO: WRetCode;
          endif;
          exec sql close C1;
       ENDSR;

       //============================================================
       // Subrotina..: GetLastcodigo
       // Objectivo..: Obter o ultimo código disponivel
       //============================================================
       BEGSR GetLastCodigo;
         exec sql
            declare C2 cursor for
            select MAX(CDCODIGO) from ftblp012 where CDINDICE =: WCdIndice
            for fetch only;

          exec sql open C2;
          if (sqlcod = 0);
             exec sql fetch C2 INTO: WRetCode;
          endif;
          exec sql close C2;
       ENDSR;
      /END-FREE
     P getChave        E
