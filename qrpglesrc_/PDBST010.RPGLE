     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=======================================================
     H* Dat.Cria...: 2012-03-29
     H* Autor......: Luis Galhoz
     H* Biblioteca.: RDBSO010
     H* Objectivo..: Funções de Leitura aos Dados da Base de Dados
     H*=======================================================
      /Copy QRPGLESRC,SDBSO010

     D WCdIndice       S              5A
     D WCdCodigo       S              5A
     D WDescCod        S             40A
     D WNome           S             40A
     D WQtdReg         S              5S 0
     D WResult         S              3S 0
     D*=====================================================================
      /FREE
          SETFILES('GBASEJM');
          SETFIELDS('RAMO, APOL, NSEG, SEG, MOR');
          SETSORT('RAMO DESC, APOL');
          GetRecord(10:12);
          DOW (NOT IsEOF());
             WNome = GetField('SEG');
             getNext();
          ENDDO;


         SETFILES('FTBLP012');
         SETFIELDS('CDINDICE, CDCODIGO, DESCCOD');
          SETSORT('CDINDICE, CDCODIGO');
         SETCRITERIA('CDINDICE = 1');
         WQtdReg = GetQtdeReg();
         WResult = GetRecord(1:5);
         DOW (NOT IsEOF());
            WCdIndice = GetField('1');
            WCdCodigo = GetField('CDCODIGO');
            WDescCod  = GetField('DESCCOD');
            getNext();
         ENDDO;

         SETCRITERIA('CDINDICE = 22');
         WQtdReg = GetQtdeReg();
         WResult = GetRecord(1:5);
         DOW (NOT IsEOF());
            WCdIndice = GetField('1');
            WCdCodigo = GetField('CDCODIGO');
            WDescCod  = GetField('DESCCOD');
            getNext();
         ENDDO;

         *INLR = *ON;
         RETURN;
      /END-FREE
