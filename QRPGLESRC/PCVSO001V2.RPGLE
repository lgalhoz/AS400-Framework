     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *=======================================================
      * Autor .....: Luis Galhoz
      * Data ......: 2013/07/17
      * Programa ..: PCVSO001
      * Objectivo .: Actualizar nova Estrutura a partir dos
      *              Ficheiros Temporários.
      *=======================================================
     FBASEAPOL  IF   F 1000        DISK    USROPN
     FFPRDP008  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Coberturas
     FFPRDP010  IF A E           K DISK    PREFIX(PRD_) COMMIT                  Notas Descritivas
     FFPRDP011  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Riscos da Apólice
     FFPRDP012  IF A E           K DISK    PREFIX(PRD_) COMMIT                  Texto das N.Descritivas
     FFPRDP009  UF A E           K DISK    PREFIX(PRD_) COMMIT                  Cláusulas e Condições
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura de dados do Registo
     D RBASE         E DS                  ExtName(GBASEDIO)
     D                                     Prefix(BAS_)
     D
     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D
     D*==> Actualizar a nova estrutura
     D UpdNewDB        PR                  EXTPGM('PCVSO002')
     D   CdProd                       5S 0 CONST
     D   DatCriacao                   8S 0 CONST
     D   Registo                       *   CONST
     D
     D*==> Campos de Trabalho
     D WCodSessao      S              5S 0 INZ(*ZEROS)
     D WNumRegis       S              5S 0 INZ(*ZEROS)
     D WDRegChar       S              8A   INZ(*BLANKS)
     D WDRegNum        S              8S 0 INZ(*ZEROS)
     D WCdProd         S              5S 0
     I*========================================================
     IBASEAPOL  AA  11
     I                                  1 1000  RBASE
     C*========================================================
      /FREE
        //------------------------------------------------------
        // Actualizar o adicionar novos Registos
        //------------------------------------------------------
        WCodSessao = $SetFiles('FGRLP023');
        $SetFields('CDFILE,NOLIBL,NOFILE,CHAR(DTCRIA),CHAR(HRCRIA)');
        $SetCriteria('CDFILE=21 OR CDFILE=22');
        $SetSort('DTCRIA #DESC, HRCRIA #DESC');
        WNumRegis = 1;
        DOW ($GetRecord(WNumRegis) > *ZEROS);
           MONITOR;
              WStdCmd = 'OVRDBF FILE(BASEAPOL) +
                         TOFILE('+%TRIM($GetField('NOLIBL'))+'/'+
                                  %TRIM($GetField('NOFILE'))+')';
              WStdLen = %LEN(WStdCmd);
              EXEC(WStdCmd:WStdLen);
              OPEN BASEAPOL;
              WDRegChar = $GetField('CHAR(DTCRIA)');
              WDRegChar = %SUBST(WDRegChar:1:2)+
                          %SUBST(WDRegChar:4:2)+
                          %SUBST(WDRegChar:7:2);
              WDRegNum  = $CvtData(%DEC(WDRegChar:6:0):'DMY':'YYMD');
              READ BASEAPOL;
              DOW (NOT %EOF());
                 UpdNewDB(BAS_RAMO:WDRegNum:%ADDR(RBASE));
                 READ BASEAPOL;
              ENDDO;
              CLOSE BASEAPOL;
           ON-ERROR;
           ENDMON;
           WNumRegis += 1;
        ENDDO;

        *INLR = *ON;
        RETURN;
      /END-FREE
