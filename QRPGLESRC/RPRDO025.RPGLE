     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=================================================================================
     H* Programa..: RPRDO025
     H* Descrição.: Actualizar Capitais das Notas descritivas
     H*=================================================================================
     FFPRDP010  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP012  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP013  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP015  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP017  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP011  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFCATP106  IF   E           K DISK    PREFIX(CAT_)
     F*=================================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D WCodRamo        S              2S 0
     D WNuApolice      S              7S 0
     D WTaxa           S              5S 4
     D WIncidencia     S              1A
     D WDataValor      S              8S 0
     D WCodErro        S              7A
     D
     D WDBSession      S              5S 0
     D WRegis          S              5S 0
     D*=================================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodRamo
     C                   PARM                    WNuApolice
     C                   PARM                    WTaxa
     C                   PARM                    WDataValor
     C                   PARM                    WCodErro
      /FREE
        RETURN;
      /END-FREE

     P*=======================================================
     P* Processo..: UpdCapRisco
     P* Objectivo.: Actualizar os Capitais de Risco
     P*=======================================================
     P UpdCapRisco     B                   EXPORT
     D UpdCapRisco     PI
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D
      /FREE
       //--------------------------------------------------------------------
       // Seleccionar os Itens Activos e aplicar a Indexação
       //--------------------------------------------------------------------
       WDBSession = $SetFiles('FPRDP010 A INNER JOIN FPRDP012 B ON +
                               (A.CDRAMO=B.CDRAMO) AND (A.NUAPOL=B.NUAPOL)');
       $SetFields('A.NUSQSUP,A.CDTEXTO,B.CPSEGURO,B.NUSEQU');
       $SetCriteria('(A.CDRAMO = '''+%CHAR(PRM_CdRamo)+''') AND +
                     (A.NUAPOL = '''+%CHAR(PRM_Apolice)+''')');
       WRegis = 1;
       DOW ($GetRegisto(WRegis) > *ZEROS);






          WRegis += 1;
       ENDDO;

       $RmvDBSession(WDBSession);
       RETURN;
      /END-FREE
     P UpdCapRisco     E
