     H*======================================================================
     H* Programa.: PSECO100
     H* Descri��o: Este programa de Sa�da os atrav�s do Pararama de Sa�da
     H*            faz parte do ponto de sa�da QIBM_QZDA_SQL2, e destina-se
     H*            a restringir os acessos ao driver ODBC, do AS400.
     H*
     H* Como aceder:
     H*       WRKREGINF + [enter]
     H*       QIBM_QZDA_SQL2 + [op��o 8]
     H*
     H* Comando a executar:
     H*       ADDEXITPGM EXITPNT(QIBM_QZDA_SQL2)
     H*                  FORMAT(ZDAQ0200)
     H*                  PGMNBR(1)
     H*                  PGM(#JMPRDOBJ/PSECO100)
     H*                  MLTTHDACN(*RUN)
     H*                  TEXT('Restringir acessos ODBC')
     H*
     H*       RMVEXITPGM EXITPNT(QIBM_QZDA_SQL2)
     H*                  FORMAT(ZDAQ0200)
     H*                  PGMNBR(1)
     H*======================================================================
      /Copy QSYSINC/QRPGLESRC,EZDAEP

     DAcceptRequest    S              1
     D*======================================================================
     C     *Entry        PList
     C                   Parm                    AcceptRequest
     C                   Parm                    EZDSQLF2
      /FREE
        AcceptRequest = *On;

        //-------------------------------------------------------------------
        // Restringir a utiliza��o do dirver ODBC ao perfil de Utilizador
        //-------------------------------------------------------------------
        IF (EZDUP05='ODBCUSER') OR (%Subst(EZDUP05:1:1)='Q')
          OR (%TRIM(EZDUP05) <> 'LGALHOZ');
           AcceptRequest = *On;
        Else;
           AcceptRequest = *Off;
        EndIf;
        Return;
      /END-FREE
