     H*======================================================================
     H* Programa.: PSECO102
     H* Descrição: Este programa de Saída faz parte do ponto de saída
     H*            QIBM_QTMF_CLIENT_REQ e destina-se a restingir os acessos
     H*            FTP.
     H*
     H* Como aceder:
     H*       WRKREGINF + [enter]
     H*       QIBM_QTMF_CLIENT_REQ + [Opção 8]
     H*
     H* Comando a executar:
     H*       ADDEXITPGM EXITPNT(QIBM_QTMF_CLIENT_REQ)
     H*                  FORMAT(VLRQ0100)
     H*                  PGMNBR(1)
     H*                  PGM(#JMPRDOBJ/PSECO102)
     H*                  MLTTHDACN(*RUN)
     H*                  TEXT('Restringir acessos FTP')
     H*
     H*       RMVEXITPGM EXITPNT(QIBM_QTMF_CLIENT_REQ)
     H*                  FORMAT(VLRQ0100)
     H*                  PGMNBR(1)
     H*======================================================================
     D*==> Parâmetros de Entrada
     D APPIDIN         S              4A
     D OPIDIN          S              4A
     D USRPRF          S             10A
     D IPADDRIN        S             15A
     D IPLENIN         S              4A
     D OPLENIN         S              4A
     D OPINFOIN        S           9999A
     D ALLOWOP         S              4A
     D
     D*==> Campos de Trabalho
     D APPID           S              1S 0
     D OPID            S              1S 0
     D IPLEN           S              5S 0
     D IPADDR          S             15A
     D OPLEN           S              5S 0
     D OPINFO          S           9999A
     D PATHNAME        S           9999A
     D*======================================================================
     C     *Entry        PList
     C                   Parm                    APPIDIN
     C                   Parm                    OPIDIN
     C                   Parm                    USRPRF
     C                   Parm                    IPADDRIN
     C                   Parm                    IPLENIN
     C                   Parm                    OPLENIN
     C                   Parm                    OPINFOIN
     C                   Parm                    ALLOWOP
      /FREE
        Return;
      /END-FREE
