     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H DECEDIT(',') DFTNAME(GP010) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
     H*=======================================================================
     H* Template...: PPRDO009
     H* Objectivo..: Manuten��o de ap�lices de Viagem
     H*=======================================================================
     F*--> Declara��o dos ficheiros nesta sec��o ....
     F*=======================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SENTO010
      /Copy QRPGLESRC,SDBSO012

     D*==> Op��es adicionais dos Ramos
     D ExtraOptions    PR                  EXTPGM('PPRDO100')
     D   RamoApolice                  9S 0 CONST
     D   PtrData                       *   CONST
     D   DatInicio                    8S 0 CONST
     D   Modo                          N
     D
     D*==> Manuten��o da Informa��o extra
     D AddExtra        PR                  EXTPGM('AP6000J')
     D   Ramo                         2A   CONST
     D   Apolice                      7A   CONST
     D
     D*==> Campos de Trabalho
     D PRM_Option      S              1A
     D PRM_RamoApol    S              9A
     D PRM_DaTInicio   S              8S 0
     D WOption         S              1A
     D WRamoApolice    DS
     D   WDSRamo               1      2S 0
     D   WCdRamoA              1      2
     D   WDSApolice            3      9S 0
     D   WNuApoliceA           3      9
     D*=======================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PRM_Option
     C                   PARM                    PRM_RamoApol
     C                   PARM                    PRM_DatInicio
      /FREE
        MONITOR;
           WRamoApolice = PRM_RamoApol;
           WRamo        = WDsRamo;
           WApolice     = WDSApolice;
        ON-ERROR;
           RETURN;
        ENDMON;

        WOption = '2';
        WDatInicio = $DateToInt(%DATE());
        MONITOR;
           WOption = PRM_Option;
        ON-ERROR;
        ENDMON;

        MONITOR;
           WDatInicio = PRM_DatInicio;
        ON-ERROR;
        ENDMON;

        AddExtra(WCdRamoA:WNuApoliceA);

        RETURN;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Ap�lice
       //==================================================================
       BEGSR RunPopUp;
          MONITOR;
          // IF (ECR_CM#NMFLD = 'SIT');
          //    SIT = %DEC($runPopUp(54:%CHAR(SIT):WModo):1:0);
          // ENDIF;
          ON-ERROR;
          ENDMON;
          // ECR_CM#ROW = CM#CROW;
          // ECR_CM#COL = CM#CCOL;
       ENDSR;
      /END-FREE
