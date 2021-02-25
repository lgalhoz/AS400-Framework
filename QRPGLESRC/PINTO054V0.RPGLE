     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PINTE054
     H* Descrição.: Manutenção da Informação de Mediadores
     H*==================================================================
     FFINTE054  CF   E             WORKSTN SFILE(RINTE05402:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================*
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SGRLO013

     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Chamada Dinâmica a funções
     D ShowDetail      PR                  EXTPGM('PCOBO201')
     D   NumRecibo                    9S 0 CONST
     D   Modo                          N   CONST
     D   Chamador                     5S 0 CONST
     D
     D*==> Lista de Pastas de interfaces
     D AddFolder       PR
     D   Pasta                      100A   CONST
     D
     D Lst#Pastas      DS                  QUALIFIED DIM(25)
     D   Nompasta                   100A
     D   Isempty                       N
     D
     D*==> Campos de Trabalho
     D WQtdeRegistos   S              3S 0
     D WMsgErro        S              7A
     D WNoFunction     S             10A
     D WSelected       S               N
     D WExtKeys        S             24A
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WListActual     S              1A
     D
     D REC#            S              4S 0
     D LST#INTER       S              5S 0
     D LST#MEDIA       S              5S 0
     D
     D*==> Constantes
     D MOD#Recibo      C                   'R'
     D MOD#Anomalia    C                   'A'
     D MOD#Interface   C                   'I'
     D EST#Aberto      C                   'A'
     D SES#Interface   C                   'I'
     D SES#Mediador    C                   'M'
     D*==================================================================*
      /FREE

       //------------------------------------
       // Validar o acesso a esta função
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------------
       // Sessão de Interfaces
       //----------------------------------------------
       LST#INTER = $SetFiles('FINTP055');
       $addCriteria('Data.......:':'DTINICIO':'GE':'D');
       $addCriteria('Estado.....:':'CDESTADO':'LK');

       $SetLstFld('DTINICIO':'Inicio':10:'C':'D');
       $SetLstFld('DTTERMO' :'Termo':10:'C':'D');
       $SetLstFld('CDESTADO':'Estado':40:'E');
       $SetLstFld('DTULTPRC':'Ultimo':10:'C':'D');

       $SetPage(12);

       //----------------------------------------------
       // Sessão de Mediadores
       //----------------------------------------------
       LST#MEDIA = $SetFiles('FINTP050');
       $addCriteria('Código.....:':'ALIAS':'GE':'D');
       $addCriteria('Nome.......:':'NOMMED':'LK');

       $SetLstFld('ALIAS':'Código':7:'C');
       $SetLstFld('NOMMED' :'Nome':40:'E');
       $SetLstFld('ISAPOLIC':'Apol.':5:'C');
       $SetLstFld('ISSEGURA':'Seg.':5:'C');
       $SetLstFld('ISRECPND':'Rec.':5:'C');
       $SetPage(12);

       WListActual = SES#Interface;
       $SetDBSession(LST#INTER);
       ECR_DSCRITER = $getCritText();

       EXSR ListShow;

       $RmvDBSession(LST#MEDIA);
       $RmvDBSession(LST#INTER);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ListShow
       // Objectivo.: Apresentar o detalhe dos registo da lista
       //=========================================================
       BEGSR ListShow;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             IF (WlistActual = SES#Interface);
                WNoFunction = 'RINTE05403';
             ELSEIF (WListActual = SES#Mediador);
                WNoFunction = 'RINTE05405';
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RINTE05400;
             WRITE RINTE05499;
             WRITE RINTE05498;
             EXFMT RINTE05403;
             IF ($ValidKey(Func#Key:KEY#F05));
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF ($ValidKey(Func#Key:KEY#F09));
                IF (WlistActual = SES#Interface);
                   WListActual = SES#Mediador;
                   $SetDBSession(LST#MEDIA);
                ELSEIF (WListActual = SES#Mediador);
                   WListActual = SES#Interface;
                   $SetDBSession(LST#INTER);
                ENDIF;
                ECR_DSCRITER = $getCritText();
             ELSEIF (Func#Key = KEY#Enter);
                EXSR SelRec;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          //-------------------------------------------------------
          // Inicializar os ReEgistos
          //-------------------------------------------------------
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RINTE05403;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã
          WQtdeRegistos = *ZEROS;

          //-------------------------------------------------------
          // Obter os Registos
          //-------------------------------------------------------
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                WQtdeRegistos += 1;
                //------------------------------------------------                     CDMEDIA
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();
                ECR_LST#DET = $GetLstLine();
                IF (WlistActual = SES#Interface);
                   ECR_CDPROCE = $GetFieldNum('CDPROCE');
                ELSEIF (WListActual = SES#Mediador);
                   ECR_CDMEDIA = $GetFieldNum('CDMEDIA');
                ENDIF;

                REC# += 1;
                ECR_RRN = 1;
                WRITE RINTE05402;
                *IN52 = *ON;       // Encontrou registos
                $getNext();
             ENDDO;
          ENDIF;
          *IN54 = $IsLastPage();   // Indicação de Fim de Página
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          WSelected = *OFF;
          READC RINTE05402;
          DOW (NOT %EOF());
             WSelected = *ON;
             IF ($ValidOption(ECR_OPTION:'2':S#PGM:WNoFunction));
                IF (WlistActual = SES#Interface);

                ELSEIF (WListActual = SES#Mediador);

                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'4':S#PGM:WNoFunction));
             ELSEIF ($ValidOption(ECR_OPTION:'5':S#PGM:WNoFunction));
             ELSEIF ($ValidOption(ECR_OPTION:'6':S#PGM:WNoFunction));
             ENDIF;
             READC RINTE05402;
          ENDDO;
          IF (NOT WSelected);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
