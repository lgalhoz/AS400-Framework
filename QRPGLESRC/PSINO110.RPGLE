     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PSINO110
     H* Descrição.: Pesquisa de Intervenientes forComp a área de dados
     H*             Local com o resultado da pesquisa.
     H*==================================================================
     FFSINE110  CF   E             WORKSTN SFILE(RSINE11002:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SCATO010

     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Estrutura de dados do Registo
     D WCodErro        S              7A
     D REC#            S              4S 0
     D
     D WComando        S            300A   INZ(*BLANKS)
     D WOption         S              5S 0 INZ(*ZEROS)
     D WDBSess01       S              5S 0 INZ(*ZEROS)
     D WDBSess02       S              5S 0 INZ(*ZEROS)
     D WSelected       S               N
     D WDataIni        S             10A   INZ(*BLANKS)
     D WAnoProc        S              2S 0 INZ(*ZEROS)
     D WNuProcess      S              6S 0 INZ(*ZEROS)
     D WCodEstado      S              1S 0 INZ(*ZEROS)
     D WNoFunction     S             10A
     C*========================================================
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------------
       // Sessão de Intervenientes
       //----------------------------------------------
       // WDBSess01 = $SetFiles('"G.SININT"':'A');
       WDBSess01 = $SetFiles('"G.SININT" A INNER JOIN "G.SINPRO" B ON +
                              (A.AAPROC = B.AAPROC) AND (A.NSEQ = B.NSEQ)');
       $addCriteria('Matricula..:':'A.IMATR1':'LK':'D');
       $addCriteria('Nome.......:':'A.NOMSIN':'LK':'D');
       $addCriteria('Processo..:':'A.NSEQ':'EQ':'D');
       $SetFields('A.IMATR1,A.NOMSIN,A.AAPROC,A.NSEQ,+
                   B.SPROC,B.DTINIC,A.CODINT,A.NREG');
       ECR_DSCRITER = $getCritText();
       $SetLstFld('A.NSEQ':'Processo':8:'D');
       $SetLstFld('A.CODINT':'T':1:'D');
       $SetLstFld('A.IMATR1':'Matricula':10:'E');
       $SetLstFld('A.NOMSIN':'Nome':30:'E');
       $SetLstFld('B.SPROC':'Est.':6:'E');
       $SetLstFld('B.DTINIC':'Abertura':10:'E');
       $SetPage(12);

       EXSR ShowInterv;
       $RmvDBSession(WDBSess01);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ShowInterv;
       // Objectivo.: Apresentar Lista de Intervenientes
       //=========================================================
       BEGSR ShowInterv;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WNoFunction = 'RSINE11003'; // Listar IDDs
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RSINE11000;
             WRITE RSINE11099;
             WRITE RSINE11098;
             EXFMT RSINE11003;
             IF (Func#Key = KEY#F05);
                $ChgCriteria();
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
          WRITE RSINE11003;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã

          //-------------------------------------------------------
          // Obter os Registos
          //-------------------------------------------------------
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();
                ECR_LST#DET = $GetLstLine();
                ECR_NREG    = $GetFieldNum('A.NREG');
                ECR_NSEQ    = $GetFieldNum('A.NSEQ');
                ECR_AAPROC  = $GetFieldNum('A.AAPROC');

                WDataIni = %CHAR(%INT($GetFieldNum('B.DTINIC') + 20000000));
                WCodEstado = $GetFieldNum('B.SPROC');
                WAnoProc = %INT($GetFieldNum('A.AAPROC'));
                WNuProcess = WAnoProc * 10000 + $GetFieldNum('A.NSEQ');
                %SUBST(ECR_LST#DET:2:7)  = %TRIM(%EDITW(WNuProcess:'0  -    '));
                %SUBST(ECR_LST#DET:61:10) = %SUBST(WDataIni:1:4) + '-' +
                                            %SUBST(WdataIni:5:2) + '-' +
                                            %SUBST(WdataIni:7:2);
                IF (WCodEstado = 1);
                   %SUBST(ECR_LST#DET:54:6)  = '1-Aber';
                ELSEIF (WCodEstado = 2);
                   %SUBST(ECR_LST#DET:54:6)  = '2-Reab';
                ELSEIF (WCodEstado = 5);
                   %SUBST(ECR_LST#DET:54:6)  = '5-Ence';
                ELSEIF (WCodEstado = 6);
                   %SUBST(ECR_LST#DET:54:6)  = '6-Aber';
                ENDIF;
                REC# += 1;

                //---------------------------------------------------
                // Validar existencia de erros
                //---------------------------------------------------
             // ECR_RRN = 1;
                WRITE RSINE11002;
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
          READC RSINE11002;
          DOW (NOT %EOF());
             WSelected = *ON;
             IF ($ValidOption(ECR_OPTION:'1'));
             ELSEIF ($ValidOption(ECR_OPTION:'2'));
             ELSEIF ($ValidOption(ECR_OPTION:'3'));
             ENDIF;
             READC RSINE11002;
          ENDDO;
          IF (NOT WSelected);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ShowDetail
       // Objectivo.: Apresentar o detalhe do registo
       //========================================================
       BEGSR ShowDetail;
       ENDSR;

       //========================================================
       // Subrotina.: LoadData
       // Objectivo.: Carregar a informação do Recibo
       //========================================================
       BEGSR LoadData;
       ENDSR;
      /END-FREE
