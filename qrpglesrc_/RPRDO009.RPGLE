     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RRDO009
     H* Descrição.: Escreve bloco de texto, correspondente ao Header Standar
     H*
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,STBLO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Estrutura de dados do Histórico
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D*==> Restantes Campos de Trabalho
     D WCdProd         S              5S 0
     D WDatTermo       S              8S 0
     D WDatEmissao     S              8S 0
     D WIdx            S              5S 0
     D WOrigem         S              1A
     D WCodEquivLusi   S              6A
     D WDescricao      S             40A
     D WLinha          S            500A
     D WptrHist        S               *
     D WptrOr          S               *
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatEmissao
     C                   PARM                    WPtrHist
     C                   PARM                    WPtrOR
      /FREE

       ptrHist = WptrHist;
       ptrOr   = WptrOR;

       $addLineType('HR'); // Header Principal
       $addException(*ON);
       EXSR MakeHeader; // Formatar as Linhas de Texto
       $addException(*OFF);
       $addLineType('T1'); // Texto Normal

       *INLR = *ON;
       RETURN;

       //==================================================================
       // Subrotina..: MakeHeader
       // Objectivo..: Obter os Elementos para o Header e formatar as linhas
       //              de Texto.
       //==================================================================
       BEGSR MakeHeader;
          $AddTextLine(*BLANKS:7);  // Adicionar linhas em Branco
          WLinha = *BLANKS;
          %SUBST(WLinha:6) = 'ORIGINAL';
          %SUBST(WLinha:25) = %EDITW(HST_NSEG:'0     ');
          %SUBST(WLinha:32) = %EDITW(HST_JMM1:'0   ');
          %SUBST(WLinha:37) = %EDITW(HST_JMM2:'0   ');
          $AddTextLine(WLinha);

          $AddTextLine();          // Adicionar linhas em Branco
          WLinha = *BLANKS;
          %SUBST(WLinha:10) = $GetDescricao(26:%TRIM(%EDITW(HST_RR:'0  '))  );
          $AddTextLine(WLinha);

          $AddTextLine();  // Adicionar linhas em Branco
          WLinha = *BLANKS;
          %SUBST(WLinha:10) = $GetDescRamo(HST_RR);
          $AddTextLine(WLinha);

          $AddTextLine();
          WLinha = *BLANKS;
          %SUBST(WLinha:12)=%TRIM(%EDITW(HST_RR:'0  ')+'-'+%CHAR(HST_APOL));
          %SUBST(WLinha:33) = %CHAR(HST_ACTA);
          $AddTextLine(WLinha);
          $AddTextLine(*BLANKS:2);

          WLinha = *BLANKS;
          IF (HST_TOTREC > *ZEROS);
             WDescricao = $GetDescricao(47:%CHAR(HST_TREG));
          ELSE;
             WDescricao = $GetDescricao(47:'99');
          ENDIF;
          %SUBST(WLinha:14) = $ApplyDatFmt(WDatEmissao:'YYMD':'/':'DMYY')+
                              '  ' + WDescricao;
          %SUBST(WLinha:52) = HST_SEG;
          $AddTextLine(WLinha);

          WLinha = *BLANKS;
          %SUBST(WLinha:52) = HST_MOR;
          $AddTextLine(WLinha);

          WLinha = *BLANKS;
          %SUBST(WLinha:6) = 'INÍCIO: ' +
                              $ApplyDatFmt(HST_DTINIC:'DMY':'/':'DMYY') +
                              ' ' + $GetDescricao(48:%CHAR(HST_DUR));
          %SUBST(WLinha:52) = %EDITW(HST_CPMOR1:'    -   ') +
                              ' ' + %TRIM(HST_CPMOR2);
          $AddTextLine(WLinha);

          WLinha = *BLANKS;
          WDatInicio = $CvtData(HST_DTALT:'DMY');
          WDatTermo = $CvtData(HST_DTINIC:'DMY');
          WDatTermo = $findDate(WDatTermo:'F':WDatInicio);
          $AddTextLine(WLinha);
          %SUBST(WLinha:6) = 'ALTERAÇÃO: '+
                              $ApplyDatFmt(WDatInicio:'YYMD':'/':'DMYY')+
                           ' - '+$ApplyDatFmt(WDatTermo:'YYMD':'/':'DMYY');
          $AddTextLine(WLinha);

          $AddTextLine();
          WLinha = *BLANKS;
          %SUBST(WLinha:6) = 'MOTIVO: '+
                            $GetDescricao(27:%TRIM(%EDITW(HST_MALT:'0  ')));
          $AddTextLine(WLinha);

          IF (HST_TOTREC > *ZEROS);
             $AddTextLine();
             WLinha = *BLANKS;
             %SUBST(WLinha:7) = %CHAR(HST_NREC);
             %SUBST(WLinha:20) = 'PRÉMIO COMERCIAL';
             %SUBST(WLinha:37) = %EDITW(HST_PRS:'¤ .   .   ,  ');
             IF (HST_FRAC > 1) AND
                 ((OR_TALT = 0) OR ((OR_TALT = 1) AND (HST_MALT = 70)));
                %SUBST(WLinha:36) = 'PRÉMIO TOTAL DE CADA FRACÇÃO';
                %SUBST(WLinha:65) = %EDITW(HST_TOTREC:'¤ .   .   ,  ');
             ELSE;
                %SUBST(WLinha:52) = 'PRÉMIO TOTAL';
                %SUBST(WLinha:65) = %EDITW(HST_TOTREC:'¤ .   .   ,  ');
             ENDIF;
             $AddTextLine(WLinha);
             $AddTextLine(*BLANKS:4);
          ELSE;
             WLinha = *BLANKS;
             $AddTextLine(*BLANKS:6);
          ENDIF;
       ENDSR;
      /END-FREE
