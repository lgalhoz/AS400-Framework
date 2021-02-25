     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO015
     H* Descrição.: Comentários e Observações
     H*
     H*==================================================================
     FGBASEGRO  IF   E           K DISK    PREFIX(OR_)
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SDBSO010

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
     D WptrHist        S               *
     D WptrOr          S               *
     D WLinha          S            500A
     D WCdProd         S              5S 0
     D WIcTituloH1     S               N
     D WIcTituloH2     S               N
     D WSession        S              5S 0
     D WCodErro        S              7A
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WCodErro
     C                   PARM                    WPtrHist
     C                   PARM                    WPtrOR
      /FREE
       WCodErro    = *BLANKS;
       ptrHist     = WptrHist;
       ptrOr       = WptrOR;
       WIcTituloH1 = *OFF;
       WIcTituloH2 = *OFF;
       EXSR AddTexto; // ForCompr as Linhas de Texto

       RETURN;

       //================================================================
       // Subrotina..: AddTexto
       // Objectivo..: Escrever os textos nas Apólices referente às
       //              indexações.
       //================================================================
       BEGSR AddTexto;
          WSession = $SetFiles('FPRDP016');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDRAMO = '''+%CHAR(WRamo)+''') AND +
                        (NUAPOL = '''+%CHAR(WApolice)+''')');
          IF ($GetRecord() > *ZEROS);
             //------------------------------------------
             // Escrever texto sobre o edificio adjacente
             //------------------------------------------
             IF ($GetField('CDEDIADJ')<>*BLANKS) AND
                  ($GetField('CDEDIADJ')<>'00000');
                EXSR AddTituloH1;
                $addLineType('H2');
                WLinha  = 'Natureza/Uso do Imóvel adjacente';
                $AddTextLine(WLinha);
                $AddLineType('B1');
                WLinha  = 'As condições de Subscrição do Risco Seguro +
                           encontram-se influenciadas pela existência de +
                           um edificio adjacente utilizado para ' +
                           $getDescricao(49:$GetField('CDEDIADJ'));
                $AddTextLine(WLinha);
             ENDIF;

             //------------------------------------------
             // Escrever texto sobre Tipo Construção/estado
             //------------------------------------------
             IF ($GetField('CDESTCONS')<>*BLANKS);
                EXSR AddTituloH1;
                EXSR AddTituloH2;
                $AddLineType('B1');
                WLinha  = 'O Edificio encontra-se em '+
                          %TRIM($getDescricao(52:$GetField('CDESTCONS'))) +
                          ' estado de Conservação.';
                $AddTextLine(WLinha);
             ENDIF;
             IF ($GetField('CDTIPCONS') <> *BLANKS);
                EXSR AddTituloH1;
                EXSR AddTituloH2;
                $AddLineType('B1');
                WLinha  = 'O Edificio utiliza '+
                          %TRIM($getDescricao(50:$GetField('CDTIPCONS'))) +
                          ' na sua construção.';
                $AddTextLine(WLinha);
             ENDIF;
             IF ($GetField('CDTIPCOB') <> *BLANKS);
                EXSR AddTituloH1;
                EXSR AddTituloH2;
                $AddLineType('B1');
                WLinha  = 'O Telhado/cobertura é em '+
                          %TRIM($getDescricao(51:$GetField('CDTIPCOB')))
                          + '.';
                $AddTextLine(WLinha);
             ENDIF;
          ENDIF;
       ENDSR;

       //================================================================
       // Subrotina..: AddTituloH1
       // Objectivo..: Escrever o Titulo  nas Apólices referente às
       //              indexações.
       //================================================================
       BEGSR AddTituloH1;
          IF (NOT WIcTituloH1);
             $addLineType('H1');
             $AddTextLine();
             WLinha  = 'Caracterização do Edificio';
             $AddTextLine(WLinha);
             WIcTituloH1 = *ON;
          ENDIF;
       ENDSR;

       //================================================================
       // Subrotina..: AddTituloH2
       // Objectivo..: Escrever o Titulo  nas Apólices referente às
       //              indexações.
       //================================================================
       BEGSR AddTituloH2;
          IF (NOT WIcTituloH2);
             $addLineType('H2');
             $AddTextLine();
             WLinha  = 'TIPO CONSTRUÇÃO E ESTADO';
             $AddTextLine(WLinha);
             WIcTituloH2 = *ON;
          ENDIF;
       ENDSR;
      /END-FREE
