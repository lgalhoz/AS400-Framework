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
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002

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
     D WCodErro        S              7A
     C*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WCodErro
     C                   PARM                    WPtrHist
     C                   PARM                    WPtrOR
      /FREE
       WCodErro = *BLANKS;
       //ptrHist = WptrHist;
       //ptrOr   = WptrOR;
       //EXSR AddTexto; // ForCompr as Linhas de Texto

       RETURN;

       //================================================================
       // Subrotina..: AddTexto
       // Objectivo..: Escrever os textos nas Apólices referente às
       //              indexações.
       //================================================================
       BEGSR AddTexto;
          //------------------------------------
          // Pontos 1.2. e 1.3. do Memo
          //------------------------------------
          IF (OR_TALT = 1 OR OR_TALT = 4 OR (OR_TALT = 5 AND
             (HST_MALT < 61 OR HST_MALT > 65)));
             $addLineType('T1');
             $AddTextLine();
             WLinha  = 'As demais condições da Apólice permanecem +
                        inalteradas.';
             $AddTextLine(WLinha);
          ENDIF;

          $AddTextLine();
          WLinha = 'Esta Apólice é constituída pela proposta que lhe serviu +
                    de base, pelas Condições Gerais e Especiais e pelas +
                    presentes Condições Particulares.';
          $AddTextLine(WLinha);

          //------------------------------------
          // Pontos 1.2. e 1.3. do Memo
          //------------------------------------
          IF (OR_TALT = 0) OR (OR_TALT = 1) OR (OR_TALT = 5);
             $AddTextLine();
             WLinha = 'O Prémio Total indicado inclui as taxas fiscais e +
                        parafiscais e, quando aplicável, a de Fraccionamento +
                        e o custo da Apólice ou Acta Adicional.';
             $AddTextLine(WLinha);
          ENDIF;
       ENDSR;
      /END-FREE
