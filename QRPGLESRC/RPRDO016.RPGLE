     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO015
     H* Descri��o.: Coment�rios e Observa��es
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
     D*==> Estrutura de dados do Hist�rico
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D WptrHist        S               *
     D WptrOr          S               *
     D WLinha          S            500A
     D WCdProd         S              5S 0
     D WCodErro        S              7A
     D WMALT           S                   LIKE(HST_MALT)
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
       WCodErro = *BLANKS;
       ptrHist  = WptrHist;
       ptrOr    = WptrOR;
       MONITOR;
          WMALT = HST_MALT;
       ON-ERROR;
          WMALT = OR_MALT;
       ENDMON;

       EXSR AddTexto; // ForCompr as Linhas de Texto

       RETURN;

       //================================================================
       // Subrotina..: AddTexto
       // Objectivo..: Escrever os textos nas Ap�lices referente �s
       //              indexa��es.
       //================================================================
       BEGSR AddTexto;
          //------------------------------------
          // Pontos 1.2. e 1.3. do Memo
          //------------------------------------
          IF (OR_TALT = 1 OR OR_TALT = 4 OR (OR_TALT = 5 AND
             (WMALT < 61 OR WMALT > 65)));
             $addLineType('T1');
             $AddTextLine();
             WLinha  = 'As demais condi��es da Ap�lice permanecem +
                        inalteradas.';
             $AddTextLine(WLinha);
          ENDIF;

          $AddTextLine();
          WLinha = 'Esta Ap�lice � constitu�da pela proposta que lhe serviu +
                    de base, pelas Condi��es Gerais e Especiais e pelas +
                    presentes Condi��es Particulares.';
          $AddTextLine(WLinha);

          //------------------------------------
          // Pontos 1.2. e 1.3. do Memo
          //------------------------------------
          IF (OR_TALT = 0) OR (OR_TALT = 1) OR (OR_TALT = 5);
             $AddTextLine();
             WLinha = 'O Pr�mio Total indicado inclui as taxas fiscais e +
                        parafiscais e, quando aplic�vel, a de Fraccionamento +
                        e o custo da Ap�lice ou Acta Adicional.';
             $AddTextLine(WLinha);
          ENDIF;
       ENDSR;
      /END-FREE
