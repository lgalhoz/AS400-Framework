     H DFTACTGRP(*NO) BNDDIR('JMDIR') ALWNULL(*USRCTL)
     H*==============================================================================
     FAP6004E   CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==============================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO013

     D*=> Campos de Trabalho
     D WSessScr        S              3S 0
     D*==============================================================================
      /FREE
        WSessScr = $AddScreen('AP6004E':'RAP6004E05');
        $AddScPFld(%ADDR(ECR_F#01):'TPRISC');
        $AddScPFld(%ADDR(ECR_F#02):'CLTAR');
        $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
        $AddScMsg(%ADDR(ECR_ERR#MSG));

        DOW (NOT *IN03);
           *IN50 = *ON;
           WRITE RAP6004E00;
           WRITE RAP6004E99;
           EXFMT RAP6004E05;
           $InzScStatus();                      // Inicalizar campos em controlo
           IF (ECR_TPRISC = *BLANKS);
              $SetScError('TPRISC':'PRD0101');
           ENDIF;
           IF (ECR_CLTAR = *BLANKS);
              $SetScError('CLTAR':'PRD0102');
           ENDIF;

           IF ($GetScStatus() = 'O');
             *IN03 = *ON;
           ENDIF;
        ENDDO;
        $RmvScreen(WSessScr);

        *INLR = *ON;

      /END-FREE
