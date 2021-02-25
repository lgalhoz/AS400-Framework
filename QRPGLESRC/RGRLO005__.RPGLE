     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO200
     H* Descrição.: Manutenção de fuicheiros de Interfaces
     H*=================================================================
     FSGRLE005  CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================*
      /Copy QRPGLESRC,SGRLO010

     D*==> Formato de Registo
     D REC           E DS                  EXTNAME('SGRLE005':*OUTPUT)
     D                                     PREFIX(ECR_)
     D ECR_ARDIAS             59    142
     D
     D*==> Array de dias
     D LST#Dias        DS                  QUALIFIED
     D   DM                           2S 0 DIM(42) OVERLAY(SGRLE00505:59)
     D WIdx            S              3S 0
     D
     D*==> Campos de Trabalho
     D
     D
     D
     D
     D
     D*==================================================================*
      /FREE
        WIdx = 1;
        DOW (WIdx < 43);
           LST#Dias.DM(WIdx) = WIdx;
           WIdx += 1;
        ENDDO;

        //ECR_ARDIAS = LST#Dias.ARDIAS;
        DOW (NOT *IN12 OR *IN03);
           EXFMT RGRLE00505;
           IF (Func#Key = KEY#Enter);
           // LEAVE;
           ENDIF;
           DSPLY ECR_CM#NMFLD;
        ENDDO;
       *INLR = *ON;

       //======================================================
       // Subrotina.: LinkCalendar
       // Objectivo.: Ligar lista ao Calendário
       //======================================================
       BEGSR LinkCalendar;
       // LST#Dias(01).DiaMes  = %ADDR(ECR_DOM01);
       // LST#Dias(01).IsEmpty = *OFF;
       ENDSR;
      /END-FREE
