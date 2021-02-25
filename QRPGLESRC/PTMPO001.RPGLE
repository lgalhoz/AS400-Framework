     H  ALWNULL(*USRCTL) DFTACTGRP(*NO)
     H*==========================================================
     H* Programa..: PTMPP001
     H* Descrição.: Cobnverter Recibos Lusitânia
     H* Observação: A informação é recebida da Lusitânia com
     H*             base no extracto da conta 05700.
     H*==========================================================
     FFTMPP005  UP   E             DISK    PREFIX(TMP_)
     FFTMPP006  UF A E           K DISK    PREFIX(TMP_)
     FGCOBGRX7  IF   E           K DISK    PREFIX(COB_)
     F*==========================================================
     D WRefer          DS
     D   WReferenc             1     20A
     D   WCodAlfa              1      3A
     D   WApolice              5     11S 0
     D   WRecibo              13     18S 0
     D
     D WNuSequ         S              8S 0 INZ(*ZEROS)
     D WAno            S              2S 0
     D WNumAlf         S              5A
     D WNumNum         S              5S 0
     D WSair           S               N
     D
     D WNuRecibo       S              9S 0
     D WCdRamo         S              2S 0
     D WNuapol         S              7S 0
     D WNusegu         S              8S 0
     D WNome           S             40A
     D WTotPrem        S             13S 2
     D WTotComi        S             13S 2
     D WTipRecib       S              1S 0
     D WSitRecib       S              1S 0
     D WCdOrig01       S              7S 0
     D WCdOrig02       S              7S 0
     D WCdOrig03       S              7S 0
     D*==========================================================
      /FREE
         WNuSequ += 1;
         IF (TMP_CODIGO = '04') OR (TMP_CODIGO = '05') OR
             (TMP_CODIGO = '06');
            //-----------------------------------------
            // Localizar, Recibo e Apólice Original
            //-----------------------------------------
            EXSR FindRecibo;

            //-----------------------------------------
            // Criar ou actualizar Registo
            //-----------------------------------------
            EXSR AddRecibo;

         ENDIF;

         //-----------------------------------------
         // Actualizar Chave do Registo
         //-----------------------------------------
         TMP_CDREGIS = WNuSequ;
         UPDATE RTMPP005 %FIELDS(TMP_CDREGIS);

         //=========================================================
         // Subtorina..: FindRecibo
         // Objectivo..: Localizar Recibo e Apólice Lusitânia azul
         //=========================================================
         BEGSR FindRecibo;
            WReferenc = TMP_REFERENC;
            WNumNum   = *ZEROS;
            IF (WRecibo >= 800000) AND (WRecibo <= 899999);
               WNumNum = WRecibo - 800000;
            ENDIF;
            WNuRecibo = WNumNum;
            WCDRamo   = *ZEROS;
            WNuApol   = WApolice;
            WNuSegu   = *ZEROS;

            WNome     = TMP_TOMADOR;
            WTotPrem  = *ZEROS;
            WTotComi  = *ZEROS;
            WTipRecib = *ZEROS;
            WSitRecib = *ZEROS;
            WCDOrig01 = *ZEROS;
            WCDOrig02 = *ZEROS;
            WCDOrig03 = *ZEROS;

            WSair   = *OFF;
            WNumAlf = %TRIM(%EDITW(WNumNum:'0     '));
            WAno    = 13;
            DOW (NOT WSair);
               CHAIN (%CHAR(WAno):WNumAlf) GCOBGRX7;
               IF (%FOUND());
                  IF (COB_GCAPOL = WApolice);
                     WNuRecibo = COB_GCNREC;
                     WCDRamo   = COB_GCRAMO;
                     WNuApol   = COB_GCAPOL;
                     WNuSegu   = COB_GCNSEG;
                     WTipRecib = COB_GCTIPR;
                     WSitRecib = COB_GCSITR;
                     WSair     = *ON;
                  ENDIF;
               ENDIF;

               WAno -= 1;
               IF (WAno < 10);
                  WSair = *ON;
               ENDIF;
            ENDDO;

            IF (TMP_CODIGO = '04');
               IF (TMP_VLDEBIT > *ZEROS);
                  WTotPrem = TMP_VLDEBIT;
               ELSE;
                  WTotPrem = TMP_VLCREDIT;
               ENDIF;
            ELSE;
               IF (TMP_VLCREDIT > *ZEROS);
                  WTotComi = TMP_VLCREDIT;
               ELSE;
                  WTotComi = TMP_VLDEBIT;
               ENDIF;
            ENDIF;
         ENDSR;

         //=========================================================
         // Subtorina..: AddRecibo
         // Objectivo..: Adicionar ou actualizar a informação do recibo
         //=========================================================
         BEGSR AddRecibo;
            CHAIN (WNuRecibo) RTMPP006;
            IF (%FOUND());
               TMP_CDOrig02 = WNuSequ;
               IF (WTotPrem > *ZEROS);
                  TMP_TOTPREM  = WTotPrem;
                  UPDATE RTMPP006 %FIELDS(TMP_TOTPREM:TMP_CDORIG02);
               ELSE;
                  TMP_TOTCOMI  = WTotComi;
                  UPDATE RTMPP006 %FIELDS(TMP_TOTCOMI:TMP_CDORIG02);
               ENDIF;
            ELSE;
               TMP_CDOrig01 = WNuSequ;
               TMP_NURECIBO = WNuRecibo;
               TMP_CDRAMO   = WCDRamo;
               TMP_NUAPOL   = WNUApol;
               TMP_NUSEGU   = WNUSegu;
               TMP_NOME     = WNome;
               TMP_TOTPREM  = WTotPrem;
               TMP_TOTCOMI  = WTotComi;
               TMP_TIPRECIB = WTipRecib;
               TMP_SITRECIB = WSitRecib;
               TMP_CDORIG01 = WNuSequ;
               TMP_CDORIG02 = WCDOrig02;
               TMP_CDORIG03 = WCDOrig03;
               WRITE RTMPP006;
            ENDIF;
         ENDSR;
      /END-FREE
