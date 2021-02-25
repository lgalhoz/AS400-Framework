     FFSINP105  UF   E           K DISK    PREFIX(D_)
      /Copy QRPGLESRC,SGRLO012

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName('G.SINOBS')
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName('G.SINOBS')
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D*=====> Actualizar texto de Observações
     D Chave           DS
     D   WAno                  1      2S 0
     D   WNSeq                 3      6S 0
     D   WNLin                 7      9S 0
     D
     D WPIni           S              3S 0
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    TrgBuffer
     C                   PARM                    TrgBufferLen
      /FREE
       //-----------------------------------------------------
       // Salvaguardar Campos
       //-----------------------------------------------------
       MONITOR;
          OldRecPtr  = %ADDR(TrgBuffer) + TOldRecOff;
          NewRecPtr  = %ADDR(TrgBuffer) + TNewRecOff;
          OldNullPtr = %ADDR(TrgBuffer) + TOldNullOff;
          NewNullPtr = %ADDR(TrgBuffer) + TNewNullOff;
       ON-ERROR;
          *INLR = *ON;
          RETURN;
       ENDMON;

       //-------------------------------------------------------
       // Aplicar modificações ao registo em função da Operação
       //-------------------------------------------------------
       IF (TTrgEvent=IO#UPD) OR (TTrgEvent = IO#INS);
          MONITOR;
             Chave = N_K00001;
             D_NRPROC   = %CHAR(WAno)+'9'+%CHAR(WNSeq);
             CHAIN (D_NRPROC) RSINP105;
             IF (%FOUND());
                IF (WNLin = 1);
                   %SUBST(D_SIN10507:1:62) = %SUBST(N_F00002:9:60);
                ELSE;
                   WPIni = ((WNLin-1)*70);
                   %SUBST(D_SIN10507:WPIni:62) = %SUBST(N_F00002:1:70);
                ENDIF;
                UPDATE RSINP105 %FIELDS(D_SIN10507);
             ENDIF;
          ON-ERROR;
          ENDMON;
       ELSEIF (TTrgEvent = IO#DEL);  // Eliminação
       ENDIF;

       *INLR = *ON;
      /END-FREE
