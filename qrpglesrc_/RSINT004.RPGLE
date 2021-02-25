     H ALWNULL(*USRCTL) DFTACTGRP(*NO)  ACTGRP(*CALLER)
     H* ACTGRP(*CALLER)
     H*=============================================================
     FFSINP102  UF A E           K DISK    PREFIX(D_)
     F                                     EXTFILE('LGALHOZ/FSINP102')
     FFSINP103  IF   E           K DISK    PREFIX(D_)
     F                                     EXTFILE('LGALHOZ/FSINP103')
     F*===============================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
     D
     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FSINP102)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FSINP102)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D*--> Obter Ultimo número disponível
     D getLast         PR             2S 0
     D   NuProc                       7A   CONST
     D   TipInterv                    1A   CONST
     D
     D WMsAccess       S               N
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    TrgBuffer
     C                   PARM                    TrgBufferLen
      /FREE

       OldRecPtr  = %ADDR(TrgBuffer) + TOldRecOff;
       NewRecPtr  = %ADDR(TrgBuffer) + TNewRecOff;
       OldNullPtr = %ADDR(TrgBuffer) + TOldNullOff;
       NewNullPtr = %ADDR(TrgBuffer) + TNewNullOff;

       IF (S#JOB = 'QZDASOINIT');
          WMsAccess = *ON;
       ELSE;
          WMsAccess = *OFF;
       ENDIF;

       IF (TTrgEvent = IO#UPD);
          //------------------------------------------------------
          // Não é possivel Alterar o nome do Interveninente no
          // MS.ACCESS quando está espelhado no AS400
          //------------------------------------------------------
          IF (O_NUMINT >*ZEROS) AND (O_INTERVEN <> N_INTERVEN) AND WMsAccess;
             N_INTERVEN = O_INTERVEN;
          ENDIF;
       ELSEIF (TTrgEvent = IO#DEL);
          //------------------------------------------------------
          // Não é possivel Eliminar um Interveniente se este
          // estiver espelhado no AS/400
          //------------------------------------------------------
          IF (O_NUMINT > *ZEROS) AND WMsAccess;
             D_NR        = O_NR;
             D_NORDEM    = O_NORDEM;
             D_NRPROC    = O_NRPROC;
             D_INTERVEN  = O_INTERVEN;
             D_SIN10201  = O_SIN10201;
             D_SIN10202  = O_SIN10202;
             D_SIN10203  = O_SIN10203;
             D_SIN10204  = O_SIN10204;
             D_SIN10205  = O_SIN10205;
             D_DATAALTA  = O_DATAALTA;
             D_DATAQUANT = O_DATAQUANT;
             D_SIN10206  = O_SIN10206;
             D_SIN10207  = O_SIN10207;
             D_SIN10208  = O_SIN10208;
             D_SIN10209  = O_SIN10209;
             D_SIN10210  = O_SIN10210;
             D_SIN10211  = O_SIN10211;
             D_SIN10212  = O_SIN10212;
             D_SIN10213  = O_SIN10213;
             D_SIN10214  = O_SIN10214;
             D_SIN10215  = O_SIN10215;
             D_SIN10216  = O_SIN10216;
             D_EXAME_MED = O_EXAME_MED;
             D_SIN10217  = O_SIN10217;
             D_POSI_SEG  = O_POSI_SEG;
             D_ENTCREDORA= O_ENTCREDORA;
             D_TIPDOCQUIT= O_TIPDOCQUIT;
             D_SIN10218  = O_SIN10218;
             D_SIN10219  = O_SIN10219;
             D_PRENCERISP= O_PRENCERISP;
             D_SIN10220  = O_SIN10220;
             D_SIN10221  = O_SIN10221;
             D_ENVIADOS  = O_ENVIADOS;
             D_ENVIODC   = O_ENVIODC;
             D_SIN10222  = O_SIN10222;
             D_NUMINT    = O_NUMINT;
             D_TIPINT    = O_TIPINT;
             MONITOR;
                WRITE RSINP102;
             ON-ERROR;
             ENDMON;
          ENDIF;
       ELSEIF (TTrgEvent = IO#INS);
          //------------------------------------------------------
          // Não é possivel criar Intervenientes pelo Ms.Access
          // Quando o registo está espelhado no AS400
          //------------------------------------------------------
          IF (GetLast(N_NRPROC:'T') > *ZEROS) AND WMsAccess;
             MONITOR;
                DELETE (N_NRPROC:N_NORDEM) RSINP102;
             ON-ERROR;
             ENDMON;
          ENDIF;
       ENDIF;

       *INLR = *ON;
      /END-FREE
     D*================================================================
     P* Processo..: GetLast
     P* Descrição.: Obter o último código livre
     D*================================================================
     P GetLast         B
     D GetLast         PI             2S 0
     D   NuProcesso                   7A   CONST
     D   TipInterv                    1A   CONST
     D WNuOrdem        S              2S 0 INZ(*ZEROS)
     D*================================================================
      /FREE
          IF (TipInterv = 'S');
            RETURN 1;
          ELSE;
             MONITOR;
                WNuOrdem = 1;
                SETLL (NuProcesso) RSINP103;
                READE(N) (NuProcesso) RSINP103;
                DOW (NOT %EOF());
                   IF (N_NORDEM = D_NORDEM);
                   ELSEIF (D_NUMINT=*ZEROS) OR (D_TIPINT=*BLANKS);
                      RETURN -1;
                   ELSEIF (WNuOrdem < %DEC(D_NORDEM:2:0));
                      WNuOrdem = %DEC(D_NORDEM:2:0);
                   ENDIF;
                   READE(N) (NuProcesso) RSINP103;
                ENDDO;

                SETLL (NuProcesso) RSINP102;
                READE(N) (NuProcesso) RSINP102;
                DOW (NOT %EOF());
                   IF (N_NORDEM = D_NORDEM);
                   ELSEIF (D_NUMINT=*ZEROS) OR (D_TIPINT=*BLANKS);
                      RETURN -1;
                   ELSEIF (WNuOrdem < %DEC(D_NORDEM:2:0));
                      WNuOrdem = %DEC(D_NORDEM:2:0);
                   ENDIF;
                   READE(N) (NuProcesso) RSINP102;
                ENDDO;
             ON-ERROR;
             ENDMON;
          ENDIF;
          WNuOrdem += 1;
          RETURN WNuOrdem;
      /END-FREE
     P GetLast         E
