     H ALWNULL(*USRCTL)  ACTGRP(*NEW) DFTACTGRP(*NO)
     H*=============================================================
     FFSINP105  UF A E           K DISK    PREFIX(D_)
      /Copy QRPGLESRC,SGRLO012

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName('G.SINPRO')
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName('G.SINPRO')
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D*==> Actualizar Intervenientes
     D ActInterven     PR                  EXTPGM('RSINO001_')
     D   WNuProcesso                  7A   CONST
     D   WModo                        1S 0 CONST
     D   WCodInterv                   9S 0 CONST
     D   WTipInterv                   1A   CONST
     D   WTipDanos                    1A   CONST
     D   WNome                       40A   CONST
     D   WMatricula                  20A   CONST
     D
     D TBD             S             10    DIM(9) CTDATA PERRCD(1)
     D WChave01        S              5S 0
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
       D_NRPROC   = %CHAR(N_AAPROC)+'9'+%CHAR(N_NSEQ);
       EXSR FixNullValues;
       IF (TTrgEvent = IO#UPD);      // Alteração
          MONITOR;
             CHAIN(N) (D_NRPROC) RSINP105;
             IF (%FOUND());
                IF (N_DTRINI=*ZEROS);
                   EXSR ChgProcSin;
                ENDIF;
             ELSE;
                EXSR AddProcSin;
                ActInterven(D_NRPROC:1:*ZEROS:*BLANKS:*BLANKS:
                            *BLANKS:*BLANKS);
             ENDIF;
          ON-ERROR;
          ENDMON;
       ELSEIF (TTrgEvent = IO#DEL);  // Eliminação
       ELSEIF (TTrgEvent = IO#INS);  // Insersão
          EXSR AddProcSin;
       ENDIF;

       *INLR = *ON;

       //===========================================================
       // SubRotina.: AddProcSin
       // Objectivo.: Adicionar um registo novo para o Processo
       //===========================================================
       BEGSR AddProcSin;
          IF (N_ACORDO = 'S');
             MONITOR;
                //-------------------------------------------------
                // Formatar Tipo de Documento
                //-------------------------------------------------
                IF (N_FPART >= '1') AND (N_FPART <= '9');
                   D_TIPODOC = TBD(%DEC(N_FPART:1:0));
                ENDIF;
                IF (N_FPART ='2');
                   D_EXISTEDAAA = 'S';
                ELSE;
                   D_EXISTEDAAA = 'N';
                ENDIF;
                IF (N_FPART = '6');
                   D_SIN10501 = 'FAX'; // (MEIOCOMUNIC)
                ELSE;
                   D_SIN10501 = 'CORREIO NORMAL';
                ENDIF;
                D_SIN10502 = 'N'; // (APLICLEIPORT)
                D_SIN10503 = 'N'; // (SUSPFRAUDE)
                D_SIN10504 = 'N'; // (FACTCLIMEXCEP)

                IF (N_DTFIM > *ZEROS);
                   D_SIN10505 = -1;  // (PROCENCERRADO)
                   D_SIN10506 = %DATE((20000000+N_DTFIM):*ISO);
                ELSE;
                   %NullInd(D_SIN10506) = *ON; //
                ENDIF;
                D_SIN10508 = -1;     // (DL832006)
                D_SIN10509 = *ZEROS; // (ENVIADOS)
                D_SIN10510 = 'N';    // (ASSRESPONS)
                D_SIN10511 = *ZEROS; // (ENVIO)
                D_SIN10512 = %DATE;  // (DATALTERA)
                %NullInd(D_DATAPART) = *ON;
                WRITE RSINP105;
             ON-ERROR;
             ENDMON;
          ENDIF;
       ENDSR;

       //===========================================================
       // SubRotina.: ChgProcSin
       // Objectivo.: Alterar um registo do Processo
       //===========================================================
       BEGSR ChgProcSin;
          MONITOR;
             D_NRPROC   = %CHAR(N_AAPROC)+'9'+%CHAR(N_NSEQ);
             CHAIN (D_NRPROC) RSINP105;
             IF (%FOUND());
                IF (N_ACORDO = 'S');
                   D_SIN10508 = -1;     // (DL832006)
                ELSE;
                   D_SIN10508 = 0;     // (DL832006)
                ENDIF;
                IF (N_DTFIM > *ZEROS);
                   D_SIN10505 = -1;  // (PROCENCERRADO)
                   D_SIN10506 = %DATE((20000000+N_DTFIM):*ISO);
                ELSE;
                   D_SIN10505 = *ZEROS;
                   %NullInd(D_SIN10506) = *ON; //
                ENDIF;
                UPDATE RSINP105 %FIELDS(D_SIN10505:D_SIN10506:
                                        D_SIN10508);
             ENDIF;
          ON-ERROR;
          ENDMON;
       ENDSR;

       //===========================================================
       // SubRotina.: FixNullValues
       // Objectivo.: Adicionar os registos do Intervenientes
       //===========================================================
       BEGSR FixNullValues;
          MONITOR;
            IF (N_DTRINI > *ZEROS);
            ENDIF;
          ON-ERROR;
             N_DTRINI = *ZEROS;
          ENDMON;
          MONITOR;
            IF (N_DTPART > *ZEROS);
            ENDIF;
          ON-ERROR;
             N_DTPART = *ZEROS;
          ENDMON;
          MONITOR;
            IF (N_DTFIM > *ZEROS);
            ENDIF;
          ON-ERROR;
             N_DTFIM = *ZEROS;
       ENDMON;
       ENDSR;
      /END-FREE
**
PARTICIPAÇÃO
DAAA CORR 2ASS
RECLAM TERC
DAAA INCORR 2ASS
PROTOCOLO QIV
OUTRAS
DAAA 1ASS (SEG)
OUTRAS
OUTRAS
