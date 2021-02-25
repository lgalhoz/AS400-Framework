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
          RETURN;
       ENDMON;

       //-------------------------------------------------------
       // Aplicar modificações ao registo em função da Operação
       //-------------------------------------------------------
       IF (TTrgEvent = IO#UPD);      // Alteração
       ELSEIF (TTrgEvent = IO#DEL);  // Eliminação
       ELSEIF (TTrgEvent = IO#INS);  // Insersão
          IF (N_ACORDO = 'S');
             MONITOR;
                //-------------------------------------------------
                // ForCompr os campos do processo de Sinistro
                //-------------------------------------------------
                D_NRPROC   = %CHAR(N_AAPROC)+'9'+%CHAR(N_NSEQ);
                D_DATAPART = %DATE((N_DTPART+20000000):*ISO);

                //-------------------------------------------------
                // ForCompr Tipo de Documento
                //-------------------------------------------------
                IF (N_FPART >= '1') AND (N_FPART <= '9');
                   D_TIPODOC = TBD(%DEC(N_FPART:1:0));
                ENDIF;
                IF (N_FPART ='2') OR (N_FPART ='4') OR
                   (N_FPART ='7');
                   D_EXISTEDAAA = 'S';
                ELSE;
                   D_EXISTEDAAA = 'N';
                ENDIF;
                IF (N_FPART = '6');
                   D_SIN10501 = 'FAX'; // (MEIOCOMUNIC)
                ELSE;
                   D_SIN10501 = 'CORREIO NORMAL';
                ENDIF;
                D_SIN10502 = 'S'; // (APLICLEIPORT)
                D_SIN10503 = 'N'; // (SUSPFRAUDE)
                D_SIN10504 = 'N'; // (FACTCLIMEXCEP)
                IF (N_DTFIM > *ZEROS);
                   D_SIN10505 = -1;  // (PROCENCERRADO)
                   D_SIN10506 = %DATE((20000000+N_DTFIM):*ISO);
                ELSE;
                   D_SIN10505 = *ZEROS; // (PROCENCERRADO)
                // D_SIN10506 = *ZEROS; // (DATAENCERPROC)
                ENDIF;
                D_SIN10508 = -1;     // (DL832006)
                D_SIN10509 = *ZEROS; // (ENVIADOS)
                D_SIN10510 = 'N';    // (ASSRESPONS)
                D_SIN10511 = *ZEROS; // (ENVIO)
                D_SIN10512 = %DATE;  // (DATALTERA)
                WRITE RSINP105;
             ON-ERROR;
             ENDMON;
          ENDIF;
       ENDIF;

       RETURN;
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
