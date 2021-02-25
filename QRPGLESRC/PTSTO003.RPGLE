     H ALWNULL(*USRCTL) BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*=======================================================
     H* Dat.Cria...: 2013-04-11
     H* Objectivo..: Grupo de Funções para gerir as Funções Batch
     H*=======================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*=======================================================
      /FREE
         //-------------------------------
         // Validar se pode executar
         //-------------------------------
         S#PGM = 'PINTO100';
         IF ($ValBatch(S#PGM:'INTER'));
            //-------------------------------
            // Adicionar Ficheiros de Input
            //-------------------------------
            $AddBatchInp(5981:1:5);
            $AddBatchInp(5981:2:5);
            $AddBatchInp(5992:1:5);
            $AddBatchInp(5992:2:5);

            //-------------------------------
            // Validar se pode executar
            //-------------------------------
            $AddBatchOut('LFAPOL':3:'Y');

            //-------------------------------
            // Actualizar Estado
            //-------------------------------
            $chgBatchStat();
         ENDIF;

         *INLR = *ON;
         RETURN;
      /END-FREE
