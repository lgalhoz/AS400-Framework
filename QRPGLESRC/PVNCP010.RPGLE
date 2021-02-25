     H*===========================================================
     H* Objectivo..: Realizar o planeamento dos recibos com
     H*              desconto em Vencimento.
     H*              Para o efeito serve-se dos meses correntes dos
     H*              Continuados, junto com a previsão para os
     H*              meses seguintes.
     H*===========================================================
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP('JMGRP')
     H*===========================================================
     FFVNCP010  UF A E           K DISK
     FFVNCE010  CF   E             WORKSTN SFILE(RVNCE01002:REC#)
     FFVNCE010  CF   E             WORKSTN SFILE(RVNCE01032:REC#)
     F*===========================================================
     F* Indicadores  Descrição
     F* -----------  ---------------------------------------------
     F*      03      Tecla F3
     F*      06      Tecla F6
     F*      12      Tecla F12
     F*      30      FTBLP010
     F*      50      Tecla PagDn
     F*      51      Tecla PagUp
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     F*===========================================================
     D PSDS           SDS
     D   S#PGM                 1     10
     D   S#JOB               244    253
     D   S#USR               254    263
     D   S#JOB#              264    269  0
     D
     D REC#            S              4  0
     D WIdx            S              5  0
     D WBotPage        S               N   INZ(*OFF)
     D WTopPage        S               N   INZ(*OFF)
     D WSelKey         S               N   INZ(*OFF)
     D
     D*==> Obter a descrição para o Header e Footer
     D getHeader       PR            50A
     D   AppName                     10A   CONST
     D   Function                    10A   CONST
     D
     D getFooter       PR           156A
     D   AppName                     10A   CONST
     D   Function                    10A   CONST
     D
     D*==> Validar se está autorizado
     D ValAccess       PR              N
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D  InVal                         5S 0 CONST
     D
     D*==> Obter a descrição para o Código
     D getDescricao    PR            40A
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D
     D*==> Pop-up de valores possíveis
     D runPopUp        PR            15A
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D
     D WRetCode        S              5S 0
     D WRow            S              3S 0
     D WCol            S              3S 0
     D*===========================================================
     C
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;



       //------------------------------------
       // Inicializar as variáveis de Trabalho
       //------------------------------------
       WSelKey  = *Off;
       WBotPage = *Off;
       WTopPage = *Off;

       //------------------------------------
       // Escrever Header e Footer Standard
       //------------------------------------
       DOU       (*In12 = *on) OR (*In03 = *on);
          WOperacao = getHeader(S#PGM:'RSECE01003');
          WRITE     RSECE01000;
          FNCKEY01  = getFooter(S#PGM:'RSECE01003');
          WRITE     RSECE01099;
          EXSR      InzRecords;
          EXSR      GetRecords; // Carregar Lista com Registos
          EXFMT     RSECE01003;
          IF (*IN50);
             EXSR CtlPagDnKey;
          ELSEIF (*IN51);
             EXSR CtlPagUpKey;
          ELSEIF (*IN06) AND ValAccess(S#PGM:'RSECE01003':6);
             EXSR addRecord;
          ELSE;
             EXSR selRecords;
          ENDIF;
       ENDDO;

       *InLR = *ON;

       //==============================================================
       // Subrotina.: dspRecord
       // Objectivo.: Mostrar o registos
       //==============================================================
       BEGSR dspRecord;
         DOW (NOT *IN12 AND NOT *IN03);
            CHAIN CDUSER RSECP020;
            IF (NOT %EOF);
               WOperacao = getHeader(S#PGM:'RSECE01005');
               WRITE     RSECE01000;
               FNCKEY01  = getFooter(S#PGM:'RSECE01005');
               WRITE RSECE01099;
               WDsDepart  = getDescricao(5:DEPART);
               WDsNivel   = getDescricao(6:CDNIVEL);
               EXFMT RSECE01005;
               IF (*IN21) AND ValAccess(S#PGM:'RSECE01005':21);
                  EXSR chgRecord;
               ELSEIF (*IN22) AND ValAccess(S#PGM:'RSECE01005':22);
                  EXSR delRecord;
               ENDIF;
            ENDIF;
         ENDDO;
         *IN12 = *OFF;
       ENDSR;

       //==============================================================
       // Subrotina.: chgRecord
       // Objectivo.: Alterar os dados do Registo
       //==============================================================
       BEGSR chgRecord;
         CHAIN CDUSER RSECP020;
         IF (NOT %EOF);
            WNomUser  = NOMUSER;
            WGrupo    = GRUPO;
            WDepart   = DEPART;
            WCdNivel  = CDNIVEL;
            DOW (NOT *IN12) AND (NOT *IN03);
               WOperacao = getHeader(S#PGM:'RSECE01021');
               WRITE     RSECE01000;
               FNCKEY01  = getFooter(S#PGM:'RSECE01021');
               WRITE     RSECE01099;
               EXFMT RSECE01021;
               IF (*IN04);
                  WRow = CROW;
                  WCol = CCOL;
                  EXSR PopUp;
                  ROW  = WRow;
                  COL  = WCol;
                  ITER;
               ELSEIF (*IN03 OR *IN12);
                  *IN12 = *OFF;
                  LEAVE;
               ENDIF;
               NOMUSER  = WNomUser;
               GRUPO    = WGrupo;
               DEPART   = WDepart;
               CDNIVEL  = WCdNivel;
               UPDATE RSECP020;
               LEAVE;
            ENDDO;
         ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: addRecord
       // Objectivo.: Adicionar um novo registo
       //==============================================================
       BEGSR addRecord;
          DOW (NOT *IN03) AND (NOT *IN12);
             WOperacao = getHeader(S#PGM:'RSECE01006');
             WRITE     RSECE01000;
             FNCKEY01  = getFooter(S#PGM:'RSECE01006');
             WRITE     RSECE01099;
             EXFMT RSECE01006;
             IF (*IN03) OR (*IN12);
                *IN12 = *OFF;
                LEAVE;
             ELSEIF (*IN04);
                WRow = CROW;
                WCol = CCOL;
                EXSR PopUp;
                ROW  = WRow;
                COL  = WCol;
                ITER;
             ENDIF;
             CDUSER   = WCdUser;
             NOMUSER  = WNomUser;
             GRUPO    = WGrupo;
             DEPART   = WDepart;
             CDNIVEL  = WCdNivel;
             WRITE RSECP020;
             LEAVE;
          ENDDO;
       ENDSR;

       //==============================================================
       // Subrotina.: popup
       // Objectivo.: Afixar uma lista de valoers possíveis
       //==============================================================
       BEGSR popup;
          //----------------------------------------------
          // Validar qual o campo onde foi seleccionado
          //----------------------------------------------
          IF (#FIELD = 'WDEPART');
              WDepart = runPopUp(5:WDepart);
          ELSEIF (#FIELD = 'WCDNIVEL');
              WCdNivel = runPopUp(6:WCdNivel);
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: delRecord
       // Objectivo.: Alterar os dados do Registo
       //==============================================================
       BEGSR delRecord;
          WOption = 'N';
          EXFMT RSECE01022;
          IF (*IN12);
             *IN12 = *OFF;
             WOption = 'N';
          ENDIF;

          IF (WOption = 'S');
             CHAIN CDUSER RSECP020;
             DELETE RSECP020;
             *IN12 = *ON;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: InzRecords
       // Objectivo.: Inicializar a lista de registos
       //==============================================================
       BEGSR inzRecords;
          REC# = 0;         // Controlo de Registos da Lista de Ecrã
          *In53 = *On;      // Limpar Lista de Ecrã
          *In52 = *On;      // Mostrar a Lista de Ecrã
          WRITE RSECE01003; // Limpar Lista
          *In53 = *Off;     // Repor Indicador
          *In54 = *Off;     // Repor Indicador
       ENDSR;

       //==============================================================
       // Subrotina.: Posicionar
       // Objectivo.: Posicionar no registo a seleccionar
       //==============================================================
       BEGSR Posicionar;
          IF (NOT WSelKey);
             SETLL *LOVAL RSECP020;
          ELSE;
             SETLL WCdUSer RSECP020;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: ctlPagUpKey
       // Objectivo.: Controlo da Tecla de Page Up
       //==============================================================
       BEGSR ctlPagUpKey;
            EVAL REC# = 0;
            dou (REC# = 24);
               readp RSECP020;
               if (not %EOF);
                  WTopPage = *ON;
                  leave;
               else;
                  REC# += 1;
                  WBotPage = *OFF;
               endif;
            enddo;

            // Reordenar a listagem
            if (REC# < 24);
                EVAL WSelKey = *OFF;
                EXSR Posicionar;
            endif;
            EXSR ctlPagDnKey;
       ENDSR;

       //==============================================================
       // Subrotina.: ctlPagDnKey
       // Objectivo.: controlo da Tecla Page Down
       //==============================================================
       BEGSR ctlPagDnKey;
          IF (NOT WBotPage);
             EXSR InzRecords;
             DOW (REC# < 12);
                read RSECP020;
                IF (NOT %EOF);
                   REC# += 1;
                   WBotPage = *Off;
                   WSelect = ' ';
                   WRITE RSECE01002;
                ELSE;
                   WBotPage = *On;
                   LEAVE;
                ENDIF;
             ENDDO;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: SelRecords
       // Objectivo.: Seleccionar os registos
       //==============================================================
       BEGSR selRecords;
          WIdx = 0;
          READC RSECE01002;
          DOW (NOT %EOF);
             IF (WSelect = '1');
                EXSR dspRecord;
                WSelect = ' ';
                LEAVE;
             ELSE;
                WSelect = ' ';
             ENDIF;
             READC RSECE01002;
          ENDDO;
       ENDSR;

       //==============================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponíveis
       //==============================================================
       BEGSR getRecords;
            setgt *loval RSECP020;
            dou  (REC# = 12);
               REC# += 1;
               read RSECP020;
               if (not %EOF);
                  WRITE RSECE01002;
               else;
                  leave;
               endif;
            enddo;
       ENDSR;
      /END-FREE
