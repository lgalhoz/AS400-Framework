     H ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('JMDIR')
     H DECEDIT(',') DFTNAME(PCVSO001) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
     H*======================================================================================
     H* Autor .....: Luis Galhoz
     H* Data ......: 2013/07/17
     H* Programa ..: PCVSO001
     H* Objectivo .: Actualizar nova Estrutura a partir dos Ficheiros Temporários e Bases.
     H*
     H* Observação.: Programa alterado para passar a correr em dois modos, a conversão para
     H*              uma Apólice a conversão para todas as Apólices. A necessidade destes
     H*              dois modos, deve-se ao facto de numa primeira tentativa para o modo
     H*              Conversão de todas as Apólices, ter corrido mal. Desta forma vamos correr
     H*              a pedido para as Apólices, até a função estar totalmente corrigida, nessa
     H*              altura corre-se para todas as Apólices.
     H*======================================================================================
     FFPRDT001  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FBASEAPOL  IF   F 1000        DISK    USROPN
     FFGRLP02303UF   E           K DISK    PREFIX(TMP_)
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO020
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura de dados do Registo
     D RBASE         E DS                  ExtName(GBASEDIO)
     D                                     Prefix(BAS_)
     D
     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D
     D*==> Actualizar a nova estrutura
     D UpdNewDB        PR                  EXTPGM('PCVSO002')
     D   CdRamo                       2S 0 CONST
     D   DatCriacao                   8S 0 CONST
     D   Registo                       *   CONST
     D
     D*==> Eliminar informação de Histórico
     D DelNewDB        PR                  EXTPGM('PCVSO004')
     D   CdRamo                       2S 0 CONST
     D   NuApolice                    7S 0 CONST
     D
     D*==> Parametros Recebidos
     D PCdRamo         S              2S 0
     D PNuApolice      S              7S 0
     D
     D*==> Campos de Trabalho
     D WCdProduto      S              5S 0
     D WCdRamo         S              2S 0
     D WNuApolice      S              7S 0
     D WDatCria        S              8S 0
     D WMesIni         S              2S 0
     D WMesFich        S              2S 0
     D WIcSelect       S               N
     D WIcUnica        S               N
     D WIcHistor       S              1A
     D WIdx            S              5S 0
     D WIdx2           S              5S 0
     D WOrigem         S              1A
     D
     D*==> Lista de Ficheiros Temporarios
     D WParms          DS                  LIKEDS(LST#ValField) DIM(10)
     D WFile           DS                  LIKEDS(LST#TmpFile) DIM(9999)
     D WLista          DS                  LIKEDS(LST#TmpFile) DIM(9999)
     D*========================================================
     IBASEAPOL  AA  11
     I                                  1 1000  RBASE
     I*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCdRamo
     C                   PARM                    PNuApolice
      /FREE
       //-----------------------------------------------------
       // função interdita para qualquer utilizador
       //-----------------------------------------------------
       IF (S#USR <> 'CALIMERO');
          $ShowDialog('PRD0162');
          *INLR = *ON;
          RETURN;
       ENDIF;

       //-----------------------------------------------------
       // Formatar parametros recebidos
       //-----------------------------------------------------
       WIcUnica = *OFF;
       MONITOR;
         WCdRamo = PCdRamo;
         WIcUnica = *ON;
       ON-ERROR;
         WCdRamo = *ZEROS;
       ENDMON;
       MONITOR;
         WNuApolice = PNuApolice;
         WIcUnica = *ON;
       ON-ERROR;
         WCdRamo    = *ZEROS;
         WNuapolice = *ZEROS;
       ENDMON;

       //--------------------------------------------
       // Validar se a Apólice já foi convertida
       //--------------------------------------------
       IF (WIcUnica);
          CHAIN(N) (WCdRamo:WNuApolice) RPRDP001;
          IF (NOT %FOUND());
             $ShowDialog('PRD0161');
             *INLR = *ON;
             RETURN;
          ELSE;
             IF (PRD_TPACTA = 'N');
                DelNewDB(WCdRamo:WNuApolice);
                *INLR = *ON;
                RETURN;
             ENDIF;
          ENDIF;
       ENDIF;

       //--------------------------------------------
       // Confirmar se é para proseguir
       //--------------------------------------------
       IF ($ShowDialog('PRD0163':*BLANKS:'NS')='N');
          *INLR = *ON;
          RETURN;
       ENDIF;

       //-----------------------------------------------------
       // Seleccionar todas ou apenas uma
       //-----------------------------------------------------
       EXSR FillHistorico;

       //--------------------------------------------------
       // Ler a lista de Trabalho de uma forma descendente
       //--------------------------------------------------
       WIdx = (WIdx2-1);
       IF (Widx <= 0);
         RETURN;
       ENDIF;
       DOW (WIdx > *ZEROS);
          MONITOR;
             WStdCmd = 'OVRDBF FILE(BASEAPOL) +
                            TOFILE('+%TRIM(WFile(Widx).NomLibl)+'/'+
                            %TRIM(WFile(WIdx).NomFile)+')';
             WStdLen = %LEN(WStdCmd);
             Exec(WStdCmd:WStdLen);
             WMesFich = %DEC(%SUBST(%CHAR(WFile(WIdx).AnoMes):5:2):2:0); // Carregamentos

             OPEN BASEAPOL;
             READ BASEAPOL;
             DOW (NOT %EOF(BASEAPOL));
                IF (WIcUnica AND (BAS_RAMO = WCdRamo) AND
                  (BAS_APOL = WNuApolice)) OR (NOT WIcUnica AND BAS_RAMO = 05);
                   WMesIni  = %DEC(%SUBST(
                                 %EDITW(BAS_DTINI:'0      '):4:2):2:0);
                   WDatCria = WFile(WIdx).DatCria;
                   WCdRamo  = BAS_RAMO;
                   IF (((WMesIni=WMesFich) AND (WFile(WIdx).CodFile=9))
                     OR (WFile(WIdx).CodFile <> 9)) AND (WCdRamo = 05);
                      UpdNewDB(WCdRamo:WDatCria:%ADDR(RBASE));
                   ENDIF;
                ENDIF;
                READ BASEAPOL;
             ENDDO;
             CLOSE BASEAPOL;

             WIcHistor = 'S';
             EXSR ActHistorico;
          ON-ERROR;
          ENDMON;
          WIdx -= 1;
       ENDDO;

       //--------------------------------------------
       // Confirmar se é para manter as Alterações
       //--------------------------------------------
       IF ($ShowDialog('PRD0164':*BLANKS:'NS')='S') AND WIcUnica;
          CHAIN (WCdRamo:WNuApolice) RPRDP001;
          PRD_TPACTA = 'N';
          UPDATE RPRDP001 %FIELDS(PRD_TPACTA);
          COMMIT;
       ELSE;
          ROLBK;
          $ShowDialog('PRD0165');
       ENDIF;

       RETURN;
       *INLR = *ON;

       //========================================================
       // Subrotina.: FillHistorico
       // Objectivo.: Carregar lista com ficheiros de Histórico
       //========================================================
       BEGSR FillHistorico;
          //------------------------------------
          // Formatar os Parametros
          //------------------------------------
          IF (WIcUnica);
             WParms(1).ValField = %TRIM(%EDITW(WNuApolice:'0       '));
             WParms(1).IsEmpty = *OFF;
             WParms(2).ValField = %TRIM(%EDITW(WCDRamo:'0  '));
             WParms(2).IsEmpty = *OFF;
             WParms(3).IsEmpty = *ON;
          ELSE;
             WParms(1).IsEmpty = *ON;
          ENDIF;
          WLista = $GetTmpFiles(WParms);

          //-----------------------------------------------------
          // Seleccionar Ficheiros de Continuados e Carregamentos
          //-----------------------------------------------------
          WIdx = 1;
          WIdx2 = 1;
          DOW (NOT WLista(WIdx).IsEmpty);
             WOrigem = 'X';
             IF ((WLista(WIdx).CodFile >= 21) AND
               (WLista(WIdx).CodFile <= 23));
                WOrigem = 'D';
             ELSEIF ((WLista(WIdx).CodFile >= 5) AND
               (WLista(WIdx).CodFile <= 9));
                WOrigem = 'C';
             ENDIF;

             IF ((WOrigem = 'C') OR (WOrigem = 'D'));
                WFile(WIdx2).CodFile  = WLista(WIdx).CodFile;
                WFile(WIdx2).NomLibl  = WLista(WIdx).NomLibl;
                WFile(WIdx2).NomFile  = WLista(WIdx).NomFile;
                WFile(WIdx2).AnoMes   = WLista(WIdx).AnoMes;
                WFile(WIdx2).CodLetra = WLista(WIdx).CodLetra;
                WFile(WIdx2).DatCria  = WLista(WIdx).DatCria;
                WFile(WIdx2).HorCria  = WLista(WIdx).HorCria;
                WFile(WIdx2).IcHistor = WLista(WIdx).IcHistor;
                WFile(WIdx2).IsEmpty  = *OFF;
                WIdx2 += 1;
                WFile(WIdx2).IsEmpty= *ON;
             ENDIF;
             WIdx += 1;
          ENDDO;
       ENDSR;

       //===========================================================
       // Subrotina.: ActHistorico
       // Objectivo.: Actualizar o indicador de ficheiro processado
       //===========================================================
       BEGSR ActHistorico;
          IF (WIcUnica = *OFF);
          // CHAIN (WFile(WIdx).CodFile) RGRLP023;
          // IF (%FOUND());
          //    TMP_ICHISTOR = WIcHistor;
          //    UPDATE RGRLP023 %FIELDS(TMP_ICHISTOR);
          // ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
