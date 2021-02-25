     H*=====================================================================
     H* Progama...: PMEDP010
     H* Objectivo.: Gerar os ficheiros de Interface para os Mediadores,
     H*             para este efeito o sistema identifica qual é o ficheiro
     H*             mais recente e em função deste procede à geração dos
     H*             Interfaces.
     H*
     H*=====================================================================
     H Option(*SrcStmt: *NoDebugIO) DftActGRP(*No)
     F*=====================================================================
     FFMEDP100  IF   E           K DISK
     FFMEDP102  IF   E           K DISK
     F*===================================================================
     D*==> Procedimento para listar ficheiros
     D listFiles       PR
     D  WNomLib                      10A   Const
     D
     D*==> Obter a data de Criação do Ficheiro
     D getObjDate      PR            14S 0
     D    WLibrary                   10A   Const
     D    WName                      10A   Const
     D    WType                      10A   Const
     D
     D*==> UserSpace
     D ReadUserSpace   PR                  ExtPgm( 'QUSRTVUS' )
     D  userspace                    20a   Const
     D  start                        10i 0 Const
     D  Length                       10i 0 Const
     D  Returned                  32767a         Options( *VarSize )
     D
     D GENDS           DS
     D  OffsetHdr                    10i 0
     D  OffsetHdr2                   10i 0
     D  NbrInList                    10i 0
     D  SizeEntry                    10i 0
     D
     D
     D Object          DS
     D  Name                   1     10A
     D  Library               11     20A
     D  Type                  21     30A
     D  Status                31     31A
     D  Attrib                32     41A
     D  Description           42     91A
     D  UserAttrib            92    101A
     D  Reserved             102    108A
     D  AspNumber            109    112B 0
     D  Owner                113    122A
     D  Domain               123    124A
     D  DatCria              125    132A
     D  DatAltera            133    140A
     D
     D*==> Declaração das Restantes Variáveis
     D Count           S              4  0
     D StartPosit      S             10i 0
     D StartLen        S             10i 0
     D UserSpace       S             20    INZ('LISTOUTQS QTEMP')
     D WNomLib         S             10                                                            
     D WNomFile        S             10A
     D*===================================================================
      /FREE
         //-----------------------------------------------
         // Seleccionar os Mediadores a quem se destinam
         // os Interfaces.
         //-----------------------------------------------
         SETLL *LOVAL RMEDP102;
         READ RMEDP102;
         DOU (%EOF());
            IF (ICSUSPEN = 'N');
               EXSR GetLastDate;  // Obter a última data
               EXSR GetFiles;
               EXSR SortFiles;

               //---------------------------------------------------
               // Processar os ficheiros e actualizar no Interface
               //---------------------------------------------------


            ENDIF;
            READ RMEDP102;
         ENDDO;

         RETURN;
         *INLR = *ON;
         //======================================================
         // Subrotina.: getLastDate
         // Objectivo.: Obter a data do Último Processamento
         //======================================================
         BEGSR getLastDate;
            listFiles('QS36F');
            StartPosit = 125;
            StartLen   = 16;
            ReadUserSpace(userspace : StartPosit : StartLen : GENDS);
            StartPosit = OffsetHdr + 1;
            StartLen = %size(Object);

            //---------------------------------------------
            // Procede à leitura dos registos
            //---------------------------------------------
            FOR count = 1 to  NbrInList;
               ReadUserSpace(userspace  : StartPosit : StartLen : Object);
               IF ((Attrib = 'PF') AND (%SUBST(Name:1:5) = 'GBDIO'));
                  WNomFile = Name;
            //    #DatConv('*DTS':DatCria:'*YMD':WDatCria:WError);
            //    #DatConv('*DTS':DatAltera:'*YMD':WDatAltera:WError);
            //    WBkData = %DEC(%SUBST(WDatCria:2:12):14:0)+20000000000000;
            //    IF (WBkData > WDatIniCont) AND
            //       ((WDatFimCont = *ZEROS) OR (WBkData < WDatFimCont));
            //       EXSR OpnFile;
            //    ENDIF;
               ENDIF;
               StartPosit += SizeEntry;
            ENDFOR;
         ENDSR;

         //======================================================
         // Subrotina.: GetFiles
         // Objectivo.: Actualizar uma lista de trabalho com os             a
         //             ficheiros do Carregamento e Continuados
         //             gerados depois da última data processada.
         //======================================================
         BEGSR GetFiles;





         ENDSR;

         //======================================================
         // Subrotina.: SortList
         // Objectivo.: Ordenar ficheiro por data crescente
         //======================================================
         BEGSR SortFiles;
         ENDSR;

            //--> Produzir Interfaces
            //--> Enviar Interfaces para o Servidor Linux

      /END-FREE
