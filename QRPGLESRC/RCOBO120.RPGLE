     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*==================================================================
     H* Programa..: RCOBO120
     H* Descrição.: Ler ficheiros XML, com recurso a funções RPG ILE.
     H*
     H*==================================================================
     F*TB2201M   O    E             PRINTER USROPN
     F*==================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Ficheiros XML a Processar
     D LST#Pasta       DS                  QUALIFIED DIM(500)
     D   NomFile                    140A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D WIdx            S              5S 0
     D
     D*==> Pré-visualização das Actas
     D ReadXMLFile     PR                  EXTPGM('RCOBO122')
     D   NomFile                    140A   CONST
     D   Server                       5S 0 CONST
     D   MsgError                     7A   CONST
     D
     D*==> Campos de Trabalho
     D WNomFile        S            140A
     D WMsgErro        S              7A
     D*==================================================================
     C*--> *ENTRY        PLIST
     C*-->               PARM                    WMsgErro
      /FREE

       //------------------------------------------
       // Procede à leitura do XML recebido
       //------------------------------------------
       WIdx = 1;
       WMsgErro = $InzFTPList(12);
       IF (WMsgErro = *BLANKS);
          WNomFile = $GetFtpFirst();
          DOW (WNomFile <> *BLANKS);
             LST#Pasta(WIdx).NomFile   = '/'+WNomFile;
             LST#Pasta(WIdx).IsEmpty   = *OFF;
             WIdx                     += 1;
             LST#Pasta(WIdx).IsEmpty   = *ON;
             WNomFile = $GetFtpNext();
          ENDDO;
       ENDIF;

       //---------------------------------------------------
       // Proceder à leitura de cada ficheiro XML
       //---------------------------------------------------
       WIdx = 1;
       WMsgErro = *BLANKS;
       DOW (NOT LST#Pasta(WIdx).IsEmpty);
          ReadXMLFile(LST#Pasta(Widx).NomFile:12:WMsgErro);
          IF (WMsgErro = *BLANKS);
             // Mover o ficheiro tratado
          ENDIF;
          WIdx += 1;
       ENDDO;
       *INLR = *ON;
      /END-FREE
