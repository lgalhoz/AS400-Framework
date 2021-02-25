     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*==================================================================
     H* Programa..: RCOBO110
     H* Descrição.: Gerar Ficheiro XML com os registos
     H* Observação: Criar na QTEMP o ficheiro FCOBT232 antes de compilar
     H*   CRTPF FILE(QTEMP/FCOBT232) SRCFILE(#JMPRDSRC/QDDSSRC)
     H*==================================================================
     FFCOBT232  UF A E           K DISK    USROPN EXTFILE('QTEMP/XMLFILE')
     F                                     PREFIX(OUT_)
     FTBIDDEF   IF   E           K DISK    PREFIX(INP_)
     FTBIDDFF   IF   E           K DISK    PREFIX(INP_)
     FTBIDDPF   UF   E           K DISK    PREFIX(IDD_)
     F*==================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Comando para Criar o Ficheiro
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Campos de Trabalho
     D WNomFile        S             20A
     D PCodFile        S             10P 0
     D PMsgError       S              7A
     D WCodFile        S             10S 0
     D WTpFile         S              3A
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCodFile
     C                   PARM                    PMsgError
      /FREE

       WCodFile = PCodFile;
       CHAIN (WCodFile) TBIDDE;
       IF (%FOUND(TBIDDEF));
          WTpFile = INP_TETIPF;
          CHAIN (WCodFile) TBIDDF;
          IF (%FOUND(TBIDDFF));
             EXSR MakeXML;
          ENDIF;
       ENDIF;
       *INLR = *ON;

       //===============================================================
       // Subrotina.: MakeXML
       // Descrição.: Criar o ficheiro XML
       //===============================================================
       BEGSR MakeXML;
          EXSR CrtXMLFile;
          SETLL (WCodFile) TBIDDF;
          READE (WCodFile) TBIDDF;
          DOW (NOT %EOF(TBIDDFF));
             OUT_TXTXML = INP_TFTEXT;
             WRITE RCOBT232;
             READE (WCodFile) TBIDDF;
          ENDDO;
          EXSR CrtXMLFinal;
       ENDSR;

       //===============================================================
       // Subrotina.: CrtXMLFile
       // Descrição.: Criar o Ficheiro XML
       //===============================================================
       BEGSR CrtXMLFile;
          MONITOR;
             WStdCmd = 'DLTF FILE(QTEMP/XMLFILE)';
             WStdLen = %LEN(WStdCmd);
             shell(WStdCmd:WStdLen);
          ON-ERROR;
          ENDMON;

          MONITOR;
             WStdCmd = 'CRTPF FILE(QTEMP/XMLFILE) SRCFILE(#JMPRDSRC/QDDSSRC) +
                        SRCMBR(FCOBT232)';
             WStdLen = %LEN(WStdCmd);
             shell(WStdCmd:WStdLen);
          ON-ERROR;
             *INLR = *ON;
             RETURN;
          ENDMON;
          OPEN FCOBT232;
       ENDSR;

       //===============================================================
       // Subrotina.: CrtXMLFinal
       // Descrição.: Criar a informação de resumo no ficheiro XML
       // Observação: A Template 6 corresponde ao servidor de testes
       //===============================================================
       BEGSR CrtXMLFinal;
          WNomFile = 'IDD'+%Char(WCodFile)+'.'+%TRIM(WTpFile);
          IF ($SendFTP(4:'QTEMP':'XMLFILE':WNomFile) <> *BLANKS);
             *INLR = *ON;
             RETURN;
          ENDIF;
          CLOSE FCOBT232;
       ENDSR;

      /END-FREE
