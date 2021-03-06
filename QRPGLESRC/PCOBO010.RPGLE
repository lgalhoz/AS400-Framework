     H OPTION(*SrcStmt : *NoDebugIO) BNDDIR('QC2LE':'JMDIR')
     H DFTACTGRP(*NO)
     H*==========================================================
     H* Esta rotina � desencadeada pelo actual programa D�bitos
     H* Directos, e passa a gerar o Interface XML, substituindo
     H* desta forma o actual fotrmato em TXT.
     H*==========================================================
      /COPY QRPGLESRC,SGRLO016
      /COPY QRPGLESRC,SGRLO010

     D*==> Ler uma linha de Texto
     D READLINE        PR            10I 0
     D   FD                          10I 0 value
     D   TEXT                          *   value
     D   MAXLEN                      10I 0 value
     D
     D*==> Campos de Trabalho
     D WInp            S             10I 0
     D WOut            S             10I 0
     D WMsgError       S             50A
     D WText           S            100A
     D WLine           S            100A
     D WLen            S             10i 0
     D
     D WPasta          S             50A   INZ('/interfaces/ddirectos/')
     D WFileName       S             50A
     D*==========================================================
      /FREE
        EXSR MakeXMLMessage;

        *INLR = *ON;
        RETURN;

        //===============================================================
        // Subrotina.: MakeXMLMessage
        // Objectivo.:
        //===============================================================
        BEGSR MakeXMLMessage;
           EXSR LoadTemplates;
        ENDSR;

        //===============================================================
        // Subrotina.: LoadTemplates
        // Objectivo.:
        //===============================================================
        BEGSR LoadTemplates;
           //-----------------------------------------------------
           // Recriar o Ficheiro de Interface
           //-----------------------------------------------------
           WINP = $Open(%TRIM(WPasta)+%TRIM(WFileName):O#RDONLY+O#TEXTDATA+
                      O#CCSID:S#IRGRP:37);
           IF (WINP < *ZEROS);
              WMsgError = '$Open(): failed for reading';
              DSPLY WMsgError;
              *INLR = *ON;
              RETURN;
           ENDIF;

           DOW  READLINE(WINP: %ADDR(WLine):%size(Wline))>=0;
              WText = WLine;
              DSPLY %SUBST(WText:1:52);
           ENDDO;

           $CLOSE(WINP);
        ENDSR;

        //===============================================================
        // Subrotina.: GravarXML
        // Objectivo.:
        //===============================================================
        BEGSR GravarXML;
           WFileName = 'TPLMsgComp.xml';

           //-----------------------------------------------------
           // Eliminar Interface se j� existir
           //-----------------------------------------------------
           $UnLink(%TRIM(WPasta)+%TRIM(WFileName));

           WFileName = 'Teste.xml';
           WOut = $Open(%TRIM(WPasta)+%TRIM(WFileName)
                        : O#RDWR + O#APPEND
                        : S#IRUSR + S#IWUSR + S#IRGRP);
                         // : 37);
           IF (WOut >= 0);
              WLine = 'Isto � um pequeno teste';
              WLen  = %LEN(%TRIM(WLine))+1;
              WLine = %TRIM(WLine) + x'0d25';
              $Write(WOut:%addr(WLine):WLen);
              $Write(WOut:%addr(WLine):WLen);
              $Write(WOut:%addr(WLine):WLen);
              $Write(WOut:%addr(WLine):WLen);
              $Close(WOut);
           ELSE;
              DSPLY 'Erro na abertura .';
           ENDIF;
        ENDSR;
      /END-FREE

     P*==========================================================
     P* Processo..: ReadLine
     P* Descri��o.:
     P*==========================================================
     P readline        B
     D readline        PI            10I 0
     D   fd                          10I 0 value
     D   text                          *   value
     D   maxlen                      10I 0 value
     D
     D rdbuf           S           1024A   static
     D rdpos           S             10I 0 static
     D rdlen           S             10I 0 static
     D
     D p_retstr        S               *
     D RetStr          S          32766A   based(p_retstr)
     D len             S             10I 0
     D*=========================================================
      /FREE
         len = 0;
         p_retstr = text;
         %subst(RetStr:1:MaxLen) = *blanks;

         dow (1 = 1);
            //--------------------------------
            // Load the buffer
            //--------------------------------
            if (rdpos>=rdlen);
               rdpos = 0;
               rdlen=$Read(fd:%addr(rdbuf):%size(rdbuf));
               if (rdlen < 1);
                  return  -1;
               endif;
            endif;

            //--------------------------------
            // Is this the end of the line?
            //--------------------------------
            rdpos = rdpos + 1;
            if (%subst(rdbuf:rdpos:1) = x'25');
              return len;
            endif;

            //--------------------------------
            // Otherwise, add it to the text string.
            //--------------------------------
            if (%subst(rdbuf:rdpos:1) <> x'0d') and len<>maxlen;
               len = len + 1;
               %subst(retstr:len:1) = %subst(rdbuf:rdpos:1);
            endif;
         enddo;
         return len;
      /END-FREE
     P                 E
