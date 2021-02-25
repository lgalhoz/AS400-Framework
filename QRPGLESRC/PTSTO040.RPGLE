     H OPTION(*SrcStmt : *NoDebugIO) BNDDIR('QC2LE':'JMDIR')
     H DFTACTGRP(*NO)
     H*==========================================================
     H* Aqui neste teste, tiramos partido das funções em C++
     H* disponíveis no AS400.
     H* Estas funções destinam-se a trabalhar com os ficheiro
     H* a um nível mais baixo.
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
        EXSR LoadTemplate;
        EXSR GravarXML;

        *INLR = *ON;
        RETURN;

        //===============================================================
        // Subrotina.: LoadTempalte
        // Objectivo.:
        //===============================================================
        BEGSR LoadTemplate;
           WFileName = 'TPLMsgComp.xml';

           //-----------------------------------------------------
           // Eliminar Interface se já existir
           //-----------------------------------------------------
           $UnLink(%TRIM(WPasta)+%TRIM(WFileName));

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
           WFileName = 'Teste.xml';
           WOut = $Open(%TRIM(WPasta)+%TRIM(WFileName)
                        : O#RDWR + O#APPEND
                        : S#IRUSR + S#IWUSR + S#IRGRP);
                         // : 37);
           IF (WOut >= 0);
              WLine = 'Isto é um pequeno teste';
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
     P* Descrição.:
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
