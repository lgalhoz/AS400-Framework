     H Option(*SrcStmt : *NoDebugIO) bnddir('QC2LE')
     H dftactgrp(*no)
     H*==========================================================
     H* Aqui neste teste, tiramos partido das funções em C++
     H* disponíveis no AS400.
     H* Estas funções destinam-se a trabalhar com os ficheiro
     H* a um nível mais baixo.
     H*==========================================================
      /COPY QRPGLESRC,SGRLO016

     D READLINE        PR            10I 0
     D   FD                          10I 0 value
     D   TEXT                          *   value
     D   MAXLEN                      10I 0 value
     D
     D WResult         S              5S 0
     D WFD             S             10I 0
     D WLine           S            100A
     D WLen            S             10i 0
     D
     D WPasta          S             50A   INZ('/interfaces/ddirectos/')
     D WFileName       S             50A   INZ('Pagamentos.XML')
     D*==========================================================
      /FREE
        //-------------------------------------------------------
        // Criar Esqueleto do Ficheiro XML
        //-------------------------------------------------------
        EXSR CrtXMLSkeleton;

        *INLR = *ON;
        RETURN;

        //===============================================================
        // Subrotina.: CrtXMLSkeleton
        // Objectivo.: Criar a estrutura XML para os debitos directos.
        //===============================================================
        BEGSR CrtXMLSkeleton;
           WFD = $Open(%TRIM(WPasta)+%TRIM(WFileName)
                              : O#WRONLY + O#CREAT + O#TRUNC
                              : S#IRUSR + S#IWUSR + S#IRGRP);
           IF (WFD >= *ZEROS);

           ELSE;
             // Erros
           ENDIF;
        ENDSR;

        //===============================================================
        // Subrotina.: GravarXML
        // Objectivo.:
        //===============================================================
        BEGSR GravarXML;
           fd = $Open(%TRIM(WPasta)+%TRIM(WFileName)
                              : O#WRONLY + O#CREAT + O#TRUNC
                              : S#IRUSR + S#IWUSR + S#IRGRP);
                           // : 37);
           if (fd >= 0);
              line = 'Isto é um pequeno teste';
              len  = %LEN(%TRIM(line))+1;
              line = %TRIM(line) + x'0d25';
              $Write(fd:%addr(line):len);
              $Write(fd:%addr(line):len);
              $Write(fd:%addr(line):len);
              $Write(fd:%addr(line):len);
              $Close(fd);
           else;
              dsply 'Erro na abertura .';
           endif;
        ENDSR;
        //===============================================================
        // Subrotina.: GravarXML
        // Objectivo.:
        //===============================================================
        BEGSR GravarXML_;
           fd = $Open(%TRIM(WPasta)+%TRIM(WFileName)
                              : O#CREAT + O#EXCL + O#CCSID + O#WRONLY
                                  + O#TEXTDATA + O#TEXT_CREAT
                              : S#IRUSR + S#IWUSR
                              : 37);
           if (fd >= 0);
              len  = 19;
              line = 'Isto é um teste 01';
           // len  = %LEN(%TRIM(line))+1;
           // line = %TRIM(line) + x'0d25';
           // $Write(fd:%addr(line):len);
           // line = 'Isto é um teste 02';
           // len  = %LEN(%TRIM(line))+1;
           // line = %TRIM(line) + x'0d25';
              $Write(fd:%addr(line):len);
              $Close(fd);
           else;
              // erro
           endif;
        ENDSR;

        //===============================================================
        // Subrotina.: LerXML
        // Objectivo.:
        //===============================================================
        BEGSR LerXML;
           fd = $Open(%TRIM(WPasta)+%TRIM(WFileName):O#RDONLY+O#TEXTDATA+
                      O#CCSID:S#IRGRP:37);
           if (fd < 0);
              Msg = '$Open(): failed for reading';
              dsply Msg;
              *inlr = *on;
              return;
           endif;

           dow  ReadLine(fd: %addr(line):%size(line))>=0;
              Msg = line;
              dsply Msg;
           enddo;

           $Close(fd);
        ENDSR;
      /end-free

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
