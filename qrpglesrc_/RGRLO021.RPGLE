  ******************************************************************
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') AUT(*ALL)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H*==================================================================
     H* Descrição.: Converter um ficheiro de Spool, num ficheiro .PDF.
     H*             para o efeito o ficheiro de Spool é convertido num
     H*             ficheiro físico, e depois é convertido num PDF.
     H*==================================================================
     Fcvtwork02 IF   F  382        DISK
     Fcvtwork01 UF A F  378        DISK
     D*==================================================================
     D*--> Program parameter - report title
     D paTitle         S             50A
     D
     D*--> Program parameter - spooled file information returned by API
     D SplInfo         DS
     D  saReturned                   10I 0
     D  saAvailabl                   10I 0
     D  saIntJobId                   16A
     D  saSplfId                     16A
     D  saJobName                    10A
     D  saUser                       10A
     D  saJobNbr                      6A
     D  saSplFile                    10A
     D  saSplNbr                     10I 0
     D  saFormType                   10A
     D  saUsrDta                     10A
     D  saStatus                     10A
     D  saFilAvail                   10A
     D  saHold                       10A
     D  saSave                       10A
     D  siPages                      10I 0
     D  siCurrPage                   10I 0
     D  siFromPage                   10I 0
     D  siToPage                     10I 0
     D  siLastPage                   10I 0
     D  siRestart                    10I 0
     D  siCopies                     10I 0
     D  siCopyRem                    10I 0
     D  siLPI                        10I 0
     D  siCPI                        10I 0
     D  siOutPty                      2A
     D  saOutq                       10A
     D  saOutqLib                    10A
     D  saOpenDate                    7A
     D  saOpenTime                    6A
     D  saPrtFile                    10A
     D  saPrtfLib                    10A
     D  saPgmName                    10A
     D  saPgmLib                     10A
     D  saAcgCode                    15A
     D  saPrtTxt                     30A
     D  siRcdLen                     10I 0
     D  siMaxRcds                    10I 0
     D  saDevType                    10A
     D  saPrtType                    10A
     D  saDocName                    12A
     D  saFlrName                    64A
     D  saS36Proc                     8A
     D  saFidelity                   10A
     D  saRplUnprt                    1A
     D  saRplChar                     1A
     D  siPageLen                    10I 0
     D  siPageWdth                   10I 0
     D  siSepartrs                   10I 0
     D  siOvrFlw                     10I 0
     D  saDBCS                       10A
     D  saDBCSExt                    10A
     D  saDBCSSOSI                   10A
     D  saDBCSRotn                   10A
     D  saDBCSCPI                    10I 0
     D  saGraphics                   10A
     D  saCodePage                   10A
     D  saFormDf                     10A
     D  saFormDfLb                   10A
     D  siDrawer                     10I 0
     D  saFont                       10A
     D  saS36SplId                    6A
     D  siRotation                   10I 0
     D  siJustify                    10I 0
     D  saDuplex                     10A
     D  saFoldRcds                   10A
     D  saCtlChar                    10A
     D  saAlign                      10A
     D  saPrtQlty                    10A
     D  saFormFeed                   10A
     D  saVolumes                    71A
     D  saLabels                     17A
     D  saExchange                   10A
     D  saCharCode                   10A
     D  siTotRcds                    10I 0
     D  siMultiUp                    10I 0
     D  saFrontOvl                   10A
     D  saFrtOvlLb                   10A
     D  snFOOffDwn                   15P 5
     D  snFOOffAcr                   15P 5
     D  saBackOvl                    10A
     D  saBckOvlLb                   10A
     D  snBOOffDwn                   15P 5
     D  snBOOffAcr                   15P 5
     D  saUOM                        10A
     D  saPagDfn                     10A
     D  saPagDfnLb                   10A
     D  saSpacing                    10A
     D  snPointSiz                   15P 5
     D  snFMOffDwn                   15P 5
     D  snFMOffAcr                   15P 5
     D  snBMOffDwn                   15P 5
     D  snBMOffAcr                   15P 5
     D  snPageLen                    15P 5
     D  snPageWdth                   15P 5
     D  saMethod                     10A
     D  saAFP                         1A
     D  saChrSet                     10A
     D  saChrSetLb                   10A
     D  saCdePagNm                   10A
     D  saCdePgeLb                   10A
     D  saCdeFnt                     10A
     D  saCdeFntLb                   10A
     D  saDBCSFnt                    10A
     D  saDBCSFntL                   10A
     D  saUserDef                    10A
     D  saReduce                     10A
     D  saReserv1                     1A
     D  siOutBin                     10I 0
     D  siCCSID                      10I 0
     D  saUserText                  100A
     D  saSystem                      8A
     D  saOrigId                      8A
     D  saCreator                    10A
     D
     D*--> Program parameter - bookmark option
     D paBookmark      S              7A
     D
     D*--> Program parameter - bookmark *POS option parameters
     D BMarkPos        DS
     D   siPosCount                   5I 0
     D   snPosLine                    3P 0
     D   snPosChar                    3P 0
     D   snPosLen                     3P 0
     D
     D*--> Program parameter - bookmark *KEY option parameters
     D BMarkKey        DS
     D   siKeyCount                   5I 0
     D   siLen                        5I 0
     D   saKeyStr                   378A
     D   snKeyOccur                   3P 0
     D   snKeyOff                     3P 0
     D   snKeyLen                     3P 0
     D
     D*--> PDF 'object' array
     D aiObject        S             10I 0 DIM(32767)
     D
     D*--> Start position of PDF options
     D aaStart         S             10A   DIM(32767)
     D
     D*--> Current object number
     D wiObject        S             10I 0
     D
     D*--> Current count of bytes written
     D wiChrCount      S             10I 0
     D
     D*--> Current page number
     D wiPage          S             10I 0
     D
     D*--> Start position of text
     D wiStart         S             10I 0
     D
     D*--> Bookmark text
     D waBookmark      S            378A
     D
     D*--> Count of occurrences of the bookmark key
     D wiOccurs        S              5I 0
     D
     D*--> Input spooled file data including control characters
     D InputData       DS
     D   saSkipLine                   3A
     D   ssSkipLine                   3S 0 OVERLAY(saSkipLine:1)
     D   saSpceLine                   1A
     D   ssSpceLine                   1S 0 OVERLAY(saSpceLine:1)
     D   saInput                    378A
     D
     D*--> Output PDF-format data
     D OutputData      DS
     D   saOutput                   378A
     D
     D*--> Procedure prototypes
     D WritePDF        PR
     D   iaOutput                   378A   CONST OPTIONS(*VARSIZE)
     D AddEscape       PR           378A
     D   iaInput                    378A
     D PDFHeader       PR
     D PDFPages        PR
     D PDFTrailer      PR
     D NewPage         PR
     D EndPage         PR
     D NumToText       PR            10A
     D    iiNum                      10I 0 CONST
     D NewObject       PR
     D*==================================================================
     C*--> Program parameters
     C     *ENTRY        PLIST
     C                   PARM                    paTitle
     C                   PARM                    SplInfo
     C                   PARM                    paBookmark
     C                   PARM                    BMarkPos
     C                   PARM                    BMarkKey
     C
      /FREE
         PDFHeader();  // Output a PDF header
         PDFPages();   // Create PDF page 'objects'
         PDFTrailer(); // Output a PDF trailer

         *INLR = *ON;
         RETURN;
      /END-FREE
     P**********************************************************************
     P* Processo..: PDFHeader
     P* Descrição.: Procedure to create a PDF 'header'
     P*
     P**********************************************************************
     P PDFHeader       B
     D PDFHeader       PI
     D liPage          S             10I 0
     D liPageObj       S             10I 0
     C*=====================================================================
      /FREE
        //-------------------------------------
        // Create catalog object
        //-------------------------------------
        WritePDF('%PDF-1.0');
        WritePDF('%âãÏÓ');
        NewObject();
        WritePDF(%trim(NumToText(wiObject))+' 0 obj');
        WritePDF('<<');
        WritePDF('/Type /Catalog');
        WritePDF('/Pages 5 0 R');
        WritePDF('/Outlines 2 0 R');
        WritePDF('/PageMode /UseOutlines');
        WritePDF('>>');
        WritePDF('endobj');

        //-------------------------------------
        // Create outlines object
        //-------------------------------------
        NewObject();
        WritePDF(%trim(NumToText(wiObject))+' 0 obj');
        WritePDF('<<');
        WritePDF('/Type /Outlines');
        WritePDF('/Count '+%trim(NumToText(siPages)));
        WritePDF('/First 9 0 R');
        WritePDF('/Last  '+%trim(NumToText((siPages*4)+5))+ ' 0 R');
        WritePDF('>>');
        WritePDF('endobj');

        //-------------------------------------
        // Create procedures object
        //-------------------------------------
        NewObject();
        WritePDF(%trim(NumToText(wiObject))+' 0 obj');
        WritePDF('[/PDF /Text]');
        WritePDF('endobj');

        //-------------------------------------
        // Create fonts object
        //-------------------------------------
        NewObject();
        WritePDF(%trim(NumToText(wiObject))+' 0 obj');
        WritePDF('<<');
        WritePDF ('/Type /Font');
        WritePDF ('/Subtype /Type1');
        WritePDF ('/Name /F1');
        WritePDF ('/BaseFont /Courier');
        WritePDF ('/Encoding /WinAnsiEncoding');
        WritePDF ('>>');
        WritePDF ('endobj');

        //-------------------------------------
        // Create pages object
        //-------------------------------------
        NewObject();
        WritePDF(%trim(NumToText(wiObject))+' 0 obj');
        WritePDF ('<<');
        WritePDF ('/Type /Pages');
        WritePDF('/Count '+%trim(NumToText(siPages)));

        //-------------------------------------
        // Write list of child pages
        //-------------------------------------
        liPage    = wiObject + 1;
        liPageObj = liPage;
        WritePDF('/Kids ['+%trim(NumToText(liPage))+' 0 R');
        DOW (liPage < siPages + wiObject);
           liPage = liPage + 1;
           liPageObj = liPageObj + 4;
           WritePDF('       '+%trim(NumToText(liPageObj))+' 0 R');
        ENDDO;
        WritePDF ('       ]');
        WritePDF ('>>');
        WritePDF ('endobj');
      /FREE
     P PDFHeader       E

     P**********************************************************************
     P* Processo..: PDFPages
     P* Descrição.: Procedure to create PDF pages
     P*
     P**********************************************************************
     P PDFPages        B
     D liLine          S             10I 0
     D liLength        S              5I 0
     D liChar          S              5I 0
     D liX             S              5I 0
     D liY             S              5I 0
     C*=====================================================================
      /FREE
        //-------------------------------------
        // Create page object for first page
        //-------------------------------------
        wiPage = 0;
        liX = 0;

        //-------------------------------------
        // Read spooled file data from input work file
        //-------------------------------------
        READ  cvtwork02  InputData;
        DOW (NOT %EOF());
           //-------------------------------------
           // Skip to a line if specified, handling
           // page throw if it occurs
           //-------------------------------------
           IF (saSkipLine <> *BLANKS);
              IF ((ssSkipLine < liLine) or (liLine = 0));
                 IF (wiPage <> 0);
                    EndPage();
                 ENDIF;
                 NewPage();
                 liLine = ssSkipLine;
                 liY   = (612/siPageLen) * (siPagelen-liLine);
              ELSE;
                 liY = -((612/siPageLen) * (ssSkipLine-liLine));
                 liLine = ssSkipLine;
              ENDIF;
           ENDIF;

           //-------------------------------------
           // Space a number of lines if specified
           //-------------------------------------
           IF (saSpceLine <> *BLANKS);
              liLine = liLine + ssSpceLine;
              liY  = -((612/siPageLen) * ssSpceLine);
           ENDIF;

           //-------------------------------------
           // Set up bookmark if position option specified
           //-------------------------------------
           IF (paBookmark = '*POS');
              IF ((liLine = snPosLine) and (waBookmark = *BLANKS));
                 waBookmark = %trim(%subst(saInput:snPosChar:snPosLen));
              ENDIF;
           ENDIF;

           //-------------------------------------
           // Set up bookmark if key option specified
           //-------------------------------------
           IF (paBookmark = '*KEY');
              liChar = %SCAN(saKeyStr:siLenSCAN:1);
              //    saKeyStr:siLenSCAN      saInput:1     liChar
              IF (liChar > *ZEROS);
                 wiOccurs = wiOccurs + 1;
                 IF (wiOccurs = snKeyOccur);
                    liChar = liChar + snKeyOff;
                    liLength = snKeyLen;
                    IF (liChar + liLength > siPageWdth);
                       liLength = siPageWdth - liChar;
                    ENDIF;
                    IF (liChar < 1);
                       liChar = 1;
                    ENDIF;
                    IF (liChar + liLength <= siPageWdth);
                       waBookmark = %trim(%subst(saInput:liChar:
                                    liLength));
                    ENDIF;
                 ENDIF;
              ENDIF;
           ENDIF;

           //-------------------------------------
           // Add escape character before special
           // characters \, ( and )
           //-------------------------------------
           saInput = AddEscape(saInput);

           //-------------------------------------
           // Output the line of text
           //-------------------------------------
           WritePDF(%trim(NumToText(liX))+ ' '+%trim(NumToText(liY))+
                    ' Td ('+%trimr(saInput)+') Tj');
           READ cvtwork02 InputData;
        ENDDO;
        EndPage;
      /END-FREE
     P PDFPages        E

     P**********************************************************************
     P* Processo..: PDFTrailer
     P* Descrição.: Procedure to create a PDF trailer
     P*
     P**********************************************************************
     P PDFTrailer      B
     D PDFTrailer      PI
     D laDateTime      S             14A
     D i               S             10I 0
     D liXRef          S             10I 0
     C*=====================================================================
      /FREE
        //-------------------------------------
        // Create information object
        //-------------------------------------
        NewObject();
        WritePDF(%trim(NumToText(wiObject))+' 0 obj');
        WritePDF('<<');
        WritePDF('/Creator ('+%trim(saPgmLib)+'/'+%trim(saPgmName)+')');
        IF (%subst(saOpenDate:1:1) = '0');
           laDateTime = '19'+%subst(saOpenDate:2:6)+saOpenTime;
        ELSE;
           laDateTime = '20'+%subst(saOpenDate:2:6)+saOpenTime;
        ENDIF;
        WritePDF('/CreationDate (D:'+laDateTime + ')');
        WritePDF('/Title ('+%trim(paTitle)+')');
        WritePDF('/Producer (CVTSPLPDF)');
        WritePDF('/Keywords ()');
        WritePDF('/Author ('+%trim(saJobNbr)+'/'+%trim(saUser)+'/'
                            +%trim(saJobName)+')');
        WritePDF('>>');
        WritePDF('endobj');

        //-------------------------------------
        // Create cross-reference
        //-------------------------------------
        liXref = wiChrCount - 1;
        WritePDF('xref 0 '+%trim(NumToText(wiObject+1)));
        WritePDF('0000000000 65535 f');
        i = *ZEROS;
        DO (i <= wiObject);
           i += 1;
           WritePDF(aaStart(i)+' 00000 n');
        ENDDO;

        //-------------------------------------
        // Write trailer
        //-------------------------------------
        WritePDF('trailer');
        WritePDF('<<');
        WritePDF('/Size '+%trim(NumToText(wiObject+1)));
        WritePDF('/Root 1 0 R');
        WritePDF('/Info '+%trim(NumToText(wiObject))+' 0 R');
        WritePDF('>>');
        WritePDF('startxref');
        WritePDF(%trim(NumToText(liXref)));
        WritePDF('%%EOF');
      /END-FREE
     P PDFTrailer      E

     P**********************************************************************
     P* Processo..: NewObject
     P* Descrição.: Procedure to create a new PDF 'object'
     P*
     P**********************************************************************
     P NewObject       B
     D NewObject       PI
     D lsDataLen       S             10S 0
     D i               S             10I 0
      /FREE
        wiObject = wiObject + 1;
        i = wiObject;
        lsDataLen = wiChrCount;
        aaStart(i) = lsDataLen;
      /END-FREE
     P NewObject       E

     P**********************************************************************
     P* Processo..: WritePDF
     P* Descrição.: Procedure to output PDF data
     P*
     P**********************************************************************
     P WritePDF        B
     D WritePDF        PI
     D   iaOutput                   378A   CONST OPTIONS(*VARSIZE)
     D liLength        S              5I 0
     C*==================================================================
      /FREE
        //-------------------------------------
        // Update byte count with length of data to be written
        //-------------------------------------
        //    ' '           CHECKR    iaOutput      liLength
        liLength = %CHECKR(' ':iaOutput);
        wiChrCount= wiChrCount + liLength + 2;

        //-------------------------------------
        // Output data to work file
        //-------------------------------------
        saOutput = %trimr(iaOutput);
        WRITE  cvtwork01  OutputData;
      /END-FREE
     P WritePDF        E

     P**********************************************************************
     P* Processo..: NumToText
     P* Descrição.: Procedure to convert a number to text
     P*
     P**********************************************************************
     P NumToText       B
     D NumToText       PI            10A
     D    iiNum                      10I 0 CONST
     D laSign          S              1A
     D laInput         S             10A
     D laOutput        S             10A
     D liIn            S              5I 0
     D liOut           S              5I 0
     D liNum           S             10I 0
     C*=================================================================
      /FREE
        //-------------------------------------
        // Set up sign if and make number positive
        // if number is negative
        //-------------------------------------
        IF (iiNum < 0);
           laSign = '-';
           liNum = -iiNum;
        ELSE;
           laSign = ' ';
           liNum = iiNum;
        ENDIF;

        //-------------------------------------
        // Number number to work character variable
        //-------------------------------------
        laInput = liNum;

        //-------------------------------------
        // Skip over leading zeros
        //-------------------------------------
        liIn  = 1;
        liOut = 1;
        DOW ((liIn < %size(laInput)) and (%subst(laInput:liIn:1)='0'));
            liIn = liIn + 1;
        ENDDO;

        //-------------------------------------
        // Move digits to output area
        //-------------------------------------
        DOW (liIn<=%size(laInput) and liOut <= %size(laOutput));
            %subst(laOutput:liOut:1) = %subst(laInput :liIn :1);
            liIn  += 1;
            liOut += 1;
        ENDDO;

        //-------------------------------------
        // Add sign
        //-------------------------------------
        IF (laSign = '-');
           laOutput = laSign + laOutput;
        ENDIF;

        //-------------------------------------
        // Return number in text format
        //-------------------------------------
        RETURN    laOutput;
      /END-FREE
     P NumToText       E

     P**********************************************************************
     P* Processo..: AddEscape
     P* Descrição.: Procedure to add an escape character before special
     P*             characters
     P**********************************************************************
     P AddEscape       B
     D AddEscape       PI           378A
     D   iaInput                    378A
     D laOutput        S            378A
     D laChar          S              1A
     D i               S              5I 0
     D o               S              5I 0
     D liLength        S              5I 0
     C*====================================================================
        //-------------------------------------
        // Determine length of input data
        //-------------------------------------
        // ' '           CHECKR    iaInput       liLength
        liLength = %CHECKR(' ':iaInput);

        //-------------------------------------
        // Work through input data and prefix
        // special characters with escape
        //-------------------------------------
        i = 1;
        o = 0;
        DOW (i <= liLength);
           laChar = %subst(iaInput:i:1);
           IF (laChar = '\' or laChar = '(' or laChar = ')');
              o = o + 1;
              %subst(laOutput:o:1) = '\';
           ENDIF;
           o = o + 1;
           %subst(laOutput:o:1) = laChar;
           i = i + 1;
        ENDDO;

        RETURN laOutput;
      /END-FREE
     P AddEscape       E

     P**********************************************************************
     P* Processo..: AddEscape
     P* Descrição.: Procedure to create a new page object
     P*
     P**********************************************************************
     P NewPage         B
     D NewPage         PI
     C*=====================================================================
      /FREE
        //-------------------------------------
        // Create a page object
        //-------------------------------------
        wiPage = wiPage + 1;
        NewObject();
        WritePDF(%trim(NumToText(wiObject))+' 0 obj');
        WritePDF('<<');
        WritePDF('/Type /Page');
        WritePDF('/Parent 5 0 R');
        WritePDF('/Resources << /Font <<'+' /F1 4 0 R >>'
                 +' /ProcSet 3 0 R >>');
        WritePDF('/MediaBox [0 0 792 612]');
        WritePDF('/Contents '+%trim(NumToText(wiObject+1))+' 0 R');
        WritePDF('>>');
        WritePDF('endobj');

        //-------------------------------------
        // Set up bookmark if *PAGNBR option specified
        //-------------------------------------
        IF (paBookmark = '*PAGNBR');
           waBookmark = 'Page '+%trim(NumToText(wiPage));
        ELSE;
           waBookmark = *BLANKS;
           wiOccurs   = 0;
        ENDIF;

        //-------------------------------------
        // Create a stream object
        //-------------------------------------
        NewObject();
        WritePDF(%trim(NumToText(wiObject))+' 0 obj');
        WritePDF('<< /Length '+%trim(NumToText(wiObject+1))+' 0 R >>');
        WritePDF('stream');
        wiStart = wiChrCount;
        WritePDF('BT');

        //-------------------------------------
        // Determine font size to use from
        // Characters per inch setting
        //-------------------------------------
        SELECT;
        WHEN (siCPI = 50);
           WritePDF('/F1 20 Tf');
        WHEN (siCPI = 120);
           WritePDF('/F1 9 Tf');
        WHEN (siCPI = 150);
           WritePDF('/F1 8 Tf');
        WHEN (siCPI = 167);
           WritePDF('/F1 6 Tf');
        OTHER;
           WritePDF('/F1 10 Tf');
        ENDSL;
      /END-FREE
     P NewPage         E

     P**********************************************************************
     P* Processo..: EndPage
     P* Descrição.: Procedure to finish a page object
     P*
     P**********************************************************************
     P EndPage         B
     D EndPage         PI
     D liLength        S             10I 0
     C*=====================================================================
      /FREE
        //-------------------------------------
        // End text stream
        //-------------------------------------
        WritePDF('ET');
        liLength = wiChrCount - wiStart;
        WritePDF('endstream');
        WritePDF('endobj');

        //-------------------------------------
        // Create indirect length object for stream
        //-------------------------------------
        NewObject();
        WritePDF(%trim(NumToText(wiObject))+' 0 obj');
        WritePDF(%trim(NumToText(liLength)));
        WritePDF('endobj');

        //-------------------------------------
        // Create outline object
        //-------------------------------------
        waBookmark = AddEscape(waBookMark);
        NewObject();
        WritePDF(%trim(NumToText(wiObject))+' 0 obj');
        WritePDF('<<');
        WritePDF('/Parent 2 0 R');
        WritePDF('/Title  ('+%trimr(waBookmark) + ')');
        IF (wiPage > 1);
           WritePDF('/Prev '+%trim(NumToText(wiObject-4))+' 0 R');
        ENDIF;
        IF (wiPage < siPages);
           WritePDF('/Next '+%trim(NumToText(wiObject+4))+' 0 R');
        ENDIF;
        WritePDF('/Dest ['+%trim(NumToText(wiObject-3))
                   +' 0 R /XYZ 0 792 0]');
        WritePDF('>>');
        WritePDF('endobj');
      /END-FREE
     P EndPage         E
