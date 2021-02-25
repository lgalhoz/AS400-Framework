     H DFTACTGRP(*NO)
      //===================================================================
      // DDS for "MYFILE"
      // A          R PARTREC
      // A            ID            10P 0
      // A            QTY           10P 0
      // A            COST           7P 2
      //
      // XML data in "partData.xml"
      //  <parts>
      //   <part><qty>100</qty><id>13</id><cost>12.03</cost></part>
      //   <part><qty>9</qty><id>14</id><cost>3.50</cost></part>
      //   ...
      //   <part><qty>0</qty><id>254</id><cost>1.98</cost></part>
      //  </records>
      //===================================================================
     FXMLTESTF  UF A E             DISK
     D options         S            100A
     D allOk           S               N

     D partHandler     PR            10I 0
     D   ok                            N
     D   parts                             LIKEREC(partrec:*OUTPUT)
     D                                     DIM(10) CONST
     D   numRecs                     10U 0 VALUE

      /free
        // Initiating the parsing
        OPTIONS = 'DOC=FILE CASE=ANY';
        allOk = *ON;
        xml-into %HANDLER(partHandler : allOk)
                 %XML('/opt/partData.xml':options);
        // Check if the operation wrote the data
        // successfully
        if not allOk;
           // some output error occurred
        endif;

        *INLR = *ON;
        return;
      /end-free


      //===================================================================
      // The procedure to receive the data from up to 10
      // XML elements at a time.  The first call to the
      // this procedure would be passed the following data
      // in the "parts" parameter:
      //   parts(1) .id = 13   .qty = 100  .cost = 12.03
      //   parts(2) .id = 14   .qty = 9    .cost = 3.50
      //   ...
      // If there were more than 10 "part" child elements in
      // the XML file, this procedure would be called more
      // than once.
      //===================================================================
     P partHandler     B
     D                 PI            10I 0
     D   ok                           1N
     D   parts                             LIKEREC(partrec:*OUTPUT)
     D                                     DIM(10) CONST
     D   numRecs                     10U 0 VALUE

     D i               S             10I 0
      * xmlRecNum is a static variable, so it will hold its
      * value across calls to this procedure.
      * Note: Another way of storing this information would be to
      * pass it as part of the first parameter; in that
      * case the first parameter would be a data structure
      * with two subfields: ok and xmlRecNum

     D xmlRecNum       S             10I 0 STATIC INZ(0)
      /free
        for i = 1 to numRecs;
           xmlRecNum = xmlRecNum + 1;
           WRITE(E) PARTREC PARTS(I);
           // Parameter "ok" was passed as the second parameter
           // for the %HANDLER built-in function for the XML-INTO
           // operation.  The procedure doing the XML-INTO
           // operation can check this after the operation to
           // see if all the data was written successfully.
           if %error;
             // log information about the error
             // logOutputError (xmlRecNum : parts(i));
             ok = *OFF;
           endif;
        endfor;

        // continue parsing
        return 0;
      /end-free
     P                 E
