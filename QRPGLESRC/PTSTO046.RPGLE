     H DFTACTGRP(*NO)
     H*============================================================
     D options         S            100A
     D allOk           S               N
     D
     D parts           DS                  QUALIFIED
     D   qty                         20a   varying
     D   id                          20a   varying
     D   cost                        20a   varying
     D
     D records         DS                  QUALIFIED
     D   parts                             LIKEDS(parts)
     D
     D partHandler     PR            10I 0
     D   ok                            N
     D   parts                             LIKEDS(parts) DIM(10) CONST
     D   numRecs                     10U 0 VALUE
     H*============================================================
      /free
        OPTIONS = 'PATH=/opt/ DOC=FILE CASE=ANY';
        xml-into %HANDLER(partHandler : allOk)
                 %XML('/opt/partData.xml':options);

        *INLR = *ON;
        return;
      /end-free
     P partHandler     B
     D                 PI            10I 0
     D   ok                           1N
     D   parts2                            LIKEDS(parts) DIM(10) CONST
     D   numRecs                     10U 0 VALUE
     D i               S             10I 0
     D xmlRecNum       S             10I 0 STATIC INZ(0)
      /free
        for i = 1 to numRecs;
           xmlRecNum = xmlRecNum + 1;
           if %error;
             ok = *OFF;
           endif;
        endfor;
        return 0;
      /end-free
     P                 E
