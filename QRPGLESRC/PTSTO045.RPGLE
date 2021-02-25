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
     D   parts                             LIKEDS(parts) DIM(3)
     D
     D i               S             10i 0
     DCOUNT            C                     3
     H*============================================================
      /free
        OPTIONS = 'DOC=FILE CASE=ANY';
        xml-into records %XML('/opt/partData.xml':options);
        for i = 1 to count;
           DSPLY records.parts(i).qty;
        endfor;

        *INLR = *ON;
        return;
      /end-free
