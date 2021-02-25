     H DFTACTGRP(*NO)
     H*============================================================
     D options         S            100A
     DStation          ds
     D woNumber                      20a   varying
     D stationId                     20a   varying
     D unitCode                      20a   varying
     D unitActn                      20a   varying
     D unitLength                    20a   varying
     D quantity                      20a   varying
     DCOUNT            C                     5
     DI                S             10I 0
     H*============================================================
      /free
        OPTIONS = 'DOC=FILE CASE=ANY';
        xml-into station %XML('/opt/XMLTEST.xml':options);
        for i = 1 to count;
           DSPLY woNumber;
        endfor;

        *INLR = *ON;
        return;
      /end-free
