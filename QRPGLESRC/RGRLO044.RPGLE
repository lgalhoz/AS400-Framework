      *  Demonstration of using regular expressions to validate an e-mail
      *  address.
      *                                    Scott Klement, July 13, 2006
      *
      *  To Compile:
      *      CRTBNDRPG MAILCHK SRCFILE(xxx/QRPGLESRC) DBGVIEW(*LIST)
      *
      *  To Run:
      *      CALL PGM(MAILCHK) PARM(&EMAILADDR &VALID)
      *
      *      Note: Don't call from the command line, it'll only
      *            pass 32 characters!  Call from another program,
      *            and pass a variable that's 100 chars long for
      *            the first parameter!
      *
      *  To reset program (clear variables, free compiled copy
      *      of regular expression, etc.) call with no parameters:
      *
      *      CALL PGM(MAILCHK)
      *
      *
     H DFTACTGRP(*NO) BNDDIR('QC2LE')

      /copy RGRLO042

     D RGRLO044        PR
     D   EmailAddr                  100A   const
     D   valid                        1A
     D RGRLO044        PI
     D   EmailAddr                  100A   const
     D   valid                        1A

     D FatalError      PR
     D    rc                         10I 0 value
     D    reg                              likeds(regex_t)

     D compiled        s              1N   inz(*OFF)
     D pattern         s             50A   varying
     D reg             ds                  likeds(regex_t)
     D match           ds                  likeds(regmatch_t)
     D rc              s             10I 0

      /free

        // --------------------------------------------------
        //  If called with no parameters, clean everything
        //  up and exit the program.
        // --------------------------------------------------

        if (%parms = 0);
           regfree(reg);
           compiled = *Off;
           *inlr = *on;
           return;
        endif;


        // --------------------------------------------------
        // Compile the regular expression
        //  (This is only done once, on the first call.)
        //
        //  For more info about this E-mail address expresion
        //  see:
        //    http://www.regular-expressions.info/email.html
        // --------------------------------------------------

        if (not Compiled);
            pattern = '^[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$';
            rc = regcomp( reg
                        : Pattern
                        : REG_EXTENDED + REG_ICASE + REG_NOSUB );
            if rc <> 0;
               FatalError(rc:reg);
            endif;
            compiled = *on;
        endif;


        // --------------------------------------------------
        //  Check the e-mail address against the regular
        //  expression.
        // --------------------------------------------------

        if (regexec( reg
                   : %trim(EmailAddr)
                   : 0
                   : match
                   : 0 ) = 0);
           valid = *on;
        else;
           valid = *off;
        endif;

        return;

      /end-free


      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * FatalError():  Send exception message with error from
      *                regular expression routines.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     P FatalError      B
     D FatalError      PI
     D    rc                         10I 0 value
     D    reg                              likeds(regex_t)

     D QMHSNDPM        PR                  ExtPgm('QMHSNDPM')
     D   MessageID                    7A   Const
     D   QualMsgF                    20A   Const
     D   MsgData                    512A   Const
     D   MsgDtaLen                   10I 0 Const
     D   MsgType                     10A   Const
     D   CallStkEnt                  10A   Const
     D   CallStkCnt                  10I 0 Const
     D   MessageKey                   4A
     D   ErrorCode                 8192A   options(*varsize)

     D ErrorCode       DS                  qualified
     D  BytesProv              1      4I 0 inz(0)
     D  BytesAvail             5      8I 0 inz(0)

     D MsgKey          S              4A
     D Data            s            512A   varying
     D Buf             s            512A

      /free

         regerror(rc: reg: %addr(Buf): %size(buf));
         Data = %str(%addr(buf));

         QMHSNDPM( 'CPF9897'
                 : 'QCPFMSG   *LIBL'
                 : Data
                 : %len(Data)
                 : '*ESCAPE'
                 : '*PGMBDY'
                 : 1
                 : MsgKey
                 : ErrorCode );

      /end-free
     P                 E
