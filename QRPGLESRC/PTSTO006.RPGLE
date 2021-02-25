     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
     H*==================================================================
     H* Programa..: PTESO006
     H* Descrição.: Testar janel de mensagem
     H*==================================================================
     D MsgBox          PR                  EXTPGM('QUILNGTX')
     D   text                      6800a   const varying options(*VarSize)
     D   length                      10i 0 const
     D   msgid                        7a   const
     D   qualmsgf                    20a   const
     D   errorCode                    8a   const
     D
     D ErrorNull       ds                  qualified
     D   BytesProv                   10i 0 inz(0)
     D   BytesAvail                  10i 0 inz(0)
     D
     D WMessageText    S           6800a
     D*===================================================================
      /FREE
        WMessageText = 'The Display Long Text (QUILNGTX) API displays ' +
                       'a pop-up window containing the string of text ' +
                       'that is passed to it. This API may not be used ' +
                       'to display text that is bidirectional right to left.';

        //--------------------------------------------------------------
        // Apresentar mensagem, com o cabeçalho correspondente à mensagem
        //--------------------------------------------------------------
        MsgBox(WMessageText:%Len(WMessageText):'PRD0100'
                :'MSG01POR  #JMPRDOBJ ':ErrorNull );

        //--------------------------------------------------------------
        // Apresentar a mensagem sem o cabeçalho
        //--------------------------------------------------------------
        MsgBox(WMessageText:%Len(WMessageText):*BLANKS
                :*BLANKS:ErrorNull );

        //--------------------------------------------------------------
        // Apresentar apenas o cabeçalho ou titulo
        //--------------------------------------------------------------
        MsgBox(*BLANKS:1:'PRD0100'
                :'MSG01POR  #JMPRDOBJ ':ErrorNull );

         RETURN;
      /END-FREE
