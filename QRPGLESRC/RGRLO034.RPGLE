     D QDBRTVFD        PR                  EXTPGM('QDBRTVFD')
     D   Receiver                 65535
     D   receiver_len                10I 0 const
     D   actual_file                 20
     D   Format                       8    CONST
     D   search_file                 20    CONST
     D   format_name                 10    CONST
     D   Override                     1    CONST
     D   System_Input                10    CONST
     D   Format_type                 10    CONST
     D   ErrorCode                         LIKE(error_code)
     D
     D QDDFMT          DS
     D   WDDFTEXT                    50A   OVERLAY(QDDFMT:93)
     D   NUMFIELD                    10I 0 OVERLAY(QDDFMT:143)
     D
     D Receiver        S          65535
     D Len             S             10I 0
     D Actual_File     S             20
     D
     D Error_Code      DS
     D   Provided                    10I 0 INZ(%size(Error_Code ))
     D   Available                   10I 0
     D   Msgid                        7
     D   Msgdta                     240
     D
      /FREE

        Receiver = *Loval;
        len = %Len(receiver);

        // reclaim all data form a file
        QDBRTVFD (receiver:Len:Actual_File:'FILD0200':'FPRDP101  #JMPRDDBS '
                  :'*FIRST':'0':'*LCL':'*INT':Error_Code);
        QDDFMT = Receiver;
        DUMP;

        *InLr = *On ;
       /END-FREE
