     H*===================================================================
     H* Rotina....: RCATO012
     H* Objectivo.: Listar os objectos existentes numa biblioteca e gravar
     H*             o resultado numa área existente na QTEMP.
     H*
     H*===================================================================
     H Option(*SrcStmt: *NoDebugIO) DftActGRP(*No)
     D*===================================================================
     D Format          s              8
     D ObjectLib       s             20
     D WNomLib         S             10
     D UserSpace       s             20    inz('LISTOUTQS QTEMP')
     D SpaceVal        s              1    inz(*BLANKS)
     D SpaceAuth       s             10    inz('*CHANGE')
     D SpaceText       s             50    inz(*BLANKS)
     D SpaceRepl       s             10    inz('*YES')
     D SpaceAttr       s             10    inz(*BLANKS)
     D Worktype        s             10    inz('*FILE')
     D                                                                                             
     D*
     D* API Error Data Structure
     D ErrorDs         DS                  INZ
     D  BytesPrv                     10i 0
     D  BytesAvl                     10i 0
     D  MessageId                    07
     D  ERR###                       01
     D  MessageDta                   99
     D
     D*---- Estrutura a gravar na QTEMP
     D Object          ds
     D  Name                   1     10A
     D  Library               11     20A
     D  Type                  21     30A
     D  Status                31     31A
     D  Attrib                32     41A
     D  Description           42     91A
     D  UserAttrib            92    101A
     D  Reserved             102    108A
     D  AspNumber            109    112B 0
     D  Owner                113    122A
     D  Domain               123    124A
     D  DatCria              125    132A
     D  DatAltera            133    140A
     D
     D*-- Listar os Objectos existentes na Biblioteca -----------------
     D $ListObjects    Pr                  ExtPgm( 'QUSLOBJ' )
     D  userspace                    20a   Const
     D  format                        8a   Const
     D  objectlib                    20a   Const
     D  type                         10a   Const
     D
     D*-- Criar espaço na Qtemp para conter o resultado  ------------
     D $CreateSpace    Pr                  ExtPgm( 'QUSCRTUS' )
     D  UserSpaceOut                 20a   Const
     D  SpaceAttr                    10    Const
     D  SpaceLen                     10i 0 Const
     D  SpaceVal                      1a   Const
     D  SpaceAuth                    10a   Const
     D  SpaceText                    50a   Const
     D  SpaceRepl                    10a   Const
     D  ErrorDs                   32767a         Options( *VarSize )
     C*====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNomLib
     C
     C
      /FREE

         //---------------------------------------------
         // Create the user space
         //---------------------------------------------
         BytesPrv = 116;
         $CreateSpace( UserSpace : SpaceAttr : 4096 :
                       SpaceVal : SpaceAuth : SpaceText : SpaceRepl:
                       ErrorDs);

         MONITOR;
            ObjectLib =  '*ALL      ' + WNomLib;

            //---------------------------------------------
            // Listar todos os objectos existentes
            //---------------------------------------------
            Format = 'OBJL0300';
            MONITOR;
               $ListObjects( Userspace : Format : ObjectLib : WorkType);
            ON-ERROR;
            ENDMON;
         ON-ERROR;
         ENDMON;
         RETURN;
      /END-FREE
