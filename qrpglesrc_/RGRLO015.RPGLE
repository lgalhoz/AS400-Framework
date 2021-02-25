     H NOMAIN
     H*================================================================
      /Copy QRPGLESRC,SGRLO014

     D*==> Declaração de Variáveis
     D Count           S              5  0
     D StartPosit      S             10i 0
     D StartLen        S             10i 0
     D StartSize       S             10i 0
     D UserSpace       S             20    INZ('LISTOUTQS QTEMP')
     D
     P*=======================================================
     P* Processo..: $getQtdeFiles
     P* Objectivo.: Obter a quantidade de ficheiros existentes
     P*             num Biblioteca.
     P*
     P*=======================================================
     P $getQtdeFiles   B                   EXPORT
     D $getQtdeFiles   PI             5S 0
     D    WNomLib                    10A   Const
     D
     D*=======================================================
      /FREE

         $crtListFile(WNomLib);
         StartPosit = 125;
         StartLen   = 16;
         $ReadUserSpace(userspace : StartPosit : StartLen : GENDS);
         StartPosit = OffsetHdr + 1;
         StartLen = %size(P$Object);
         StartSize = SizeEntry;
         Count = NbrInList;

         RETURN Count;
      /END-FREE
     P $getQtdeFiles   E

     P*=======================================================
     P* Processo..: $getFileInfo
     P* Objectivo.: Obter a Informação do Ficheiro
     P*
     P*=======================================================
     P $getFileInfo    B                   EXPORT
     D $getFileInfo    PI                  Like(Obj$Data)
     D  WRecNbr                       5S 0 Const
     D
     D #DatConv        PR                  EXTPGM('QWCCVTDT')
     D   InpFormat                   10A   CONST
     D   InVariavel                  20A   CONST OPTIONS(*VARSIZE)
     D   OutFormat                   10A   CONST
     D   OutVariavel                 20A   CONST OPTIONS(*VARSIZE)
     D   ErroCode                    20A   CONST OPTIONS(*VARSIZE)
     D
     D WPosInicio      S             10i 0
     D WDatCria        S             16A
     D WDatAltera      S             16A
     D WError          S             20A
     D*=======================================================
      /FREE
         WPosInicio = StartPosit + (StartSize) * (WRecNbr - 1);
         $ReadUserSpace(userspace  : WPosInicio : StartLen : P$Object);

         Obj$Data.Name     = P$Name;
         Obj$Data.Library  = P$Library;
         Obj$Data.Type     = P$Type;
         Obj$Data.Attrib   = P$Attrib;
         Obj$Data.Descript = P$Description;

         //---------------------------------------------------
         // Converter Data de Criação e Alteração do Objecto
         //---------------------------------------------------
         #DatConv('*DTS':P$DatCria:'*YMD':WDatCria:WError);
         #DatConv('*DTS':P$DatAltera:'*YMD':WDatAltera:WError);
         Obj$Data.DatCria   = %DEC('20' + %SUBST(WDatCria:2:6):8:0);
         Obj$Data.HorCria   = %DEC(%SUBST(WDatCria:8:6):6:0);
         Obj$Data.DatAltera = %DEC('20' + %SUBST(WDatAltera:2:6):8:0);
         Obj$Data.HorAltera = %DEC(%SUBST(WDatAltera:8:6):6:0);

         RETURN Obj$Data;
      /END-FREE
     P $getFileInfo    E

     P*=======================================================
     P* Processo..: $crtListFile
     P* Objectivo.: Listar os objectos existentes numa biblioteca e gravar
     P*             o resultado numa área existente na QTEMP.
     P*
     P* Observação: O resultado é criado numa User Space localizada
     P*             na QTEMP.
     P*=======================================================
     P $crtListFile    B                   EXPORT
     D $crtListFile    PI
     D    WNomLib                    10A   Const
     D
     D*
     D Format          s              8
     D ObjectLib       s             20
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
     P $crtListFile    E

     P*===================================================================
     P* Processo..: getObject
     P* Objectivo.: Obter a informação Sobre o Objecto
     P*
     P* Parametros Recebidos:
     P*             NomLib  = Biblioteca
     P*             NomObje = Objecto
     P*             Tipo    = Tipo de Objecto
     P*===================================================================
     P $getObject      B                   EXPORT
     D $getObject      PI                  Like(Obj$Data)
     D    WLibrary                   10A   Const
     D    WName                      10A   Const
     D    WType                      10A   Const
     D*
     D*---- Estrutura do Erro
     D ApiError        ds
     D   BytPro                      10i 0 inz(%size(ApiError))
     D   BytAval                     10i 0 inz
     D   MsgID                        7A
     D                                1A
     D   MsgData                    128A
     D
     D*---- Estrutura retornada
     D P$Object        ds
     D  BytRetn                      10i 0
     D  BytAvail                     10i 0
     D  ObjName                      10A
     D  ObjLibrary                   10A
     D  ObjType                      10A
     D  ReturnLibrary                10A
     D  ASPNumber                    10i 0
     D  Owner                        10A
     D  Domain                        2A
     D  WDatCria                     13A
     D  WDatAltera                   13A
     D
     D*-- Listar os Objectos existentes na Biblioteca -----------------
     D $RtvObjDesc     PR                  ExtPgm('QUSROBJD')
     D  P$Object                  32767a   OPTIONS(*VARSIZE)
     D  Lenght                       10i 0 Const
     D  Format                        8a   Const
     D  LibAndObj                    20a   Const
     D  Type                         10A   Const
     D  ApiError                  32767A   OPTIONS(*VARSIZE)
     D
     D WFormat         S              8A   INZ('OBJD0100')
     D WObjectLib      S             20A
     D WRetLenght      S             10i 0
     C*====================================================================
      /FREE

         MONITOR;
            %SUBST(WObjectLib:1:10)  =  WName;
            %SUBST(WObjectLib:11:10) =  WLibrary;

            //---------------------------------------------
            // Obter Informação sobre o Objecto
            //---------------------------------------------
            MONITOR;
               $RtvObjDesc( P$Object : %SIZE(P$Object) : WFormat :
                            WObjectLib : WType : ApiError );

               IF (MsgId = *BLANKS);
                  Obj$Data.Name = ObjName;
                  Obj$Data.Library = ObjLibrary;
                  Obj$Data.Type    = ObjType;
                  Obj$Data.Attrib  = *BLANKS;
                  Obj$Data.Descript = *BLANKS;

                  //---------------------------------------------------
                  // Converter Data de Criação e Alteração do Objecto
                  //---------------------------------------------------
                  Obj$Data.DatCria   = %DEC('20' + %SUBST(WDatCria:2:6):8:0);
                  Obj$Data.HorCria   = %DEC(%SUBST(WDatCria:8:6):6:0);
                  Obj$Data.DatAltera = %DEC('20' + %SUBST(WDatAltera:2:6):8:0);
                  Obj$Data.HorAltera = %DEC(%SUBST(WDatAltera:8:6):6:0);
               ENDIF;
            ON-ERROR;
            ENDMON;
         ON-ERROR;
         ENDMON;
         RETURN Obj$Data;
      /END-FREE
     P $getObject      E
