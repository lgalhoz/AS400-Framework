     H*===================================================================
     H* Rotina....: RCATO013
     H* Objectivo.: Obter a informação do Objecto.
     H*
     H* Parametros Recebidos:
     H*             NomLib  = Biblioteca
     H*             NomObje = Objecto
     H*             Tipo    = Tipo de Objecto
     H*
     H* Parametros Devolvidos:
     H*             DatCria = Data de Criação do Objecto
     H*
     H*===================================================================
     H Option(*SrcStmt: *NoDebugIO) DftActGRP(*No)
     D*===================================================================
     D*---- Estrutura do Erro
     D ApiError        ds
     D   BytPro                      10i 0 inz(%size(ApiError))
     D   BytAval                     10i 0 inz
     D   MsgID                        7A
     D                                1A
     D   MsgData                    128A
     D
     D*---- Estrutura retornada
     D Object          ds
     D  BytRetn                      10i 0
     D  BytAvail                     10i 0
     D  ObjName                      10A
     D  ObjLibrary                   10A
     D  ObjType                      10A
     D  ReturnLibrary                10A
     D  ASPNumber                    10i 0
     D  Owner                        10A
     D  Domain                        2A
     D  DatCria                      13A
     D  DatAltera                    13A
     D
     D*-- Listar os Objectos existentes na Biblioteca -----------------
     D $RtvObjDesc     PR                  ExtPgm('QUSROBJD')
     D  Object                    32767a   OPTIONS(*VARSIZE)
     D  Lenght                       10i 0 Const
     D  Format                        8a   Const
     D  LibAndObj                    20a   Const
     D  Type                         10A   Const
     D  ApiError                  32767A   OPTIONS(*VARSIZE)
     D
     D WFormat         S              8A   INZ('OBJD0100')
     D WLibrary        S             10A
     D WName           S             10A
     D WType           S             10A
     D WObjectLib      S             20A
     D WRetLenght      S             10i 0
     D WDatCria        S             14S 0
     C*====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WLibrary
     C                   PARM                    WName
     C                   PARM                    WType
     C                   PARM                    WDatCria
     C
      /FREE

         MONITOR;
            %SUBST(WObjectLib:1:10) =  WName;
            %SUBST(WObjectLib:11:10) =  WLibrary;

            //---------------------------------------------
            // Obter Informação sobre o Objecto
            //---------------------------------------------
            MONITOR;
               $RtvObjDesc( Object : %SIZE(Object) : WFormat :
                            WObjectLib : WType : ApiError );

               IF (MsgId = *BLANKS);
                   WDatCria = %DEC(%SUBST(DatCria:2:12):14:0)+20000000000000;
               ENDIF;
            ON-ERROR;
            ENDMON;
         ON-ERROR;
         ENDMON;
         *INLR = *On;
      /END-FREE
