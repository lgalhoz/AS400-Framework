     H*BNDDIR('JMDIR')
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
      *=======================================================
      * Dat.Cria...: 2012-10-23
      * Objectivo..: Devolver o Header da função, junto com as
      *              teclas a que o utilizador está autorizado.
      *=======================================================
      */Copy QRPGLESRC,SSECO010
     D $MakeKey        PR            43A
     D  Parm001                      10A   CONST
     D  Parm002                      10A   CONST Options(*NOPASS)
     D  Parm003                      10A   CONST Options(*NOPASS)
     D  Parm004                      10A   CONST Options(*NOPASS)
     D
     D $IsPending      PR              N
     D  Chave                        43A   CONST
     D
     D $CrtAudit       PR
     D  Chave                        43A   CONST
     D
     D $AddParms       PR
     D  Chave                        43A   CONST
     D  Campo                        25A   CONST
     D  Valor                        50A   CONST
     D*=======================================================
      /FREE

         $MakeKey('1':'A':'*ON');
         $MakeKey('1':'*OFF');
         $MakeKey('X');


         RETURN;
      /END-FREE
     C*=======================================================
     C* Processo..: $MakeKey
     C* Objectivo.: Criar Chave associada ao Registo
     C*=======================================================
     P $MakeKey        B                   EXPORT
     D                 PI            43A
     D  Parm001                      10A   CONST
     D  Parm002                      10A   CONST Options(*NOPASS)
     D  Parm003                      10A   CONST Options(*NOPASS)
     D  Parm004                      10A   CONST Options(*NOPASS)
     D  WResult        S             10A
     D*================================================================
      /FREE
          IF (%PARMS() > 3);
             WResult = %TRIM(Parm001)+'|'+
                       %TRIM(Parm002)+'|'+
                       %TRIM(Parm003)+'|'+
                       %TRIM(Parm004);
          ELSEIF (%PARMS() > 2);
             WResult = %TRIM(Parm001)+'|'+
                       %TRIM(Parm002)+'|'+
                       %TRIM(Parm003);
          ELSEIF (%PARMS() > 1);
             WResult = %TRIM(Parm001)+'|'+
                       %TRIM(Parm002);
          ELSE;
             WResult = %TRIM(Parm001);
          ENDIF;
          RETURN WResult;
      /END-FREE
     P $MakeKey        E

     C*=======================================================
     C* Processo..: $IsPending
     C* Objectivo.: Validar se o registo está a aguardar ou
     C*             se existe autorização para continuar com
     C*             a operação.
     C*=======================================================
     P $IsPending      B                   EXPORT
     D                 PI              N
     D  Chave                        43A   CONST
     D  WResult        S             10A
     D*================================================================
      /FREE
          RETURN *ON;
      /END-FREE
     P $IsPending      E

     C*=======================================================
     C* Processo..: $CrtAudit
     C* Objectivo.: Criar um registo de Auditoria
     C*=======================================================
     P $CrtAudit       B                   EXPORT
     D                 PI
     D  Chave                        43A   CONST
     D  WResult        S             10A
     D*================================================================
      /FREE

      /END-FREE
     P $CrtAudit       E

     C*=======================================================
     C* Processo..: $AddParms
     C* Objectivo.: Adicionar Parametros
     C*=======================================================
     P $AddParms       B                   EXPORT
     D                 PI
     D  Chave                        43A   CONST
     D  Campo                        25A   CONST
     D  Valor                        50A   CONST
     D  WResult        S             10A
     D*================================================================
      /FREE

      /END-FREE
     P $AddParms       E
