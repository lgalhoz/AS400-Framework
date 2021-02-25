     H NOMAIN
     H*===================================================================
     H* Processo...: $CheckObject
     H* Objectivo..: alidar a existência de um Objecto
     H*===================================================================
      /Copy QRPGLESRC,SGRLO010

     D WIcExiste       S               N

     P*===================================================================
     P*  Retrieve field location                                      *
     P*===================================================================
     P $checkObject    B                   Export
     D                 Pi              N
     D PRM_ObjNam                    10A   Const
     D PRM_ObjLib                    10A   Const
     D PRM_ObjTyp                    10A   Const Options(*NOPASS)
     D*------------------------------------------**
     D*-- Global variables:
     D*------------------------------------------**
     D WObjNam         S             10A
     D WObjLib         S             10A
     D WObjTyp         S             10A
     D*------------------------------------------**
     D*-- Api error data structure:
     D*------------------------------------------**
     D ApiError        DS
     D  AeBytPro                     10i 0 Inz( %Size( ApiError ))
     D  AeBytAvl                     10i 0 Inz
     D  AeMsgId                       7a
     D                                1a
     D  AeMsgDta                    128a
     D*-----------------------------------------**
     D*-- Object description structure OBJD0100:
     D*-----------------------------------------**
     D RoData          Ds
     D  RoBytRtn                     10i 0
     D  RoBytAvl                     10i 0
     D  RoObjNam                     10a
     D  RoObjLib                     10a
     D  RoObjTypRt                   10a
     D  RoObjLibRt                   10a
     D  RoObjASP                     10i 0
     D  RoObjOwn                     10a
     D  RoObjDmn                      2a
     D  RoObjCrtDts                  13a
     D  RoObjChgDts                  13a
     D  RoExtAtr                     10a
     D  RoTxtDsc                     50a
     D  RoSrcF                       10a
     D  RoSrcLib                     10a
     D  RoSrcMbr                     10a
     D*--------------------------------------**
     **-- Retrieve object description:
     D*--------------------------------------**
     D RtvObjD         PR                  ExtPgm( 'QUSROBJD' )
     D  RoRcvVar                  32767a         Options( *VarSize )
     D  RoRcvVarLen                  10i 0 Const
     D  RoFmtNam                      8a   Const
     D  RoObjNamQ                    20a   Const
     D  RoObjTyp                     10a   Const
     D  RoError                   32767a         Options( *VarSize )
     D WResult         S               N
     D IcExiste        C                   *ON
     D*----------------------------------------------------**
     **-- Get Web value:
     D*----------------------------------------------------**
     C                   Eval      WObjNam = PRM_ObjNam
     C                   Eval      WObjLib = PRM_ObjLib
     C                   IF        (%PARMS() > 2)
     C                   EVAL      WObjTyp = PRM_ObjTyp
     C                   ELSE
     C                   EVAL      WObjTyp = '*FILE'
     C                   ENDIF
     C
     C                   CallP     RtvObjD( RoData
     C                                    : %Size( RoData )
     C                                    : 'OBJD0100'
     C                                    : WObjNam + WObjLib
     C                                    : WObjTyp
     C                                    : ApiError
     C                                    )
     **
     C                   IF        (AeBytAvl > *Zero) And (AeMsgId = 'CPF9812')
     C                   EVAL      WResult = *OFF
     C                   ELSE
     C                   EVAL      WResult = *ON
     C                   ENDIF
     C                   RETURN    WResult
     P $CheckObject    E
