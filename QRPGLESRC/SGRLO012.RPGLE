     D*-------------------------------------------------
     D* Trigger Buffer
     D*-------------------------------------------------
     DTrgBuffer        DS
     D TFileName                     10
     D TLibName                      10
     D TMemName                      10
     D TTrgEvent                      1
     D TTrgTime                       1
     D TCommitLock                    1
     D TFiller1                       3
     D TCCSID                        10I 0
     D TRelRecNbr                    10I 0
     D TFiller2                      10I 0
     D TOldRecOff                    10I 0
     D TOldRecLen                    10I 0
     D TOldNullOff                   10I 0
     D TOldNullLen                   10I 0
     D TNewRecOff                    10I 0
     D TNewRecLen                    10I 0
     D TNewNullOff                   10I 0
     D TNewNullLen                   10I 0
     D
     D*-------------------------------------------------
     D* Trigger Buffer Length
     D*-------------------------------------------------
     DTrgBufferLen     S             10I 0
     D
     D*-------------------------------------------------
     D*  Record layout for New and Old Null Maps.
     D*-------------------------------------------------
     D OldNullMap      DS                  Based(OldNullPtr)
     D  ONullFld                      1    DIM(5)
     D NewNullMap      DS                  Based(NewNullPtr)
     D  NNullFld                      1    DIM(5)
     D
     D*==> Operação de IO (TrgBuffer.TTrgEvent)
     D IO#INS          C                   '1'
     D IO#UPD          C                   '3'
     D IO#DEL          C                   '2'
     D IO#READ         C                   '4'
     D
     D*==> Momento da Chamada da Trigger (TrgBuffer.TTrgTime)
     D IO#BEFORE       C                   '2'
     D IO#AFTER        C                   '1'
     D IO#NULL         C                   '3'
     D
     D*==> Commit Lock Level (TrgBuffer.TCommitLock)
     D CMT#NONE        C                   '0'
     D CMT#CHG         C                   '1'
     D CMT#CS          C                   '2'
     D CMT#ALL         C                   '3'
     D
     D*==> Obter o programa chamador
     D $getAppCaller   PR            10A
     D    AppName                    10A   Const Options(*NOPASS)
