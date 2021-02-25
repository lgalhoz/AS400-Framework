     D Obj$Data        DS                  QUALIFIED
     D  Name                         10A
     D  Library                      10A
     D  Type                         10A
     D  Attrib                       10A
     D  Descript                     50A
     D  DatCria                       8S 0
     D  HorCria                       6S 0
     D  DatAltera                     8S 0
     D  HorAltera                     6S 0
     D
     D P$Object        DS
     D  P$Name                 1     10A
     D  P$Library             11     20A
     D  P$Type                21     30A
     D  P$Status              31     31A
     D  P$Attrib              32     41A
     D  P$Description         42     91A
     D  P$UserAttrib          92    101A
     D  P$Reserved           102    108A
     D  P$AspNumber          109    112B 0
     D  P$Owner              113    122A
     D  P$Domain             123    124A
     D  P$DatCria            125    132A
     D  P$DatAltera          133    140A
     D
     D*==> Criar lista de ficheiros
     D $crtListFile    PR
     D  WNomLib                      10A   Const
     D
     D*==> Obter a quantidade de Ficheiros existente
     D $getQtdeFiles   PR             5S 0
     D  WNomLib                      10A   Const
     D
     D*==> Obter o Ficheiro
     D $getFileInfo    PR                  Like(Obj$Data)
     D  WRecNbr                       5S 0 Const
     D
     D*==> Obter a data de criação do Objecto
     D $getObject      PR                  Like(Obj$Data)
     D    WLibrary                   10A   Const
     D    WName                      10A   Const
     D    WType                      10A   Const
     D
     D $ReadUserSpace  PR                  ExtPgm( 'QUSRTVUS' )
     D  userspace                    20a   Const
     D  start                        10i 0 Const
     D  Length                       10i 0 Const
     D  Returned                  32767a         Options( *VarSize )
     D
     D GENDS           DS
     D  OffsetHdr                    10i 0
     D  OffsetHdr2                   10i 0
     D  NbrInList                    10i 0
     D  SizeEntry                    10i 0
