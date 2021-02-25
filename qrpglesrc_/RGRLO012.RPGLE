     H NOMAIN
     H*===================================================================
     H* Processo...: getFldLoc
     H* Objectivo..: Obter as coordenadas de um Campo
     H* Descrição..: Usar esta função para reposicionar o cursor no campo
     H*              onde foi detectado o erro.
     H*              Esta função deve ser usada em conjunto com a opção
     H*              P-Field usada nos ecrãs para mudar a cor do campo.
     H*
     H* Parametros.:
     H*        <nome Ficheiro Ecrã>    Recebe    10 A
     H*        <Nome Formato Registo>  Recebe    10 A
     H*        <Nome do Campo>         Recebe    10 A
     H*        <Linha>                 Devolve    3 P 0
     H*        <Coluna>                Devolve    3 P 0
     H*
     H* 1.   Exemplo:
     H* 1.1. Declaração:
     H*         D  GetFldLoc   PR       N
     H*         D      FileName       10A   Const
     H*         D      RecName        10A   Const
     H*         D      FieldName      10A   Const
     H*         D      Row             3P 0 Const
     H*         D      Col             3P 0 Const
     H*
     H* 1.2. Utilização:
     H*         Resultado = GetFldLoc('FSECE010':'RSECE01005':'GRUPO':
     H*                                   WRow:WCol):
     H*
     H*
     H*===================================================================
     H**********************************************************************
      /Copy QRPGLESRC,SGRLO010

     D QNamTyp         S             20A
     D NamTyp          S             10A
     D ChrTyp          S              1A
     D LglTyp          S               N
     D BufTyp          S          32767A
     D IntTyp          S             10I 0
     D SmlIntTyp       S              5I 0
     D
     D  WRow           S              3P 0
     D  WCol           S              3P 0
     D
     D*==================================================================

     P*===================================================================
     P*  Retrieve field location                                      *
     P*===================================================================
     P $getFldLoc      B                   Export
     D                 Pi                  Like( LglTyp  )
     D  QDspFil                            Like( NamTyp ) Const
     D  RcdFmt                             Like( NamTyp  ) Const
     D  FldNam                             Like( NamTyp  ) Const
     D  Row                           3P 0
     D  Col                           3P 0
     D RcvVar          S                   Like( BufTyp    )
     D RcvVarLen       S                   Like( IntTyp    )
     D                                     Inz( %Size( RcvVar ) )
     D Idx             S                   Like( IntTyp    )
     D SDFFINFO        S                   Like( IntTyp    )
     D LDFARFTE        S                   Like( IntTyp    )
     D SDFARFTE        S                   Like( IntTyp    )
     D SDFFRDPD        S                   Like( IntTyp    )
     D SDFFRCTE        S                   Like( IntTyp    )
     D SDFFXRDP        S                   Like( IntTyp    )
     D SDFFRINF        S                   Like( IntTyp    )
     D SDFFNTB         S                   Like( IntTyp    )
     D True            C                   *On
     D False           C                   *Off
     D FMTAR           S             16    Dim( 999 )
     D FMTNM           S             10    Dim( 999 )
     D FLDNM           S             10    Dim( 999 )
     D FLDRC           S              2    Dim( 999 )
     D
     D ApiErrDs        ds
     D   BytesProv                   10I 0 inz(0)
     D   BytesAvail                  10I 0 inz(0)
     D
     D                 Ds
     D RC12                                Like( SmlIntTyp )
     D  RC1                                Like( ChrTyp    )
     D                                     Overlay( RC12:1 )
     D  RC2                                Like( ChrTyp    )
     D                                     Overlay( RC12:2 )
     D*  Base file section ( QDFFBASE )
     DQDFFBASE         Ds
     D QDFFRETN                1      4B 0
     D QDFFSIZE                5      8B 0
     D QDFFINOF                9     10B 0
     D QDFFRCS                11     12B 0
     D  QDFBITS               13     13
     D QDFFFSCR               14     15B 0
     D QDFFSRSQ               16     17B 0
     D QDFCCSID               18     19B 0
     D QDFFSCRS               20     20
     D*  File header section ( QDFFINFO )
     DQDFFINFO         Ds
     D QDFFDFLO                1      4B 0
     D QDFFWUOF                5      8B 0
     D QDFFFMWU                9     12B 0
     D QDFFSEQO               13     16B 0
     D QDFFFSFL               17     18B 0
     D QDFFFSCE               19     20B 0
     D  QDFBITS01             21     22
     D QDFRVED7               23     34
     D QDFFXDOF               35     36B 0
      *  Record format table ( QDFARFTE )
     DQDFARFTE         Ds
     D QDFARFNM                1     10
     D  QDFBITS08             11     12
     D QDFARFOF               13     16B 0
      *  Record header section ( QDFFRINF )
     DQDFFRINF         Ds
     D QDFFRDDO                1      4B 0
     D QDFFOFIT                5      8B 0
     D QDFFSTBO                9     12B 0
     D  QDFBITS09             13     16
     D QDFFFLD                17     18B 0
     D QDFVED21               19     22
     D QDFFINDO               23     24B 0
     D QDFVED22               25     28
     D QDFFRAOF               29     30B 0
     D*  Display-Record-Level-Device-Dependent section ( QDFFRDPD )
     DQDFFRDPD         Ds
     D QDFFDRCO                1      4B 0
     D QDFFINCP                5      6B 0
     D QDFFFIBF                7      8B 0
     D QDFFFOIS                9     10B 0
     D QDFVED23               11     12
     D   QDFBITS11            14     14
     D   QDFBITS12            15     15
     D   QDFBITS13            16     16
     D  QDFBITS14             17     18
     D  QDFBITS15             19     20
     D QDFFERRM               21     22B 0
     D  QDFBITS16             23     23
     D  QDFBITS17             24     24
     D QDFFCGRI               25     26B 0
     D  QDFBITS18             27     28
     D QDFFXRDO               29     30B 0
     D QDFVED32               31     32
     D QDFFRDOC               33     34B 0
      *  Display-Record-Level-Device-Dependent section
      *  extension structure ( QDFFXRDP )
     DQDFFXRDP         Ds
     D QDFFFOTO                1      4B 0
     D QDFFNRCO                5      8B 0
     D QDFVED33                9     12
     D QDFFSEGS               13     14B 0
     D QDFVED34               15     16
     D QDFHCTLO               17     18B 0
     D QDFVED35               19     24
      *  Row-Column table entry ( QDFFRCTE )
     DQDFFRCTE         Ds
     D QDFFSROW                1      1
     D QDFFSCOL                2      2
     D WIcErro         S               N
     D
     D*==> Formatar a biblioteca
     D  FILLIB         DS
     D   WFilLib                           LIKE( QNamTyp )
     D    WFILE                1     10A
     D    WLIB                11     20A
     C*==============================================================
     C
     C                   IF        (QDspFil = *BLANKS) OR
     C                             (RcdFmt = *BLANKS) OR
     C                             (FldNam = *BLANKS)
     C                   RETURN    *OFF
     C                   ENDIF
     C
     C                   EVAL      WIcErro = *OFF
     C                   EVAL      WLib  = '*LIBL'
     C                   EVAL      WFile = QDspFil
     C                   Eval      Row   = 0
     C                   Eval      Col   = 0
      *  Retrieve display file description ( QDFRTVFD ) API
+----C                   MONITOR
|    C                   Call      'QDFRTVFD'
|    C                   Parm                    RcvVar
|    C                   Parm                    RcvVarLen
|    C                   Parm      'DSPF0100'    Format            8
|    C                   Parm                    WFilLib
|    C                   Parm                    ApiErrDs
|+---C*                  If        ApiErrLen > 0
||   C*                  Return    *Off
|+---C*                  EndIf
+----C                   ON-ERROR
|    C                   EVAL      WIcErro = *ON
|    C                   Return    *Off
+----C                   ENDMON
     C
+----C                   IF        (NOT WIcErro)
|+---C                   MONITOR
||   C                   Eval      QDFFBASE = %SUBST( RcvVar:1:20 )
||   C                   Eval      SDFFINFO = QDFFINOF + 1
||   C                   Eval      QDFFINFO = %SUBST( RcvVar:SDFFINFO:36 )
||   C                   Eval      SDFARFTE = SDFFINFO + QDFFDFLO
||   C                   Eval      LDFARFTE = QDFFRCS * 16
||   C                   Eval      FMTAR    = *Blank
||   C                   Eval      Idx = 1
||+--C                   DoW       Idx <= QDFFRCS
|||  C                   Eval      FMTAR( Idx ) = %SUBST(RcvVar:SDFARFTE:16)
|||  C                   Eval      FMTNM( Idx ) = %SUBST(RcvVar:SDFARFTE:10)
|||  C                   Eval      Idx          = Idx + 1
|||  C                   Eval      SDFARFTE     = SDFARFTE + 16
||+--C                   EndDo
||   C                   Eval      Idx = 1
||   C     RcdFmt        LookUp    FmtNm( Idx )                           10
||+--C                   If        *In10 = True
|||  C                   MoveA     FmtAr( Idx )  QDFARFTE
|||  C                   Eval      SDFFRINF = SDFFINFO + QDFARFOF
|||  C                   Eval      QDFFRINF = %SUBST( RcvVar:SDFFRINF:30 )
|||  C                   Eval      SDFFRDPD = SDFFRINF + QDFFRAOF
|||  C                   Eval      QDFFRDPD = %SUBST( RcvVar:SDFFRDPD:34 )
|||  C                   Eval      SDFFRCTE = SDFFRINF + QDFFDRCO + 6
|||  C                   Eval      SDFFXRDP = SDFFRINF + QDFFXRDO
|||  C                   Eval      QDFFXRDP = %SUBST( RcvVar:SDFFXRDP:24 )
|||  C                   Eval      SDFFNTB  = SDFFRINF + QDFFNRCO
|||  C                   Eval      FldNm    = *Blank
|||  C                   Eval      FldRC    = *Blank
|||  C                   Eval      Idx  = 1
|||+-C                   DoW       Idx <= QDFFFLD
|||| C                   Eval      FLDNM( Idx ) = %SUBST( RcvVar:SDFFNTB:10)
|||| C                   Eval      FLDRC( Idx ) = %SUBST( RcvVar:SDFFRCTE:2)
|||| C                   Eval      Idx          = Idx      +  1
|||| C                   Eval      SDFFNTB      = SDFFNTB  + 10
|||| C                   Eval      SDFFRCTE     = SDFFRCTE +  2
|||+-C                   EndDo
|||  C                   Eval      Idx = 1
|||  C     FldNam        LookUp    FldNm( Idx )                           11
|||+-C                   If        *In11 = True
|||| C                   Eval      RC1   = X'00'
|||| C                   MoveA     FldRC( Idx )  QDFFRCTE
|||| C                   Move      QDFFSROW      RC2
|||| C                   Z-Add     RC12          Row
|||| C                   Move      QDFFSCOL      RC2
|||| C                   Z-Add     RC12          Col
|||| C                   Eval      Col = Col + 1
||||+C                   If        Col > 80
|||||C                   Eval      Col = Col - 80
|||||C                   Eval      Row = Row + 1
|||||C                   Eval      Row = Row + 1
||||+C                   EndIf
|||+-C                   EndIf
||+--C                   EndIf
|+---C                   ON-ERROR
||   C                   RETURN    *OFF
|+---C                   ENDMON
+----C                   EndIf
     C                   Return    *ON
     P $getFldLoc      E
