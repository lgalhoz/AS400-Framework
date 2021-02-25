     H DFTACTGRP(*NO)
     H*NOMAIN
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
     H*===================================================================
     H**********************************************************************
      /Copy QRPGLESRC,SGRLO010

     D*==> Funções Internas
     D RtvFldInfo      PR              N
     D  QDspFil                      10A   Const
     D  RcdFmt                       10A   Const
     D  FldNam                       10A   Const
     D  Row                           3P 0
     D  Col                           3P 0
     D
     D*===> Estrutura dos Campos de Ecrã
     D LST#SCREEN      DS                  QUALIFIED DIM(25)
     D   SCR                               LIKEDS(TPLSCREEN)
     D   IsEmpty                       N
     D WCurLst         S              2S 0
     D WIdxLst         S              2S 0
     D WIdxFld         S              2S 0
     D
     D TPLSCREEN       DS                  QUALIFIED
     D   NomFile                     10A
     D   NomFmt                      10A
     D   Flds                              LIKEDS(TPLFIELD) DIM(50)
     D   SCStatus                     1A
     D   RowAddr                       *
     D   ColAddr                       *
     D   MsgAddr                       *
     D
     D TPLFIELD        DS                  QUALIFIED
     D   Nome                        10A
     D   Row                          3S 0
     D   Col                          3S 0
     D   P#FldNam                    10A
     D   P#FldVal                      *
     D   IcEmpty                       N
     D
     D*==> Campos de Trabalho
     D  WRow           S              3P 0
     D  WCol           S              3P 0
     D
     D*==================================================================
      /FREE
        RtvFldInfo('AP6004E':'RAP6004E05':'CLTAR':WRow:WCol);

        *INLR = *ON;

      /END-FREE

     P*===================================================================
     P*         PROCESSOS
     P*===================================================================
     P*  WScreenId = $AddScreen(<File>:<Fmt>);    // Criar Ecrã
     P*  WStatus   = $SetScreen(WScreenID);       // Colocar Ecrã
     P*  WStatus   = $RmvScreen(WScreenID);       // Remover Ecrã
     P*
     P*  WStatus   = $AddScPFld(P-Field:<Field>); // Gravar endereço do P-Field
     P*  WStatus   = $AddScRow(ROW);              // Gravar endereço da Linha
     P*  WStatus   = $AddScCOL(COL);              // Gravar endereço da Coluna
     P*  WStatus   = $AddScMsg(MSG);              // Gravar endereço da Mensagem
     P*
     P*  WStatus   = $SetScError(<Field>:<MsgId>);// ForCompr erro
     P*  WStatus   = $GetScStatus();              // O = 'Sem erros; E = Com Erros
     P*  WStatus   = $InzScStatus();              // Coloca Status = 'O'
     P*===================================================================

     P*===================================================================
     P* Processo...: RtvFldInfo
     P* Descrição..: Retrieve field location
     P* Observação
     P*
     P*     Expandir esta funcionalidade para trabalhar com um conjunto de
     P*     ficheiros de trabalho, que passam a guarda as coordenadas do
     P*     campo, bem como o P-Field que lhe está associado.
     P*
     P*     A actualização da tabela, será realizada com recurso a um programa
     P*     desenvolvido para o efeito e que realiza os seguinte passos:
     P*        1. Ler o source do ficheiro de ecrã (DSPF)
     P*        2. Limpar Campos P-Field e respectivas referências
     P*        3. EXtrair as Coordenadas dos Campos
     P*        4. Actualizar no Source os Campos P-Field e novas referências
     P*
     P*     Os Campos P-Field obedecem à seguinte estrutura:
     P*           P#<Dominio><Seq.Ficheiro><Seq.Campo>
     P*
     P*     Por exemplo para os ficheiros de Ecrãs:
     P*        Ficheiro de Ecrã = FPRDE100
     P*           Campos:
     P*               P#PRD10001  Sobre o Campo A
     P*               P#PRD10002  Sobre o Campo B
     P*               P#PRD10003  Sobre o Campo C
     P*
     P*        Ficheiro de Ecrã = FPRDE102
     P*           Campos:
     P*               P#PRD10201  Sobre o Campo A
     P*               P#PRD10202  Sobre o Campo B
     P*               P#PRD10203  Sobre o Campo C
     P*
     P*===================================================================
     P RtvFldInfo      B                   Export
     D                 PI              N
     D  QDspFil                      10A   Const
     D  RcdFmt                       10A   Const
     D  FldNam                       10A   Const
     D  Row                           3P 0
     D  Col                           3P 0
     D
     D*==> Obter informação do Campo a partir da DDS
|    D  QDFRTVFD       PR                  ExtPGM('QDFRTVFD')
|    D     wRcvVar                32767A   Const
|    D     WRcvVarLen                10I 0 Const
|    D     Format                     8A   Const
|    D     WFilLib                   20A   Const
|    D     WApiErrDs                       LIKEDS(ApiErrDs)
     D
     D RcvVar          S          32767A
     D RcvVarLen       S             10I 0 Inz(%Size(RcvVar))
     D Idx             S             10I 0
     D SDFFINFO        S             10I 0
     D LDFARFTE        S             10I 0
     D SDFARFTE        S             10I 0
     D SDFFRDPD        S             10I 0
     D SDFFRCTE        S             10I 0
     D SDFFXRDP        S             10I 0
     D SDFFRINF        S             10I 0
     D SDFFNTB         S             10I 0
     D True            C                   *On
     D False           C                   *Off
     D FMTAR           S             16    Dim(999)
     D FMTNM           S             10    Dim(999)
     D FLDNM           S             10    Dim(999)
     D FLDRC           S              2    Dim(999)
     D
     D ApiErrDs        DS
     D   BytesProv                   10I 0 inz(0)
     D   BytesAvail                  10I 0 inz(0)
     D
     D                 DS
     D RC12                           5I 0
     D  RC1                           1A   Overlay(RC12:1)
     D  RC2                           1A   Overlay(RC12:2)
     D
     D*-----------------------------------------
     D* Base file section (QDFFBASE)
     D*-----------------------------------------
     D QDFFBASE        DS
     D   QDFFRETN              1      4B 0                                      Length of the returned data
     D   QDFFSIZE              5      8B 0                                      Size of the display file description
     D   QDFFINOF              9     10B 0                                      Displacement to file header section
     D   QDFFRCS              11     12B 0                                      Number of record formats specified
     D   QDFBITS              13     13                                         Display attribute bits
     D   QDFFFSCR             14     15B 0                                      Number of valid file screen sizes
     D   QDFFSRSQ             16     17B 0                                      Displacement to sort sequence table
     D   QDFCCSID             18     19B 0                                      Source member used to create device file
     D   QDFFSCRS             20     20                                         Screen size table
     D
     D*-----------------------------------------
     D* File header section (QDFFINFO)
     D*-----------------------------------------
     D QDFFINFO        DS
     D   QDFFDFLO              1      4B 0                                      Length of the file header section
     D   QDFFWUOF              5      8B 0                                      Displacement file-level information
     D   QDFFFMWU              9     12B 0                                      Length of file header section
     D   QDFFSEQO             13     16B 0                                      Displacement from structure
     D   QDFFFSFL             17     18B 0                                      Maximum number of entries in the
     D   QDFFFSCE             19     20B 0                                      Maximum number of entries
     D   QDFBITS01            21     22                                         File level flag
     D   QDFRVED7             23     34                                         Reserved
     D   QDFFXDOF             35     36B 0                                      Displacement to display-file-level
     D
     D*-----------------------------------------
     D*  Record format table (QDFARFTE)
     D*-----------------------------------------
     D QDFARFTE        DS
     D   QDFARFNM              1     10                                         Record format name
     D   QDFBITS08            11     12                                         Miscellaneous record contents
     D   QDFARFOF             13     16B 0                                      Displacement to the record header
     D
     D*-----------------------------------------
     D* Record header section (QDFFRINF)
     D*-----------------------------------------
     D QDFFRINF        DS
     D   QDFFRDDO              1      4B 0                                      Length of the record header section
     D   QDFFOFIT              5      8B 0                                      The displacement from structure
     D   QDFFSTBO              9     12B 0                                      The displacement from structure
     D   QDFBITS09            13     16                                         Miscellaneous record contents
     D   QDFFFLD              17     18B 0                                      Number of fields in this record
     D   QDFVED21             19     22                                         Reserved
     D   QDFFINDO             23     24B 0                                      If INDARA keyword is specified
     D   QDFVED22             25     28                                         Reserved
     D   QDFFRAOF             29     30B 0                                      Displacements to display-record-level
     D
     D*--------------------------------------------------------------
     D* Display-Record-Level-Device-Dependent section (QDFFRDPD)
     D*--------------------------------------------------------------
     D QDFFRDPD        DS
     D   QDFFDRCO              1      4B 0                                      Displacement to first row-column
     D   QDFFINCP              5      6B 0                                      Number of input-capable fields (H too)
     D   QDFFFIBF              7      8B 0                                      Number of input and both fields
     D   QDFFFOIS              9     10B 0                                      Number of option indicators
     D   QDFVED23             11     12                                         Reserved
     D   QDFBITS11            14     14                                         Reserved
     D   QDFBITS12            15     15                                         Reserved
     D   QDFBITS13            16     16                                         Reserved
     D   QDFBITS14            17     18                                         Reserved
     D   QDFBITS15            19     20                                         Reserved
     D   QDFFERRM             21     22B 0                                      Index to first field in index table
     D   QDFBITS16            23     23                                         Reserved
     D   QDFBITS17            24     24                                         Reserved
     D   QDFFCGRI             25     26B 0                                      Response indicator for record-level
     D   QDFBITS18            27     28                                         Reserved
     D   QDFFXRDO             29     30B 0                                      Displacement to display-record-level
     D   QDFVED32             31     32                                         Reserved
     D   QDFFRDOC             33     34B 0                                      Displacement to keyword category
     D*
     D*-----------------------------------------------------
     D* Display-Record-Level-Device-Dependent section
     D*  extension structure ( QDFFXRDP )
     D*-----------------------------------------------------
     D QDFFXRDP        DS
     D   QDFFFOTO              1      4B 0                                      Displacement to field order
     D   QDFFNRCO              5      8B 0                                      Displacement to first field name
     D   QDFVED33              9     12                                         Reserved
     D   QDFFSEGS             13     14B 0                                      Number of segments in record
     D   QDFVED34             15     16                                         Reserved
     D   QDFHCTLO             17     18B 0                                      Buffer displacement to the field
     D   QDFVED35             19     24                                         Reserved
     D
     D*-----------------------------------------------------
     D* Row-Column table entry (QDFFRCTE)
     D*-----------------------------------------------------
     D QDFFRCTE        DS
     D   QDFFSROW              1      1                                         Starting row
     D   QDFFSCOL              2      2                                         Starting Col
     D WIcErro         S               N
     D
     D*==> ForCompr a biblioteca
     D  FILLIB         DS
     D   WFilLib                     20A
     D    WFILE                1     10A
     D    WLIB                11     20A
     C*==============================================================
      /FREE
        IF (QDspFil = *BLANKS) OR (RcdFmt = *BLANKS) OR (FldNam = *BLANKS);
           RETURN *OFF;
        ENDIF;

        WIcErro = *OFF;
        WLib  = '*LIBL';
        WFile = QDSPFIL;
        Row   = *ZEROS;
        Col   = *ZEROS;

        //------------------------------------------------------
        // Retrieve display file description (QDFRTVFD) API
        //------------------------------------------------------
+----   MONITOR;
           QDFRTVFD(RcvVar:RcvVarLen:'DSPF0100':WFilLib:ApiErrDs);
+----   ON-ERROR;
|          WIcErro = *ON;
|          Return *Off;
+----   ENDMON;

+----   IF (NOT WIcErro);
|+---      MONITOR;
||            QDFFBASE = %SUBST(RcvVar:1:20);         // Base File Section
||            SDFFINFO = QDFFINOF + 1;
||            QDFFINFO = %SUBST(RcvVar:SDFFINFO:36);  // File Header Section
||            SDFARFTE = SDFFINFO + QDFFDFLO;
||            LDFARFTE = QDFFRCS * 16;
||            FMTAR    = *Blank;
||            Idx = 1;
||+--         DOW (Idx <= QDFFRCS);
|||               FMTAR(Idx) = %SUBST(RcvVar:SDFARFTE:16);
|||               FMTNM(Idx) = %SUBST(RcvVar:SDFARFTE:10);
|||               Idx       += 1;
|||               SDFARFTE  += 16;
||+--         ENDDO;

||            Idx = %LooKup(RcdFmt:FmtNm);
              IF (Idx > *ZEROS);
                 QDFARFTE = FmtAr(Idx);
|||              SDFFRINF = SDFFINFO + QDFARFOF;
|||              QDFFRINF = %SUBST(RcvVar:SDFFRINF:30);
|||              SDFFRDPD = SDFFRINF + QDFFRAOF;
|||              QDFFRDPD = %SUBST(RcvVar:SDFFRDPD:34);
|||              SDFFRCTE = SDFFRINF + QDFFDRCO + 6;
|||              SDFFXRDP = SDFFRINF + QDFFXRDO;
|||              QDFFXRDP = %SUBST(RcvVar:SDFFXRDP:24);
|||              SDFFNTB  = SDFFRINF + QDFFNRCO;
|||              FldNm    = *Blank;
|||              FldRC    = *Blank;
|||              Idx  = 1;
|||+-            DOW (Idx <= QDFFFLD);
||||                FLDNM(Idx) = %SUBST(RcvVar:SDFFNTB:10);
||||                FLDRC(Idx) = %SUBST(RcvVar:SDFFRCTE:2);
||||                Idx          = Idx + 1;
||||                SDFFNTB      = SDFFNTB + 10;
||||                SDFFRCTE     = SDFFRCTE + 2;
|||+-            ENDDO;
|||              Idx = %LooKup(FldNam:FldNm);
                 IF (Idx > *ZEROS);
||||                RC1   = X'00';
                    QDFFRCTE = FldRc(Idx);
||||                RC2 = QDFFSROW;
||||                Row = RC12;
||||                RC2 = QDFFSCOL;
||||                Col = RC12;
||||                Col = Col + 1;
||||+               IF (Col > 80);
|||||                  Col = Col - 80;
|||||                  Row = Row + 1;
|||||                  Row = Row + 1;
||||+               EndIf;
|||+-            EndIf;
||+--         EndIf;
|+---      ON-ERROR;
||            RETURN *OFF;
|+---      ENDMON;
+----   EndIf;
        Return *ON;
      /END-FREE
     P RtvFldInfo      E
