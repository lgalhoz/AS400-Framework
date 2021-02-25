     H*DFTACTGRP(*NO) BNDDIR('JMDIR') ALWNULL(*USRCTL)
     H NOMAIN
     H*==============================================================================
     H*------------------------------------------------------------------------------
     H* Com este grupo de funções, agilizamos os programas tradicionais de RPG ILE
     H* na componente de manuseamento dos campos de Input.
     H*------------------------------------------------------------------------------
     H* Este grupo de funções tem como objectivo gerir os atributos dos campos dos
     H* Ecrãs. para o efeito deverá respeitar os Standards, da onde são descritos os
     H* campos reservados a usar, por exemplo para devolver o nome do campo onde for
     H* pressionada uma tecla de função, ou então a utilização de campos P-Field para
     H* para controlo de atributos dos Campos.
     H*------------------------------------------------------------------------------
     H* Estrutura: [ECRA](1)<--->(n)[Formato Registo](1)<--->(n)[Campo]
     H*------------------------------------------------------------------------------
     H* Em baixo um exemplo da utilização das funções, num programa:
     H*     ...
     H*       WSessScr = $AddScreen('FPRDE102':'RPRDE10205');
     H*       $AddScPFld(%ADDR(ECR_F#01):'NOME');             // Controlo do Nome
     H*       $AddScPFld(%ADDR(ECR_F#02):'MORADA');           // Controlo da Morada
     H*       $AddScCoord(%ADDR(ECR_F#ROW):%ADDR(ECR:F#COL)); // Controlo Coordenada
     H*       $AddScMsg(%ADDR(ECR_MSG#ERR));                  // Controlo mensagem
     H*     ...
     H*       $InzScStatus();                    // Inicalizar campos em controlo
     H*       IF (ECR_NOME = *BLANKS);
     H*          $SetScError('NOME':'PRD0101');
     H*       ENDIF;
     H*       IF (ECR_MORADA = *BLANKS);
     H*          $SetScError('MORADA':'PRD0101');
     H*       ENDIF;
     H*     ...
     H*       IF ($GetScStatus() = 'O');
     H*          EXSR Gravar;
     H*       ENDIF;
     H*     ...
     H*       $RmvScreen(WSessScr);
     H*     ...
     H*
     H*==============================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO013

     D*==============================================================================
     D* Grupo de funções internas.
     D*==============================================================================
     D*--> API para importar as definições do ecrã
     D RtvFldInfo      PR              N
     D  FileName                     10A   Const
     D
     D*==============================================================================
     D*     Estrutura dos Campos de Ecrã
     D*==============================================================================
     D LST#SCR         DS                  QUALIFIED DIM(KTE#MaxScr)
     D   Name                        10A   INZ(*BLANKS)
     D   Fmt                               LIKEDS(TPLSCREEN) DIM(KTE#MaxFmt)
     D   QtdFmt                       3S 0 INZ(*ZEROS)
     D   CurFmt                       3S 0 INZ(*ZEROS)
     D   RowAddr                       *   INZ(*NULL)
     D   ColAddr                       *   INZ(*NULL)
     D   MsgAddr                       *   INZ(*NULL)
     D   PrevScreen                   3S 0 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D KTE#MaxFmt      C                   10
     D KTE#MaxScr      C                   25
     D
     D WCurScr         S              2S 0
     D WIdxFmt         S              2S 0
     D WIdxFld         S              2S 0
     D
     D TPLSCREEN       DS                  QUALIFIED
     D   Name                        10A   INZ(*BLANKS)
     D   Fld                               LIKEDS(TPLFIELD) DIM(KTE#MaxFld)
     D   CurFld                       3S 0 INZ(*ZEROS)
     D   QtdFld                       3S 0 INZ(*ZEROS)
     D   SCStatus                     1A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D KTE#MaxFld      C                   50
     D
     D TPLFIELD        DS                  QUALIFIED
     D   Name                        10A   INZ(*BLANKS)
     D   P#FldName                   10A   INZ(*BLANKS)
     D   P#FldVal                     1A   INZ(*BLANKS)
     D   P#FldPtr                      *   INZ(*NULL)
     D   Row                          3P 0 INZ(*ZEROS)
     D   Col                          3P 0 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==============================================================================

     P*==============================================================================
     P* Processo...: $AddScreen
     P* Descrição..: Adicionar um novo Ecrã.
     P*==============================================================================
     P $AddScreen      B                   Export
     D                 PI             3S 0
     D  FILENAME                     10A   Const
     D  FMTNAME                      10A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WFmtName        S             10A
     D WIdx            S              3S 0
     D WResult         S              3S 0
     D*==============================================================================
      /FREE
        WFmtName = *BLANKS;
        IF (%PARMS() > 1);
           WFmtName = FMTNAME;
        ENDIF;

        WIdx = 1;
        DOW (WIdx < KTE#MaxScr);
           //-----------------------------------------
           // Reservar o primeiro candidato livre
           //-----------------------------------------
           IF (LST#SCR(WIdx).IsEmpty);
              LST#SCR(WIdx).IsEmpty = *OFF;
              IF (WCurScr <> *ZEROS);
                 LST#SCR(WIdx).PrevScreen = WCurScr; // Guardar anterior
              ELSE;
                 LST#SCR(WIdx).PrevScreen = WIdx;    // Se é o primeiro então manter
              ENDIF;
              WCurScr = WIdx;

              //------------------------------------------------
              // Carregar informação do ecrã, formatos e campos
              //------------------------------------------------
              RtvFldInfo(FILENAME);

              //------------------------------------------------
              // Posicionar-se no ecrã seleccionado
              //------------------------------------------------
              IF (WFmtName = *BLANKS);
                 LST#SCR(WCurScr).CurFmt = 1;
              ELSE;
                 WIdx = 1;
                 DOW (WIdx < KTE#MaxFmt);
                    IF (NOT LST#SCR(WCurScr).Fmt(WIdx).IsEmpty) AND
                       (%TRIM(LST#SCR(WCurScr).Fmt(WIdx).Name) =
                       %TRIM(WFmtName));
                       LST#SCR(WCurScr).CurFmt = WIdx;
                    ENDIF;
                    WIdx += 1;
                 ENDDO;
              ENDIF;
              Return WCurScr;
           ENDIF;
           WIdx += 1;
        ENDDO;
        RETURN *ZEROS;
      /END-FREE
     P $AddScreen      E

     P*==============================================================================
     P* Processo...: $SetScreen
     P* Descrição..: Definir o ecrã corrente ou o formato do registo
     P*==============================================================================
     P $SetScreen      B                   Export
     D                 PI              N
     D  IDSCREEN                      3S 0 Const
     D  FMTNAME                      10A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WIdx            S              3S 0
     D WFmtName        S             10A
     D WResult         S               N
     D*==============================================================================
      /FREE
         WFmtName = *BLANKS;
         IF (%PARMS() > 1);
            WFmtName = FMTNAME;
         ENDIF;
         IF (LST#SCR(IDSCREEN).IsEmpty);
            RETURN *OFF;
         ELSE;
            WCurScr = IDSCREEN;
            IF (WFmtName = *BLANKS);
               RETURN *ON;
            ELSE;
               WIdx = 1;
               DOW (NOT LST#SCR(WCurScr).Fmt(WIdx).IsEmpty);
                  IF (%TRIM(LST#SCR(WCurScr).Fmt(WIdx).Name) =
                     %TRIM(WFmtName));
                     LST#SCR(WCurScr).CurFmt = WIdx;
                     RETURN *ON;
                  ENDIF;
                  WIdx += 1;
               ENDDO;
            ENDIF;
            LST#SCR(WCurScr).CurFmt = 1;
            RETURN *OFF;
         ENDIF;
      /END-FREE
     P $SetScreen      E

     P*==============================================================================
     P* Processo...: $GetScreen
     P* Descrição..: Obter o Ecrã corrente
     P*==============================================================================
     P $GetScreen      B                   Export
     D                 PI             3S 0
     D*==============================================================================
      /FREE
         RETURN WCurScr;
      /END-FREE
     P $GetScreen      E
     P*==============================================================================
     P* Processo...: $RmvScreen
     P* Descrição..: Remover o ecrã
     P*==============================================================================
     P $RmvScreen      B                   Export
     D                 PI              N
     D  IDSCREEN                      3S 0 Const
     D
     D*==> Campos de Trabalho
     D WResult         S               N
     D*==============================================================================
      /FREE
         IF (LST#SCR(IDSCREEN).IsEmpty);
            RETURN *OFF;
         ELSE;
            LST#SCR(IDSCREEN).IsEmpty = *ON;
            WCurScr = LST#SCR(IDSCREEN).PrevScreen;
            IF (WCurScr = IDSCREEN);
               WCurScr = *ZEROS;
            ENDIF;
            RETURN *ON;
         ENDIF;
      /END-FREE
     P $RmvScreen      E

     P*==============================================================================
     P* Processo...: $AddScPFld
     P* Descrição..: Adicionar o endereço do campo de Contro do campo
     P*              Quando possível dispensar o segundo parâmetro.
     P*==============================================================================
     P $AddScPFld      B                   Export
     D                 PI              N
     D   PFIELD                        *   Const
     D   FLDNAME                     10A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WCurFmt         S              3S 0
     D WP#FldVal       S              1A   BASED(PtrPFld)
     D WFldName        S             10A
     D WIdx            S              3S 0
     D WResult         S               N
     D*==============================================================================
      /FREE
         WFldName = *BLANKS;
         WCurFmt  = LST#SCR(WCurScr).CurFmt;
         IF (%PARMS() > 1);
            WFldName = FLDNAME;
         ENDIF;

         WIdx = 1;
         DOW (NOT LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).IsEmpty) OR
            (WIdx <= KTE#MaxFld);
            IF (%TRIM(LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).Name) =
                   %TRIM(WFldName));
               LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).P#FldPtr=PFIELD;
               PtrPFld = PFIELD;
               LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).P#FldVal=WP#FldVal;
               RETURN *ON;
            ENDIF;
            WIdx += 1;
         ENDDO;
         RETURN *OFF;
      /END-FREE
     P $AddScPFld      E

     P*==============================================================================
     P* Processo...: $AddScCoord
     P* Descrição..: Adicionar o endereço das Coordenadas do Ecrã
     P*==============================================================================
     P $AddScCoord     B                   Export
     D                 PI              N
     D   SCRROW                        *   Const
     D   SCRCOL                        *   Const
     D
     D*==> Campos de Trabalho
     D WCurFmt         S              3S 0
     D WResult         S               N
     D*==============================================================================
      /FREE
         WCurFmt = LST#SCR(WCurScr).CurFmt;

         LST#SCR(WCurScr).RowAddr = SCRROW;
         LST#SCR(WCurScr).ColAddr = SCRCOL;

         RETURN *ON;
      /END-FREE
     P $AddScCoord     E

     P*==============================================================================
     P* Processo...: $AddScMsg
     P* Descrição..: Adicionar o endereço da Mensagem de erro do Ecrã
     P*==============================================================================
     P $AddScMsg       B                   Export
     D                 PI              N
     D   MSGTEXTO                      *   Const
     D
     D*==> Campos de Trabalho
     D WCurFmt         S              3S 0
     D WResult         S               N
     D*==============================================================================
      /FREE
         WCurFmt = LST#SCR(WCurScr).CurFmt;
         LST#SCR(WCurScr).MsgAddr = MSGTEXTO;

         RETURN *ON;
      /END-FREE
     P $AddScMsg       E

     P*==============================================================================
     P* Processo...: $SetScError
     P* Descrição..: Formatar mensagem de Erro
     P*==============================================================================
     P $SetScError     B                   Export
     D                 PI              N
     D     FLDNAME                   10A   Const
     D     MSGID                      7A   Const
     D
     D*==> Campos de Trabalho
     D WIdx            S              3S 0
     D WCurFmt         S              3S 0
     D WFldName        S             10A
     D WResult         S               N
     D WFldAtrib       S              1A   BASED(PtrPFLD)
     D WScrCol         S              3P 0 BASED(PtrCOL)
     D WScrRow         S              3P 0 BASED(PtrROW)
     D WScrMsg         S             78A   BASED(PtrMSG)
     D*==============================================================================
      /FREE
         WFldName = FLDNAME;
         WCurFmt  = LST#SCR(WCurScr).CurFmt;
         LST#SCR(WCurScr).Fmt(WCurFmt).SCStatus = 'E';
         PtrRow   = LST#SCR(WCurScr).RowAddr;
         PtrCOL   = LST#SCR(WCurScr).ColAddr;
         PtrMSG   = LST#SCR(WCurScr).MsgAddr;
         WScrMsg  = $GetMessage(MSGID);

         WIdx = 1;
         DOW (NOT LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).IsEmpty);
            IF (%TRIM(LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).Name) =
                   %TRIM(WFldName));
               PtrPFLD   = LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).P#FldPtr;
               IF (PtrPFLD <> *NULL);
                  WFldAtrib = ATR#Red;
               ENDIF;
               WScrRow   = LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).Row;
               WScrCol   = LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).Col;
               RETURN *ON;
            ENDIF;
            WIdx += 1;
         ENDDO;
         RETURN WResult;
      /END-FREE
     P $SetScError     E

     P*==============================================================================
     P* Processo...: $SetScFldProt
     P* Descrição..: Proteger campo de escrita
     P*==============================================================================
     P $SetScFldProt   B                   Export
     D                 PI              N
     D     FLDNAME                   10A   Const
     D
     D*==> Campos de Trabalho
     D WIdx            S              3S 0
     D WCurFmt         S              3S 0
     D WFldName        S             10A
     D WResult         S               N
     D WFldAtrib       S              1A   BASED(PtrPFLD)
     D*==============================================================================
      /FREE
         WFldName = FLDNAME;
         WCurFmt  = LST#SCR(WCurScr).CurFmt;

         WIdx = 1;
         DOW (NOT LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).IsEmpty);
            IF (%TRIM(LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).Name) =
                   %TRIM(WFldName));
               PtrPFLD   = LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).P#FldPtr;
               WFldAtrib = ATR#Protect;
               RETURN *ON;
            ENDIF;
            WIdx += 1;
         ENDDO;
         RETURN WResult;
      /END-FREE
     P $SetScFldProt   E

     P*==============================================================================
     P* Processo...: $SetScFldUnpr
     P* Descrição..: Proteger campo de escrita
     P*==============================================================================
     P $SetScFldUnpr   B                   Export
     D                 PI              N
     D     FLDNAME                   10A   Const
     D
     D*==> Campos de Trabalho
     D WIdx            S              3S 0
     D WCurFmt         S              3S 0
     D WFldName        S             10A
     D WResult         S               N
     D WFldAtrib       S              1A   BASED(PtrPFLD)
     D*==============================================================================
      /FREE
         WFldName = FLDNAME;
         WCurFmt  = LST#SCR(WCurScr).CurFmt;

         WIdx = 1;
         DOW (NOT LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).IsEmpty);
            IF (%TRIM(LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).Name) =
                   %TRIM(WFldName));
               PtrPFLD   = LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).P#FldPtr;
               WFldAtrib = ATR#Red; // Desproteger o Campo
               RETURN *ON;
            ENDIF;
            WIdx += 1;
         ENDDO;
         RETURN WResult;
      /END-FREE
     P $SetScFldUnpr   E

     P*==============================================================================
     P* Processo...: $GetScStatus
     P* Descrição..: Obter o estado do Ecrã
     P*==============================================================================
     P $GetScStatus    B                   Export
     D                 PI             1A
     D
     D*==> Campos de Trabalho
     D WCurFmt         S              3S 0
     D*==============================================================================
      /FREE
         WCurFmt  = LST#SCR(WCurScr).CurFmt;
         RETURN LST#SCR(WCurScr).Fmt(WCurFmt).SCStatus;
      /END-FREE
     P $GetScStatus    E

     P*==============================================================================
     P* Processo...: $InzScStatus
     P* Descrição..: Inicializar o ecrã
     P*==============================================================================
     P $InzScStatus    B                   Export
     D                 PI              N
     D
     D*==> Campos de Trabalho
     D WIdx            S              3S 0
     D WCurFmt         S              3S 0
     D WResult         S               N
     D WFldAtrib       S              1A   BASED(PtrPFLD)
     D WScrMsg         S             78A   BASED(PtrMSG)
     D*==============================================================================
      /FREE
         WCurFmt  = LST#SCR(WCurScr).CurFmt;
         LST#SCR(WCurScr).Fmt(WCurFmt).SCStatus = 'O';
         PtrMSG   = LST#SCR(WCurScr).MsgAddr;
         WScrMsg  = *BLANKS;

         WIdx = 1;
         DOW (NOT LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).IsEmpty);
            PtrPFLD   = LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).P#FldPtr;
            IF (PtrPFLD <> *NULL);
               WFldAtrib = LST#SCR(WCurScr).Fmt(WCurFmt).Fld(WIdx).P#FldVal;
            ENDIF;
            WIdx += 1;
         ENDDO;
         RETURN *ON;
      /END-FREE
     P $InzScStatus    E

     P*==============================================================================
     P* Processo...: RtvFldInfo
     P* Descrição..: Retrieve field location
     P*==============================================================================
     P RtvFldInfo      B                   Export
     D                 PI              N
     D  PFILENAME                    10A   Const
     D
     D*==> Obter informação do Campo a partir da DDS
     D  QDFRTVFD       PR                  ExtPGM('QDFRTVFD')
     D     wRcvVar                32767A   Const
     D     WRcvVarLen                10I 0 Const
     D     Format                     8A   Const
     D     WFilLib                   20A   Const
     D     WApiErrDs                       LIKEDS(ApiErrDs)
     D
     D RcvVar          S          32767A
     D RcvVarLen       S             10I 0 Inz(%Size(RcvVar))
     D SDFFINFO        S             10I 0
     D LDFARFTE        S             10I 0
     D SDFARFTE        S             10I 0
     D SDFFRDPD        S             10I 0
     D SDFFRCTE        S             10I 0
     D SDFFXRDP        S             10I 0
     D SDFFRINF        S             10I 0
     D SDFFNTB         S             10I 0
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
     D*==> Formatar a biblioteca
     D  FILLIB         DS
     D   WFilLib                     20A
     D    WFILE                1     10A
     D    WLIB                11     20A
     D
     D WIdx            S              3S 0
     D WName           S             10A
     D WRow            S              3S 0
     D WCol            S              3S 0
     D*==============================================================================
      /FREE
        LST#SCR(WCurScr).Name = PFILENAME;
        IF (PFILENAME = *BLANKS);
           RETURN *OFF;
        ENDIF;

        WIcErro = *OFF;
        WLib  = '*LIBL';
        WFile = PFILENAME;

        //------------------------------------------------------
        // Retrieve display file description (QDFRTVFD) API
        //------------------------------------------------------
        MONITOR;
           QDFRTVFD(RcvVar:RcvVarLen:'DSPF0100':WFilLib:ApiErrDs);
        ON-ERROR;
           Return *Off;
        ENDMON;

        MONITOR;
           QDFFBASE = %SUBST(RcvVar:1:20);         // Base File Section
           SDFFINFO = QDFFINOF + 1;                // Location for File Header Section
           QDFFINFO = %SUBST(RcvVar:SDFFINFO:36);  // File Header Section
           SDFARFTE = SDFFINFO + QDFFDFLO;         // Location for First Format Name
           LDFARFTE = QDFFRCS * 16;                // Return the Record Format Definition Size
           WIdxFmt = 1;
           DOW (WIdxFmt <= QDFFRCS);
              LST#SCR(WCurScr).QtdFmt = WIdxFmt;
              LST#SCR(WCurScr).Fmt(WIdxFmt).IsEmpty = *OFF;
              LST#SCR(WCurScr).Fmt((WIdxFmt+1)).IsEmpty = *ON;
              LST#SCR(WCurScr).Fmt(WIdxFmt).Name = %SUBST(RcvVar:SDFARFTE:10);
              QDFARFTE   = %SUBST(RcvVar:SDFARFTE:16); // Record Format Table

              SDFFRINF = SDFFINFO + QDFARFOF;        // Position of Record Header Section
              QDFFRINF = %SUBST(RcvVar:SDFFRINF:30); // Record Header Section
              SDFFRDPD = SDFFRINF + QDFFRAOF;        // Position of Display-Record-Level
              QDFFRDPD = %SUBST(RcvVar:SDFFRDPD:34); // Display-Record-Level Device-Dependent Section
              SDFFRCTE = SDFFRINF + QDFFDRCO + 6;    //
              SDFFXRDP = SDFFRINF + QDFFXRDO;        // Position of Display-Record-Level Device-Dependent Section
              QDFFXRDP = %SUBST(RcvVar:SDFFXRDP:24); // Display-Record-Level Device-Dependent Section Extension
              SDFFNTB  = SDFFRINF + QDFFNRCO;        // Position of First Field Name
              WIdxFld  = 1;
              WIdx     = *ZEROS;
              DOW (WIdxFld <= QDFFFLD);
                 WName = %SUBST(RcvVar:SDFFNTB:10);
                 IF (WName <> *BLANKS);
                    WIdx += 1;
                    LST#SCR(WCurScr).Fmt(WIdxFmt).CurFld = 1;
                    LST#SCR(WCurScr).Fmt(WIdxFmt).QtdFld = WIdx;
                    LST#SCR(WCurScr).Fmt(WIdxFmt).Fld(WIdx).name = WName;
                    RC1      = X'00';
                    QDFFRCTE = %SUBST(RcvVar:SDFFRCTE:2);
                    RC2   = QDFFSROW;
                    WRow  = RC12;
                    RC2   = QDFFSCOL;
                    WCol  = RC12 + 1;
                    IF (WCol > 80);
                       WCol -= 80;
                       WRow += 2;
                    EndIf;
                    LST#SCR(WCurScr).Fmt(WIdxFmt).Fld(WIdx).row = WRow;
                    LST#SCR(WCurScr).Fmt(WIdxFmt).Fld(WIdx).col = WCol;
                    LST#SCR(WCurScr).Fmt(WIdxFmt).Fld(WIdx).IsEmpty = *OFF;
                    LST#SCR(WCurScr).Fmt(WIdxFmt).Fld((WIdx+1)).IsEmpty = *ON;
                 ENDIF;
                 WIdxFld  += 1;
                 SDFFNTB  += 10;
                 SDFFRCTE += 2;
              ENDDO;

              WIdxFmt  += 1;
              SDFARFTE += 16;
           ENDDO;
        ON-ERROR;
           RETURN *OFF;
        ENDMON;

        RETURN *ON;
      /END-FREE
     P RtvFldInfo      E
