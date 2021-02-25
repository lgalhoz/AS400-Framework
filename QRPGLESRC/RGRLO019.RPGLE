     H NOMAIN
     H*===================================================================
     H* Processo...: $CheckObject
     H* Objectivo..: alidar a existência de um Objecto
     H*===================================================================
     FFILEMBR   IF A F   74        DISK    USROPN
     F*=================================================================
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

     P*==================================================================
     P* Processo..: $CrtFile
     P* Descrição.: Criar ficheiro no AS400.
     P*==================================================================
     P $CrtFile        B                   EXPORT
     D $CrtFile        PI             7A
     D   FileName                    10A   Const
     D   FileLen                      3S 0 Const
     D   LibFileSrc                  21A   Const Options(*NOPASS)
     D   MbrSource                   10A   Const Options(*NOPASS)
     D   LibName                     10A   Const Options(*NOPASS)
     D   IsToDelete                    N   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WFileName       S             10A
     D WFileLen        S              3S 0
     D WLibFileSrc     S             21A
     D WMbrSource      S             10A
     D WLibName        S             10A
     D WIsToDelete     S               N
     D*==================================================================
      /FREE
         WFileName   = FileName;
         WFileLen    = FileLen;
         WLibFileSrc = *BLANKS;
         WLibName    = 'QTEMP';
         WIsToDelete = *ON;
         IF (%PARMS() > 2);
            WLibFileSrc = LibFileSrc;
            IF (%PARMS() > 3);
               WMbrSource = MbrSource;
               IF (%PARMS() > 4);
                  IF (LibName <> *BLANKS);
                     WLibName = LibName;
                  ENDIF;
                  IF (%PARMS() > 5);
                     WIsToDelete = IsToDelete;
                  ENDIF;
               ENDIF;
            ENDIF;
         ENDIF;

         //------------------------------------------------------
         // Eliminar o ficheiro se já existir
         //------------------------------------------------------
         IF (WIsToDelete);
            IF ($checkObject(WFileName:WLibName:'*FILE'));
            MONITOR;
               WStdCmd = 'DLTF FILE('+%TRIM(WLibName)+'/'+
                          %TRIM(WFileName)+')';
               $shellExec(WStdCmd);
            ON-ERROR;
               RETURN 'FTP0006';
            ENDMON;
            ENDIF;
         ENDIF;

         //------------------------------------------------------
         // Criar o ficheiro de Dados com ou sem source
         //------------------------------------------------------
         IF (WFileLen > *ZEROS);
            WStdCmd = 'CRTPF FILE('+%TRIM(WLibName) +'/'+
                       %TRIM(WFileName)+') RCDLEN('+%CHAR(WFileLen)+
                      ') OPTION(*NOLIST) SIZE(*NOMAX)';
         ELSE;
            WStdCmd = 'CRTPF FILE('+%TRIM(WLibName)+'/'+
                       %TRIM(WFileName)+
                       ') SRCFILE('+%TRIM(WLibFileSrc)+
                       ') SRCMBR('+%TRIM(WMbrSource)+
                       ') SIZE(*NOMAX) OPTION(*NOLIST)';
         ENDIF;
         MONITOR;
            $shellExec(WStdCmd);
         ON-ERROR;
            RETURN 'FTP0007';
         ENDMON;

         //---------------------------------------------------------------
         // Eliminar ficheiro de Spool gerado pela criação do ficheiro
         //---------------------------------------------------------------
         WStdCmd = 'DLTSPLF FILE('+%TRIM(WFileName)+') +
                      SPLNBR(*LAST) JOBSYSNAME(*ANY) SELECT(*ALL)';
         MONITOR;
            $shellExec(WStdCmd);
         ON-ERROR;
         ENDMON;
         RETURN *BLANKS;
      /END-FREE
     P $CrtFile        E

     P*==================================================================
     P* Processo..: GetAS400Name
     P* Descrição.: Obter o nome do Objectio no AS400.
     P*==================================================================
     P $GetAS400Name   B                   EXPORT
     D $GetAS400Name   PI           100A
     D    NomLib                     10A   Const
     D    NomFile                    10A   Const
     D
     D*==> Nome do Ficheiro
     D FILRecord       DS
     D    $LIB                14     23
     D    $FILE               24     33
     D    $MBR                65     74
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WMsgErro        S              7A
     D WNomMbro        S             10A
     D WResult         S            100A
     D*===================================================================
      /FREE
        //--------------------------------------------------------
        // Listar Membros dos Ficheiro a Exportar
        //--------------------------------------------------------
        MONITOR;
           WStdCmd = 'DLTF FILE(QTEMP/FILEMBR)';
           $shellExec(WStdCmd);
        ON-ERROR;
        ENDMON;
        MONITOR;
           WStdCmd = 'DSPFD FILE('+%TRIM(NomLib)+'/'+%TRIM(NomFile) + ') +
                      TYPE(*MBRLIST) OUTPUT(*OUTFILE) +
                      OUTFILE(QTEMP/FILEMBR)';
           $shellExec(WStdCmd);
        ON-ERROR;
           RETURN *BLANKS;
        ENDMON;

        WNomMbro = *BLANKS;
        OPEN FILEMBR;
        READ FILEMBR FILRecord;
        IF NOT %EOF();
           WNomMbro = $MBR;
           CLOSE FILEMBR;
        ENDIF;

        IF (WNomMbro <> *BLANKS);
           WResult  = '/QSYS.LIB/'+%TRIM(NomLIB)+'.LIB/'+%TRIM(NomFile)+
                      '.FILE/'+%TRIM(WNomMbro)+'.MBR';
        ELSE;
           WMsgErro = 'FTP0005'; // Não existem membros
        ENDIF;

        //--------------------------------------------------------
        // Eliminar o ficheiro temporário de trabalho
        //--------------------------------------------------------
        MONITOR;
           WStdCmd = 'DLTF FILE(QTEMP/FILEMBR)';
           $shellExec(WStdCmd);
        ON-ERROR;
        ENDMON;

        RETURN WResult;
      /END-FREE
     P $GetAS400Name   E
