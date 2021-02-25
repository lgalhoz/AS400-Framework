      *=======================================================
      * Dat.Cria...: 2012-12-12
      * Autor......: Luis Galhoz
      * Objectivo..: Invocar a API, para obter o programa chamador
      *=======================================================
     H NOMAIN
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012

     P*=======================================================
     P* Processo..: $getAppCaller
     P* Objectivo.: Devolve o nome do Prgorama Chamador
     P*=======================================================
     P $getAppCaller   B                   EXPORT
     D                 PI            10A
     D    AppName                    10A   Const Options(*NOPASS)
     D
      * API para obter a lista de Programas activa
     D FindCaller      PR                  Extpgm('QWVRCSTK')
     D                             2000A
     D                               10I 0
     D                                8A   CONST
     D                               56A
     D                                8A   CONST
     D                               15A

      * O dados da Stack
     D Var             DS          2000
     D  BytAvl                       10I 0
     D  BytRtn                       10I 0
     D  Entries                      10I 0
     D  Offset                       10I 0
     D  EntryCount                   10I 0

      *  Call Stack Job Information
     D JobIdInf        DS
     D  JIDQName                     26A   Inz('*')
     D  JIDIntID                     16A
     D  JIDRes3                       2A   Inz(*loval)
     D  JIDThreadInd                 10I 0 Inz(1)
     D  JIDThread                     8A   Inz(*loval)
     D
     D  Entry          DS
     D    EntryLen                   10I 0
     D    ReqstLvl                   10I 0 Overlay(Entry:21)
     D    PgmNam                     10A   Overlay(Entry:25)
     D    PgmLib                     10A   Overlay(Entry:35)
     D    PgmProc                    10A   Overlay(Entry:49)

     D VarLen          S             10I 0 Inz(%size(Var))
     D ApiErr          S             15A
     D Caller          S             50A
     D WhoCalled       S             10A
     D WAppName        S             10A
     D i               S             10I 0

      /FREE
        IF (%PARMS() > *ZEROS);
           WAppName = AppName;
        ELSE;
           WAppName = *BLANKS;
        ENDIF;

        WhoCalled = *BLANKS;
        CallP     FindCaller(Var:VarLen:'CSTK0100':JobIdInf
                  :'JIDF0100':ApiErr);
        For i = 1 to EntryCount;
           Entry = %subst(Var:Offset + 1);
           IF (PgmProc <> 'RGRLO027') AND (PgmNam <> S#PGM);
              Caller = %trim(PgmLib) + '/' + %trim(PgmNam);
              IF (WAppName <> *BLANKS) AND (PgmNam = WAppName);
                 WhoCalled = PgmNam;
                 RETURN WhoCalled;
              ELSEIF (WhoCalled = *blanks);
                 WhoCalled = PgmNam;
                 RETURN WhoCalled;
              ENDIF;
           ENDIF;
           Offset = Offset + EntryLen;
        Endfor;

        RETURN WhoCalled;
      /END-FREE
     P $getAppCaller   E
