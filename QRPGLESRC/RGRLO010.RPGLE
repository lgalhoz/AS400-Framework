     H NOMAIN
      *=======================================================
      * Dat.Cria...: 2011-05-18
      * Autor......: Luis Galhoz
      * Objectivo..: Devolver informação associada ao perfil
      *              do Utilizador.
      *=======================================================
      /Copy QRPGLESRC,SGRLO010

     D QSYRUSRI        PR                  ExtPgm('QSYRUSRI')
     D   rcvvar                   65535a   options(*varsize)
     D   rcvvarlen                   10i 0 const
     D   format                       8a   const
     D   usrprf                      10a   const
     D   errorCode                32767a   options(*varsize)
     D
     D ErrorCode       ds                  qualified
     D   bytesProv                   10i 0 inz(0)
     D   bytesAvail                  10i 0 inz(0)
     D
     D ResultSize      s             10i 0 inz(%size(Result))
     D Result          DS                  qualified
     D                                     based(p_Result)
     D   bytesrtn                    10i 0
     D   bytesavail                  10i 0
     D   UsrName                     50A   overlay(Result:199)
     D   OutQueuName                 10A   overlay(Result:361)
     D   OutQueuLib                  10A   overlay(Result:371)
     D   OutPrintDev                 10A   overlay(Result:381)
     D   HomeDirOff                  10i 0 overlay(Result:601)
     D   HomeDirLen                  10i 0 overlay(Result:605)
     D
     D homeDir         ds                  qualified
     D                                     based(p_homeDir)
     D   CCSID                       10i 0
     D   Country                      2a
     D   LangId                       3a
     D                                3a
     D   Flags                       10i 0
     D   Len                         10i 0
     D   Delim                        1c
     D                               10a
     D   nameucs                   5000c
     D   nameebc                   5000a   overlay(nameucs)
     P*=======================================================
     P* Processo..: getUserName
     P* Objectivo.: Devolve o nome do Utilizador
     P*=======================================================
     P $getUserName    B                   EXPORT
     D                 PI            50A
     D   WResult       S             50A
      /FREE
         // ------------------------------------------------
         //  Invocar a API para determinar a dimensão do
         //  buffer.
         // ------------------------------------------------
         p_Result = %alloc(ResultSize);
         dou Result.bytesAvail <= Result.BytesRtn;
            QSYRUSRI(Result:ResultSize:'USRI0300':'*CURRENT':ErrorCode);
            if (ResultSize < Result.BytesAvail);
               ResultSize = Result.BytesAvail;
               p_Result = %realloc(p_Result: ResultSize);
            endif;
         enddo;
         WResult = Result.UsrName;
         RETURN WResult;
      /END-FREE
     P $getUserName    E

     P*=======================================================
     P* Processo..: getDefPrint
     P* Objectivo.: Devolve a Impressora definida para o Utilizador
     P*=======================================================
     P $getDefPrint    B                   EXPORT
     D                 PI            10A
     D   WResult       S             10A
      /FREE
         // ------------------------------------------------
         //  Invocar a API para determinar a dimensão do
         //  buffer.
         // ------------------------------------------------
         p_Result = %alloc(ResultSize);
         dou Result.bytesAvail <= Result.BytesRtn;
            QSYRUSRI(Result:ResultSize:'USRI0300':'*CURRENT':ErrorCode);
            if (ResultSize < Result.BytesAvail);
               ResultSize = Result.BytesAvail;
               p_Result = %realloc(p_Result: ResultSize);
            endif;
         enddo;
         WResult = Result.OutPrintDev;
         RETURN WResult;
      /END-FREE
     P $getDefPrint    E

     P*=======================================================
     P* Processo..: getHomeDir
     P* Objectivo.: Devolve a localização dos documentos do
     P*             Utilizador.
     P*=======================================================
     P $getHomeDir     B                   EXPORT
     D                 PI           100A
     D charVar         S           5000a   varying
     D WResult         S            100A
      /FREE

         // ------------------------------------------------
         //  Invocar a API para determinar a dimensão do
         //  buffer.
         // ------------------------------------------------
         p_Result = %alloc(ResultSize);
         dou Result.bytesAvail <= Result.BytesRtn;
            QSYRUSRI(Result:ResultSize:'USRI0300':'*CURRENT':ErrorCode);
            if (ResultSize < Result.BytesAvail);
               ResultSize = Result.BytesAvail;
               p_Result = %realloc(p_Result: ResultSize);
            endif;
         enddo;

         p_Homedir = p_Result + Result.HomeDirOff;
         if HomeDir.ccsid=1200 or homeDir.CCSID=13488;
            charVar = %char(%subst(HomeDir.nameUcs:1
                                  : %div(HomeDir.len:2)));
         else;
            charVar = %subst(HomeDir.nameEbc:1:HomeDir.len);
         endif;
         dealloc p_Result;
         WResult = CharVar;
         RETURN WResult;
      /END-FREE
     P $getHomeDir     E
