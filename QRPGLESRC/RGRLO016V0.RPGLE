     H NOMAIN
     H*=================================================================
     H* Processo...: SndFTP
     H* Descrição..: Enviar um ficheiro de dados de AS/400, ou uma
     H*              biblioteca para outro Sistema.
     H*
     H* Observação.: Se a transferência ocorrer com sucesso então recebe
     H*              o resultado como *OFF, se ocorrer erro recebe *ON.
     H*=================================================================
     D*==> Obter a quantidade de Ficheiros existente
     D SNDFTP          PR              N
     D    P_FILE                     20A   Const
     D    DestFile                   20A   Const
     D    P_FTPUser                  10A   Const OPTIONS(*NOPASS)
     D    P_FTPPassw                 15A   Const OPTIONS(*NOPASS)
     D    P_FTPServer                15A   Const OPTIONS(*NOPASS)
     D    P_FTPFmt                    5A   Const OPTIONS(*NOPASS)
     D
     D*---------------------------------------------
     D* Informação sobre a data e hora da operação
     D*---------------------------------------------
     D                SDS
     D  CurDate              276    281  0
     D    CMes               276    277  0
     D    CDia               278    279  0
     D    CAno               280    281  0
     D  CurTime              282    287
     C*=================================================================
     C
     P*==================================================================
     P* Processo..: SNDFTP
     P* Descrição.: Transferir qualquer ficheiro AS/400, via FTP.
     P*             Este programa é para ser usado como ferramenta
     P*             entre o antigo AS/400 eo novo AS/400.
     P*==================================================================
     P SndFTP          B                   EXPORT
     D SndFTP          PI              N
     D    P_FILE                     20A   Const
     D    DestFile                   20A   Const
     D    P_FTPUser                  10A   Const OPTIONS(*NOPASS)
     D    P_FTPPassw                 15A   Const OPTIONS(*NOPASS)
     D    P_FTPServer                15A   Const OPTIONS(*NOPASS)
     D    P_FTPFmt                    5A   Const OPTIONS(*NOPASS)
     D
     D*---------------------------------------------
     D* Invocar um comando de AS/400 externo:
     D*---------------------------------------------
     D shell           PR                  EXTPGM('SNDPF')
     D    P_FILE                     20A   Const
     D    DestFile                   20A   Const
     D    P_FTPUser                  10A   Const OPTIONS(*NOPASS)
     D    P_FTPPassw                 15A   Const OPTIONS(*NOPASS)
     D    P_FTPServer                15A   Const OPTIONS(*NOPASS)
     D    P_FTPFmt                    5A   Const OPTIONS(*NOPASS)
     D    P_IcStatus                   N   Const OPTIONS(*NOPASS)
     D
     D*---------------------------------------------
     D* Lista de Parametros Recebidos
     D*---------------------------------------------
     D File            DS
     D   NomFile                     10A
     D   NomLib                      10A
     D FTPUser         S             10A   INZ('ntorrado')
     D FTPPassw        S             15A   inz('Demomat@2011')
     D FTPServer       S             15A   INZ('20.0.0.130')
     D FTPFmt          S              5A   INZ('ASCII')
     D
     D WIcStatus       S               N
     C*=================================================================
     C*--------------------------------------------------------
      /FREE
        //-------------------------------------------------------------
        // Assumir valores por defeito para os parametros facultativos
        //-------------------------------------------------------------
        File = P_File;
        WIcStatus = *OFF; // Sem erros até ao momento
        MONITOR;
           FTPServer = P_FTPServer;
           FTPUser = P_FTPUser;
           FTPPassw = P_FTPPassw;
           FTPFmt = P_FTPFmt;
        ON-ERROR;
        ENDMON;

        Shell(File:DestFile:FTPServer:FTPUser:FTPPassw:FTPFmt:WIcStatus);

        RETURN WIcStatus;
      /END-FREE
     P SNDFTP          E
