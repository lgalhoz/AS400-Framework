     H ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) BNDDIR('QC2LE':'JMDIR')
     H*===========================================================
     H* Programa..: PAUDO020
     H* Descrição.: Auditar Ficheiros de Recibos enviados
     H*
     H* CRTPF FILE(QTEMP/FINTT005) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*
     H*===========================================================
     FFINTT005  UF A E           K DISK    EXTFILE('QTEMP/FINTT005')
     F                                     USROPN
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO016
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Lista de Parametros
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Ler uma linha de texto
     D ReadLine        PR            10I 0
     D   fd                          10I 0 value
     D   text                          *   value
     D   maxlen                      10I 0 value
     D
     D*==> Layout do Interface
     D REG#FILE        DS                  QUALIFIED
     D    Linha                1    140
     D    LRAMO                1      2
     D    LMOD                 3      4
     D    LSMOD                5      6
     D    LAPOL                7     13
     D    LANO                14     17
     D    LNPROC              18     22  0
     D    LDTSIN              23     30  0
     D    LDTABE              31     38  0
     D    LCENC               39     40
     D    LDTENC              41     48  0
     D    LDTRAB              49     56  0
     D    LIDS                57     57
     D    LNCASO              58     59
     D    LDTAVI              60     67  0
     D    LMARCT              68     85
     D    LMATRT              86     97
     D    LCOMPT              98    101
     D    LCIDST             102    104
     D    LDATENTDOC         105    112  0
     D    ICREGESTR          113    113
     D    ICFACEXC           114    114
     D    ICSUSFRA           115    115
     D    ICDECLEI           116    116
     D    CODPOST            117    120  0
     D    CODFREG            121    123  0
     D WIdx            S              3S 0
     D
     D*==> Campos de Trabalho
     D WComando        S            300A
     D WNomPasta       S             50A
     D WDatEnvio       S              8A
     D WAno            S              4A
     D WMes            S              2A
     D WDia            S              2A
     D WFolder         S               *
     D WFileInDir      S             40A
     D WLenFile        S              3S 0
     D WMsgErro        S              7A
     D WExtFile        S             21A
     D WFd             S             10I 0
     D WLine           S            400A
     D WLine_          S            400A
     D*===========================================================
      /FREE

       //-------------------------------------------------
       // Eliminar o ficheiro com os interfaces
       //-------------------------------------------------
       MONITOR;
          WComando = 'DLTF FILE(QTEMP/FINTT005)';
          ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
       ON-ERROR;
       // WMsgErro = 'XXXXXXX';
       ENDMON;

       //-------------------------------------------------
       // Criar ficheiro de Interface
       //-------------------------------------------------
       MONITOR;
          WComando = 'CRTPF FILE(QTEMP/FINTT005) SRCFILE(#JMPRDSRC/QDDSSRC) +
                      SIZE(*NOMAX)';
          ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
       ON-ERROR;
       // WMsgErro = 'XXXXXXX';
       ENDMON;
       OPEN FINTT005;

       //------------------------------------------------------
       // Carregar registos dos ficheiros enviados
       //------------------------------------------------------
       EXSR ExecEnviados;

       CLOSE FINTT005;
       *INLR = *ON;

       //=============================================================
       // Subrotina..: ExecEnviados
       // Descrição..: Localizar os ficheiros de Interface enviados
       //=============================================================
       BEGSR ExecEnviados;
          WNomPasta = '/interfaces/enviado/';
          WFolder = $OpenDir(%TRIM(WNomPasta));
          IF (WFolder <> *NULL);
             P_dirent = $ReadDir(WFolder);
             DOW (P_dirent <> *NULL);
                WFileInDir = %SUBST(dirent.name:1:dirent.namelen);
                WLenFile = %LEN(%TRIM(WFileInDir));
                IF (WLenFile >= 18);
                   IF (%SUBST(WFileInDir:1:9) = 'OPSINIST_');
                      WDatEnvio = %SUBST(WFileInDir:10:8);
                      WAno      = %SUBST(WDatEnvio:1:4);
                      WMes      = %SUBST(WDatEnvio:5:2);
                      IF (WMes > '12');
                          WMes = '0' + %SUBST(WDatEnvio:5:1);
                          WDia = %SUBST(WDatEnvio:6:2);
                      ELSE;
                          WMes = %SUBST(WDatEnvio:5:2);
                          WDia = %SUBST(WDatEnvio:7:2);
                      ENDIF;
                      IF (WDia > '31');
                         WDia = '0' + %SUBST(WDia:1:1);
                      ENDIF;
                      WDatEnvio = WAno + WMes + WDia;
                      EXSR ExecFile;
                   ENDIF;
                ENDIF;
                P_dirent = $ReadDir(WFolder);
             ENDDO;
          ENDIF;
       ENDSR;

       //===============================================================
       // Subrotina.: ExecFile
       // Descição..:
       //===============================================================
       BEGSR ExecFile;
          //-------------------------------------------------
          // Procede à Leitura do Interface
          //-------------------------------------------------
          WMsgErro = *BLANKS;
          WLine_ = '######';
          WFd = $Open(%TRIM(WNomPasta)+%TRIM(WFileInDir):O#RDONLY+
                      O#TEXTDATA+O#CCSID:S#IRGRP:37);
          IF (Wfd >= *ZEROS);
             DOW (ReadLine(WFd:%addr(WLine):%Size(WLine)) >= *ZEROS);
                IF (WLine <> *BLANKS);
                    REG#FILE.Linha = WLine;
                    LRAMO      = REG#FILE.LRAMO;
                    LMOD       = REG#FILE.LMOD;
                    LSMOD      = REG#FILE.LSMOD;
                    LAPOL      = REG#FILE.LAPOL;
                    LANO       = REG#FILE.LANO;
                    LNPROC     = REG#FILE.LNPROC;
                    LDTSIN     = REG#FILE.LDTSIN;
                    LDTABE     = REG#FILE.LDTABE;
                    LCENC      = REG#FILE.LCENC;
                    LDTENC     = REG#FILE.LDTENC;
                    LDTRAB     = REG#FILE.LDTRAB;
                    LIDS       = REG#FILE.LIDS;
                    LNCASO     = REG#FILE.LNCASO;
                    LDTAVI     = REG#FILE.LDTAVI;
                    LMARCT     = REG#FILE.LMARCT;
                    LMATRT     = REG#FILE.LMATRT;
                    LCOMPT     = REG#FILE.LCOMPT;
                    LCIDST     = REG#FILE.LCIDST;
                    LDATENTDOC = REG#FILE.LDATENTDOC;
                    ICREGESTR  = REG#FILE.ICREGESTR;
                    ICFACEXC   = REG#FILE.ICFACEXC;
                    ICSUSFRA   = REG#FILE.ICSUSFRA;
                    ICDECLEI   = REG#FILE.ICDECLEI;
                    CODPOST    = REG#FILE.CODPOST;
                    CODFREG    = REG#FILE.CODFREG;
                    LDTTRF     = %DEC(WDatEnvio:8:0);
                    WRITE RINTT005;
                ENDIF;
             ENDDO;
          ENDIF;
          $Close(WFd);
       ENDSR;
      /END-FREE
     P*=======================================================
     P* Processo..: ReadLine
     P* Descrição.: Ler uma linha do ficheiro de Texto
     P*             É um algoritmo artesanal.
     C*=======================================================
     P ReadLine        B
     D ReadLine        PI            10I 0
     D   fd                          10I 0 value
     D   text                          *   value
     D   maxlen                      10I 0 value
     D
     D rdbuf           S           1024A   static
     D rdpos           S             10I 0 static
     D rdlen           S             10I 0 static
     D
     D p_retstr        S               *
     D WRetStr         S          32766A   based(p_retstr)
     D Wlen            S             10I 0
     D*=================================================================
      /FREE
         WLen = 0;
         p_retstr = text;
         %subst(WRetStr:1:MaxLen) = *blanks;
         dow (1 = 1);

            //--------------------------------------------
            //  Carregar o texto
            //--------------------------------------------
            if (rdpos >= rdlen);
               rdpos = 0;
               rdlen=$read(fd:%addr(rdbuf):%size(rdbuf));

               if (rdlen < 1);
                  return -1;
               endif;
            endif;

            //--------------------------------------------
            //  Validar se é o fim da linha
            //--------------------------------------------
            rdpos = rdpos + 1;
            if (%subst(rdbuf:rdpos:1) = x'25');
               return  Wlen;
            endif;

            //--------------------------------------------
            // Se não for adiciona o texto à palavra.
            //--------------------------------------------
            if (%subst(rdbuf:rdpos:1) <> x'0d' and Wlen <> maxlen);
               Wlen += 1;
               %subst(WRetstr:Wlen:1) = %subst(rdbuf:rdpos:1);
            endif;
         enddo;
         return  Wlen;
      /END-FREE
     P                 E
