     H ALWNULL(*USRCTL)  BNDDIR('JMDIR':'QC2LE') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *==========================================================
      * Autor .....: Luis Galhoz
      * Data ......: 2015/12/15
      * Programa ..: PGRLO003
      * Objectivo .: Extrair Metadados dos ficheiros tempor�rios
      *              e actualizar hist�rico.
      * ---------------------------------------------------------
      * Descri��o .: Para o efeito procede � leitura dos ficheiro
      *              tempor�rios, e por cada aplica o respectivo
      *              formato registo, para extrac��o dos campos.
      *==========================================================
     FBASEAPOL  IF   F 1000        DISK    USROPN
     FFGRLP026  UF A E           K DISK    PREFIX(GRL_) COMMIT                  Metadados
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D
     D*==> Converter um valor Alfa num Exadecimal
     D CharToHex       PR                  ExtProc( 'cvthc' )
     D  CTHHex                         *   value
     D  CTHChar                        *   value
     D  CTHCharSize                  10I 0 value
     D
     D*==> Converter Packeado num n�mero
     D CvtPacked       PR            15A
     D   Valor                       15A   Const
     D
     D*==> Actualizar a nova estrutura
     D UpdNewDB        PR                  EXTPGM('PCVSO002')
     D   CdProd                       5S 0 CONST
     D   DatCriacao                   8S 0 CONST
     D   Registo                       *   CONST
     D
     D*==> Estrutura dos Campos dos Ficheiro
     D LST#Fields      DS                  QUALIFIED DIM(25)
     D   NoField                     15A   INZ(*BLANKS)
     D   PosIni                       4S 0 INZ(*ZEROS)
     D   PosFim                       4S 0 INZ(*ZEROS)
     D   TpCampo                      2A   INZ(*BLANKS)
     D   CdField                      5S 0 INZ(*ZEROS)
     D   IsToExtract                   N   INZ(*OFF)
     D   IsEmpty                       N   INZ(*ON)
     D WIdxLst         S              2S 0 INZ(*ZEROS)
     D
     D*==> Campos de Trabalho
     D WTemporario     S              5S 0 INZ(*ZEROS)
     D WDefinicao      S              5S 0 INZ(*ZEROS)
     D WNumRegFile     S              5S 0 INZ(*ZEROS)
     D WNumRegField    S              5S 0 INZ(*ZEROS)
     D WDRegChar       S              8A   INZ(*BLANKS)
     D WDRegNum        S              8S 0 INZ(*ZEROS)
     D WCdFile         S              5S 0 INZ(*ZEROS)
     D WCdBatch        S              9S 0 INZ(*ZEROS)
     D WNuSequ         S              5S 0
     D WFileName       S             10A
     D WLibName        S             10A
     D WIsFileOpen     S               N
     D WValField       S             15A
     D WSeqRecord      S              9S 0
     D WSeqField       S              5S 0
     D WPosIni         S              4S 0 INZ(*ZEROS)
     D WPosFim         S              4S 0 INZ(*ZEROS)
     D WIcMDados       S              1A   INZ(*BLANKS)
     D WTpCampo        S              2A   INZ(*BLANKS)
     I*========================================================
     IBASEAPOL  AA  11
     I                                  1 1000  REGIS
     C*========================================================
      /FREE
        //------------------------------------------------------
        // Actualizar o adicionar novos Registos
        //------------------------------------------------------
        WTemporario = $SetFiles('FGRLP023');
        $SetFields('ICMDADOS,CDFILE,NOLIBL,NOFILE,CHAR(DTCRIA),CHAR(HRCRIA),+
                    CDBATCH,NUSEQU');
        $SetCriteria('CDFILE IN (5, 6)');
        WNumRegFile = 1;
        DOW ($GetRecord(WNumRegFile) > *ZEROS);
           WDRegChar = $GetField('CHAR(DTCRIA)');
           WDRegChar = %SUBST(WDRegChar:1:2)+
                       %SUBST(WDRegChar:4:2)+
                       %SUBST(WDRegChar:7:2);
           WDRegNum  = $CvtData(%DEC(WDRegChar:6:0):'DMY':'YYMD');
           WFileName = $GetField('NOFILE');
           WLibName  = $GetField('NOLIBL');
           WCdFile   = $GetfieldNum('CDFILE');
           WCdBatch  = $GetfieldNum('CDBATCH');
           WNuSequ   = $GetfieldNum('NUSEQU');
           WIcMDados = $GetField('ICMDADOS');

           //--------------------------------------------------------
           // S� extrai dados se ainda n�o foram actalizados
           //--------------------------------------------------------
           EXSR OpenFiles;
           IF (WIsFileOpen);
              WSeqRecord = 1;
              READ BASEAPOL;
              DOW (NOT %EOF(BASEAPOL));
                 EXSR ExtData;
                 WSeqRecord += 1;
                 READ BASEAPOL;
              ENDDO;
              COMMIT;
              CLOSE BASEAPOL;
           ENDIF;
           WNumRegFile += 1;
        ENDDO;
        $RmvDBSession(WTemporario);

        *INLR = *ON;
        RETURN;

        //=========================================================
        // Subrotina ..: OpenFiles
        // Descri��o ..: Abrir Ficheiros para proceder � extrac��o
        //               de Metadados.
        //=========================================================
        BEGSR Openfiles;
           WIsFileOpen = *OFF;
           MONITOR;
              //-----------------------------------------
              // Obter a defini��o dos Campos
              //-----------------------------------------
              EXSR GetDefinition;
              IF (LST#Fields(1).IsEmpty);
                 LEAVESR;  // N�o existe ent�o sai
              ENDIF;

              //-----------------------------------------
              // abrir Ficheiro Tempor�rio
              //-----------------------------------------
              WStdCmd = 'OVRDBF FILE(BASEAPOL) TOFILE('+%TRIM(WlibName)+'/'+
                                  %TRIM(WFileName)+')';
              WStdLen = %LEN(WStdCmd);
              EXEC(WStdCmd:WStdLen);
              OPEN BASEAPOL;
              WIsFileOpen = *ON;
           ON-ERROR;
              WIsFileOpen = *OFF;
           ENDMON;
        ENDSR;

        //=========================================================
        // Subrotina ..: GetDefinition
        // Descri��o ..: Obter a defini��o dos campos, a partir da
        //               configura��o.
        //=========================================================
        BEGSR GetDefinition;
           WNumRegField = 1;
           WIdxLst      = 1;
           LST#Fields(WIdxLst).IsEmpty = *ON;
           WDefinicao = $SetFiles('FGRLP024 AS A INNER JOIN FGRLP025 AS B +
                                   ON (A.CDFILE = B.CDFILE)');
           $SetFields('FGRLP024':'A');
           $SetFields('FGRLP025':'B':*OFF);
           $SetCriteria('(A.CDFILE = '''+%CHAR(WCdFile)+''')');
           DOW ($GetRecord(WNumRegField) > *ZEROS);
              LST#Fields(WIdxLst).PosIni  = $GetFieldNum('B.POSINI');
              LST#Fields(WIdxLst).PosFim  = $GetFieldNum('B.POSFIM');
              LST#Fields(WIdxLst).TpCampo = $GetField('B.TPFIELD');
              LST#Fields(WIdxLst).CdField = $GetFieldNum('B.CDFIELD');
              LST#Fields(WIdxLst).IsToExtract = *OFF;
              IF ($GetField('B.ISTOEXTR') = 'S');
                 LST#Fields(WIdxLst).IsToExtract = *ON;
              ENDIF;
              LST#Fields(WIdxLst).CdField = $GetFieldNum('B.CDFIELD');
              LST#Fields(WIdxLst).IsEmpty = *OFF;
              WIdxLst += 1;
              LST#Fields(WIdxLst).IsEmpty = *ON;
              WNumRegField += 1;
           ENDDO;
           $RmvDBSession(WDefinicao);
        ENDSR;

        //=========================================================
        // Subrotina ..: ExtData
        // Descri��o ..: Extrair os Metadados do Ficheiro.
        //=========================================================
        BEGSR ExtData;
           WSeqField = 4;
           WIdxLst   = 1;
           DOW (NOT LST#Fields(WIdxLst).IsEmpty);
              IF (LST#Fields(WIdxLst).IsToExtract);
                 //--------------------------------------------------
                 // Extrair os valores do Campo
                 //--------------------------------------------------
                 WPosIni = LST#Fields(WIdxLst).PosIni;
                 WPosFim = LST#Fields(WIdxLst).PosFim;
                 WTpCampo = LST#Fields(WIdxLst).TpCampo;
                 WValField = %SUBST(REGIS:WPosIni:(WPosFim-WPosIni)+1);

                 //--------------------------------------------------
                 // Actualizar Tabela
                 //--------------------------------------------------
                 GRL_CDBATCH   = WCdBatch;
                 GRL_NUSEQU    = WNuSequ;
                 GRL_NUMREG    = WSeqRecord;
                 GRL_SEQFLD    = WSeqField;
                 GRL_CDFIELD   = LST#Fields(WIdxLst).CdField;
                 GRL_VALFIELD  = CvtPacked(WValField);
                 GRL_GRLO02601 = %DATE(20000101);
                 GRL_GRLO02602 = %DATE(29991231);
                 WRITE RGRLP026;

                 WSeqField += 1;
              ENDIF;
              WidxLst   += 1;
           ENDDO;
        ENDSR;
      /END-FREE
     P*=======================================================
     P* Processo..: CvtPacked
     P* Descri��o.: Solu��o de recurso para converter packeados
     P*=======================================================
     P CvtPacked       B                   EXPORT
     D                 PI            15A
     D   Valor                       15A   Const
     D
     D*==> Declara��o das Vari�veis
     D wValor          S             15A
     D wResult         S             15A
     D wHexa           S             15A
     D wIdx            S              5S 0
     D*===========================================================
      /FREE
         wValor  = VALOR;
         wResult = *BLANKS;

         CharToHex (%addr(wHexa):%addr(wValor):%size(wHexa));
         wValor = wHexa;
         FOR wIdx = 1 BY 1 TO 15;
            IF (%SUBST(wValor:wIdx:1) = 'F');
               LEAVE;
            ELSE;
               wResult = %TRIM(wResult) + %SUBST(wValor:wIdx:1);
            ENDIF;
         ENDFOR;

         RETURN wResult;
      /END-FREE
     P CvtPacked       E
