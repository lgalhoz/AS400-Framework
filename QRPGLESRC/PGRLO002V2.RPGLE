     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PGRLO001
     H* Descrição.: Extracção de Metadados dos Ficheiros Temporários.
     H*==================================================================
     FFGRLP023  IF   E           K DISK    PREFIX(GRL_)                         Temporários
     FFGRLP026  IF   E           K DISK    PREFIX(GRL_)                         Metadados
     FFGRLP025  IF   E           K DISK    PREFIX(TPL_)                         Definição dos Campos
     FTMPFILE   IF   F 1000        DISK    USROPN
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Lista de Campos do Ficheiro
     D LST#Fields      DS                  QUALIFIED DIM(50)
     D    CdField                     5S 0
     D    NoField                    10A
     D    PosIni                      5S 0
     D    PosFim                      5S 0
     D    Dimens                      5S 0
     D    TpCampo                     1A
     D    IsEmpty                      N
     D WIdx            S              5S 0
     D
     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D WNoFile         S             10A
     D WNoLib          S             10A
     D WCdBatch        S              9S 0
     D WCdFile         S              5S 0
     D WNuSequ         S              5S 0
     D WSeqField       S              5S 0
     D WValField       S             20A
     D*===================================================================
     ITMPFILE   AA  11
     I                                  1  700  REGISTO
     I*===================================================================
      /FREE
        SETLL *LOVAL RGRLP023;
        READ RGRLP023;
        DOW (NOT %EOF(RGRLP023));
           IF (GRL_ICHISTOR <> 'S');
              WCdBatch = GRL_CDBATCH;
              WNuSequ  = GRL_NUSEQU;
              WCdFile  = GRL_CDFILE;
              EXSR Extrair;
           ENDIF;
           READ RGRLP023;
        ENDDO;

        *INLR = *ON;

       //===================================================================
       // Subrotina.: Extrair
       // Objectivo.: Extrair metadados
       //===================================================================
       BEGSR Extrair;
          //--------------------------------------------------------
          // Carregar lista de Campos
          //--------------------------------------------------------
          WIdx = 1;
          LST#Fields(WIdx).IsEmpty = *ON;
          SETLL (GRL_CDFILE) RGRLP025;
          READE (GRL_CDFILE) RGRLP025;
          DOW (NOT %EOF(RGRLP025));
             LST#Fields(WIdx).CdField = TPL_NuSequ;
             LST#Fields(WIdx).NoField = TPL_NoField;
             LST#Fields(WIdx).PosIni  = TPL_PosIni;
             LST#Fields(WIdx).PosFim  = TPL_PosFim;
             LST#Fields(WIdx).Dimens  = LST#Fields(WIdx).PosFim -
                                        LST#Fields(WIdx).PosIni + 1;
             LST#Fields(WIdx).TpCampo = TPL_TpCampo;
             LST#Fields(WIdx).IsEmpty = *OFF;
             WIdx += 1;
             READE (GRL_CDFILE) RGRLP025;
          ENDDO;
          IF (LST#Fields(1).IsEmpty);
             LEAVESR;
          ENDIF;

          //--------------------------------------------------------
          // Extair os dados
          //--------------------------------------------------------
          MONITOR;
             WNoFile = GRL_NOFILE;
             WNoLib  = GRL_NOLIBL;
             WStdCmd = 'OVRDBF FILE(TMPFILE) TOFILE('+%TRIM(WNoLib)+
                       '/'+%TRIM(WNoFile)+')';
             WStdLen = %LEN(WStdCmd);
             EXEC(WStdCmd:WStdLen);
          ON-ERROR;
             LEAVESR;
          ENDMON;

          WSeqField = 1;
          OPEN TMPFILE;
          READ TMPFILE;
          DOW (NOT %EOF(TMPFILE));
             WValField = %SUBST(REGISTO:LST#Fields(WIdx).PosIni:
                                    LST#Fields(WIdx).Dimens);
             GRL_CDBATCH  = WCdBatch;
             GRL_NUSEQU   = WNuSequ;
             GRL_SEQFLD   = WSeqField;
             GRL_CDFILE   = WCdFile;
             GRL_CDFIELD  = WCdField;
             GRL_VALFIELD = WValField;
             WRITE RGRLP026;
             WSeqField += 1;
             READ TMPFILE;
          ENDDO;
          CLOSE TMPFILE;
       ENDSR;
      /END-FREE
