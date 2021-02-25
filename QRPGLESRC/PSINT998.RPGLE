     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO019
     H* Descrição.: Carregar Sinistros IDS Automóvel
     H*
     H* Para Compilar este programa fazer:
     H*    CRTPF FILE(QTEMP/FINTT030) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*
     H* Para correr este programa fazer:
     H*    CRTPF FILE(QTEMP/FINTT030) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*    CALL PGM(#JMPRDOBJ/PSINT998) PARM('QS36F/FICI150824')
     H*===========================================================
     FFICIDS    IF   E           K DISK    PREFIX(TMP_) USROPN
     F                                     EXTFILE(WExtFile)
     FFINTT030  UF A E           K DISK    PREFIX(SIN_)
     F*===========================================================
     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Campos de Trabalho
     D WExtFile        S             21A
     D
     C*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WExtFile
      /FREE
        //--------------------------------------------------------------
        // Proceder à abertura do ficheiro QS36F/FICI<aa><mm><dd>
        //--------------------------------------------------------------
        OPEN FICIDS;
        READ FICIDS;
        DOW (NOT %EOF(FICIDS));
           //---------------------------------------------
           // Carregar Interface e actualizar Base Dados
           //---------------------------------------------
           IF (TMP_NUMPRO > *ZEROS);
              SIN_TIPO    = TMP_TIPPRO;             // Tipo de Registo
              SIN_NUMPROC = %EDITC(TMP_NUMPRO:'X'); // Número do Processo (NNNNNYYYY)
              SIN_DATPROC = %EDITC(TMP_DATPRO:'X'); // Data do Processo
              SIN_DATSINO = %EDITC(TMP_DATSIN:'X'); // Data do Sinistro
              SIN_NUAPOL  = %EDITC(TMP_NUMAPO:'X'); // Número da Apólice
              SIN_MATRIC  = TMP_MATRIC;             // Número da Apólice
              SIN_IMPPROC = %EDITC(TMP_CUSPRO:'X'); // Custo do Processo
              SIN_DATENCE = %EDITC(TMP_DATENC:'X'); // Data de Encerramento
              SIN_NUMSEGU = %EDITC(TMP_NUMSEG:'X'); // Código do Segurado
              SIN_NOMSEGU = TMP_NOMSEG;             // Nome do Segurado
              WRITE RINTT030;
           ENDIF;
           READ FICIDS;
        ENDDO;
        CLOSE FICIDS;

        *INLR = *ON;
      /END-FREE
