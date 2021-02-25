     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO050
     H* Descrição.: Gerar interfaces para os Mediadores, para o efeito
     H*             Localizar os ficheiros temporários gerados nos
     H*             Sistema, e por cada novo que localizar, procede
     H*             à extracção dos registos.
     H*
     H* Para Compilar este programa fazer:
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT020)
     H*===========================================================
     F*==> Temporários a Processar
     FRECIBOS   IF   F  550        DISK    USROPN EXTFILE(WGuias)
     FGCOBR     IF   E           K DISK    PREFIX(REC_)
     F
     F*==> Informação sobre os Mediadores Alvo
     FFINTP055  UF A E           K DISK    PREFIX(CTL_)
     FFINTP056  UF A E           K DISK    PREFIX(CTL_)
     FFINTP050  IF   E           K DISK    PREFIX(INT_)
     FFINTP051  UF A E           K DISK    PREFIX(INT_)
     FFINTP052  UF A E           K DISK    PREFIX(INT_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Formato de Registo dos Recibos
     D K#RECIBO        DS                  QUALIFIED
     D   NRec                  1      9S 0
     D   Ramo                 10     11S 0
     D   Apol                 12     18S 0
     D   Segur               182    184P 0
     D   TotRec              239    243P 2
     D   Com                 251    255P 2
     D   JMCOM1              465    468P 2
     D   JMCOM2              469    472P 2
     D   JMM1                503    505S 0
     D   DTINIC              186    191S 0
     D   DTFIMC              193    198S 0
     D
     D*==> Campos de Trabalho
     D WGuias          S             10A
     D WDatInicio      S              8S 0
     D WSessTempor     S              5S 0
     D WIdx            S              3S 0
     D WLastState      S              3A
     D WCdProcess      S              5S 0
     D
     D*==> Declaração de Constantes
     D STS#Aberto      C                   'OPN'
     D STS#SemEfeito   C                   'SEF'
     D STS#Enviado     C                   'ENV'
     D STS#Cancelado   C                   'CAN'
     D*===========================================================
      /FREE
        //-----------------------------------------------------------------
        // Seleccionar a data do último processo de extracção Mediadores
        //-----------------------------------------------------------------
        EXSR OpenControlo;
        EXSR SelTemporarios;

        //================================================================
        // Subrotina.: SelTemporarios
        // objectivo.: Processar ficheiros Temporários
        //================================================================
        BEGSR SelTemporarios;
           //-----------------------------------------------------------------
           // Seleccionar Ficheiros Temporários gerados depois da data
           //-----------------------------------------------------------------
           WSessTempor = $SetFiles('FGRLP020 B INNER JOIN FGRLP023 F ON +
                                    (B.CDBATCH = F.CDBATCH)');
           $SetCriteria('(B.CDFILE = 12)');
           $SetFields('A.CDBATCH,B.NUSEQU,B.NOFILE,B.NOLIBL');
           WIdx = 1;
           DOW ($GetRecord(WIdx) > *ZEROS);
              EXSR ExtFiles;
              Widx += 1;
           ENDDO;
           RETURN;
        ENDSR;

        //================================================================
        // Subrotina.: ExtFiles
        // objectivo.: Ler os ficheiros a Processar
        //================================================================
        BEGSR ExtFiles;
           // Validar se o ficheiro já foi processado
           // Identificar o tipo de ficheiro
           // Extrair os registos e actualizar as Interfaces
        ENDSR;

        //================================================================
        // Subrotina.: OpenControlo
        // objectivo.: Abrir o controlo dos Interfaces
        //================================================================
        BEGSR OpenControlo;
           SETLL (*LOVAL) RINTP055;
           READP(N) RINTP0055;
           IF (%FOUND(FINTP055));
              WCdProcess = CTL_CDPROCE;
              WLastState = CTL_CDESTADO;
              IF (CTL_CDESTADO = STS#Aberto);
                 WDatInicio = CTL_DTINICIO;
              ELSEIF (CTL_ESTADO = STS#SemEfeito);
                 WDatInicio = CTL_DTINICIO;
              ELSE;
                 WDatInicio = CTL_DTTERMO;
              ENDIF;
           ENDIF;

           IF (WLastState <> 'OPN');
              WCdProcess = $GetNextKey('FINTP055':'CDPROCE');
              CTL_CDPROCE = WCdProcess;
              CTL_CDESTADO = STS#Aberto;
              CTL_DTINICIO = WDatInicio;
              CTL_DTTERMO = 99999999;
              CTL_DTULTPRC = $DatToInt(%DATE());
              CTL_INT05401 = %DATE();
              CTL_INT05402 = %DATE();
              WRITE RINTP055;
           ENDIF;
        ENDSR;

        //================================================================
        // Subrotina.: CloseControlo
        // objectivo.: Fechar o ficheiro de Controlo
        //================================================================
        BEGSR CloseControlo;
        ENDSR;
      /END-FREE
