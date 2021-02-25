     H*====================================================================
     H* Programa...: PSINP102
     H* Objectivo..: Classificar os Intervinientes do Processo de Sinistro
     H*              para o efeito tentar localizar dentro dos registos de
     H*              Intervinientes alguns já classificados, e se não
     H*              encontrar então procurar no ficheiro de Segurados e
     H*              Empresas.
     H*====================================================================
     H  ALWNULL(*USRCTL) DFTACTGRP(*NO)
     H*====================================================================
     FGSININT   IF   E           K DISK    EXTFILE('G.SININT') PREFIX(INT_)
     FCLNUM     UF   F   12        DISK    EXTFILE('CL.NUM')
     FSD1PE     UF A E           K DISK    EXTFILE('SD.1.PE') PREFIX(C)
     FGFSEG     UF A E           K DISK    PREFIX(S)
     FFSINP105  IF   E           K DISK    PREFIX(PRO_)
     FFSINP114  UF   E           K DISK
     FFSINP11401IF   E           K DISK    RENAME(RSINP114:RSINAS)
     FFSINP11402IF   E           K DISK    RENAME(RSINP114:RSINMS)
     F*====================================================================
     D*==> Chaves da parte do Registo de AS/400
     D WAaProc         S                   LIKE(AAPROC)
     D WNuProc         S                   LIKE(NUPROC)
     D WSequencia      S                   LIKE(SEQUENCIA)
     D WAsTpInt        S                   LIKE(ASTPINT)
     D WAsSqInt        S                   LIKE(ASSQINT)
     D WNumInt         S                   LIKE(NUMINT)
     D WLastSeg        S                   LIKE(NUMINT)
     D WNomeOr         S                   LIKE(ASNOME)
     D WMorada         S                   LIKE(CMORADA)
     D WTelfone        S                   LIKE(CTELEF)
     D WCodPostal      S                   LIKE(CCPOST)
     D WNomeTF         S                   LIKE(ASNOME)
     D WAsNome         S                   LIKE(ASNOME)
     D WMsNome         S                   LIKE(MSNOME)
     D WIcLigar        S               N
     D WDatSelect      S              8S 0
     D WDatInicio      S              8S 0
     D WHorInicio      S              6S 0
     D P_DatInicio     S              8S 0
     D
     D IgnoreInt       DS                  QUALIFIED DIM(15)
     D    Nome                       40A   INZ(*BLANKS)
     D WIdx            S              5S 0
     D WNome           S                   LIKE(INT_NOMSEG)
     D
     D*==> Função para Comparar Nomes
     D ValName         PR              N
     D    WNome01                    50A   CONST
     D    WNome02                    50A   CONST
     D    WRigor                      3S 2 CONST
     D
     D*==> Validar com que erro os nomes são iguais
     D ValPrecisao     PR             3S 2
     D    WNome01                    50A   CONST
     D    WNome02                    50A   CONST
     D    WRigor                      3S 2 CONST
     D
     D ProcCodif       DS
     D   ProcAno               1      2S 0
     D   PDifer                3      3S 0
     D   ProcNum               4      7S 0
     D
     D RecCorr         DS                   LIKEREC(MPESNPF1:*OUTPUT)
     D RecSeg          DS                   LIKEREC(FSEGF1:*OUTPUT)
     D*====================================================================
     ICLNUM     AA
     I                                  2    6 0NSOC
     I                                  8   12 0NIND
     I*====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    P_DatInicio
     C
      /FREE
       //----------------------------------------------------
       // Formatar o parâmetro de Entrada
       //----------------------------------------------------
       WDatSelect = 20120101;
       MONITOR;
          WDatSelect = P_DatInicio;
       ON-ERROR;
       ENDMON;

       //----------------------------------------------------
       // Só classifica processo abertos ou encerrados em 2011
       //----------------------------------------------------
       SETLL *LOVAL RSINP105;
       READ RSINP105;
       DOW (NOT %EOF());
          IF (%NULLIND(PRO_SIN10506)=*ON) OR
            (PRO_SIN10506 >= %DATE(WDatSelect:*ISO));
             IF (PRO_SIN10508 = -1);
                ProcCodif = PRO_NRPROC;
                MONITOR;
                   WAaProc = ProcAno;
                   WNuProc = ProcNum;
                   EXSR CrtSegNum;
                ON-ERROR;
                ENDMON;
             ENDIF;
           ENDIF;
           READ RSINP105;
       ENDDO;
       *INLR = *OFF;
       RETURN;

       //================================================================
       // Subrotina.: CrtSegNum
       // Objectivo.: Criar dados para o Segurado. Para o efeito procura
       //             se já existem dados criados. Se já existirem
       //             então utiliza o mesmo número, caso contrário
       //             atribui um número diferente.
       //================================================================
       BEGSR CrtSegNum;
          SETLL (WAaProc:WNuProc) RSINAS;
          READ RSINAS;
          DOW (NOT %EOF);
             IF (AAPROC <> WAaProc) OR (NUPROC <> WNuProc);
                LEAVE;
             ENDIF;

             //------------------------------------------
             // Obter o código dentro do mesmo processo
             //------------------------------------------
             IF ((NUMINT = *ZEROS) AND (MSNOME <> *BLANKS));
                WAsTpInt = ASTPINT;
                WAsSqInt = ASSQINT;
                WSequencia = SEQUENCIA;
                WNomeOr = MSNOME;   // Nome Original
                WIcLigar = *OFF;
                EXSR getProCode;

                //------------------------------------------
                // Criar um novo código de Interviniente
                //------------------------------------------
                IF (NOT WIcLigar);
                   EXSR CrtEntidade;
                ENDIF;

                //------------------------------------------
                // Actualizar o número do Interviniente
                //------------------------------------------
                CHAIN (WAaProc:WNuProc:Wsequencia) RSINP114;
                NUMINT    = WNumInt;
                DATALTER  = %DATE();
                HORALTER  = %TIME();
                CODESTAD  = 'A';
                UPDATE RSINP114 %FIELDS(NUMINT:DATALTER:HORALTER:CODESTAD);
             ENDIF;
             READ RSINAS;
          ENDDO;
       ENDSR;

       //================================================================
       // Subrotina.: getProCode
       // Objectivo.: Obter o Código de Pessoa ou Empresa a partir de
       //             um registo de Interviniente previamente classificado
       //             e dentro do mesmo processo de Sinistro.
       // Observação: Igual com 50% de precisão.
       //================================================================
       BEGSR getProCode;
          SETLL (WAaProc:WNuProc) RSINMS;
          READ RSINMS;
          DOW (NOT %EOF);
             IF (WAaProc <> AAPROC) OR (WNuProc <> NUPROC);
                LEAVE;
             ENDIF;

             //------------------------------------------
             //
             //------------------------------------------
             IF (NUMINT <> *ZEROS);
                IF (ASNOME = *BLANKS);
                   WNomeTF = MSNOME;
                ELSE;
                   WNomeTF = ASNOME;
                ENDIF;

                IF ValName(WNomeTF:WNomeOr:0.5);
                   WNumInt = NUMINT;
                   WIcligar = *ON;
                   LEAVE;
                ENDIF;
             ENDIF;
             READ RSINMS;
          ENDDO;
       ENDSR;

       //================================================================
       // Subrotina.: CrtEntidade
       // Objectivo.: Criar um registo para o Interviniente no ficheiro de
       //             Pessoas e Empresas.
       //================================================================
       BEGSR CrtEntidade;
          //----------------------
          // Obter Numeração
          //----------------------
          CHAIN 1 CLNUM;
          NIND += 1;
          WNumInt = NIND;
          EXCEPT UPDNUMERO;

          //-------------------------------------
          // Formatar Data e Hora de Inicio
          //-------------------------------------
          WDatInicio = (%SubDt(%Date():*Years) * 10000) +
                     (%SubDt(%Date():*Months) * 100) +
                      %SubDt(%Date():*Days);
          WHorInicio = (%SubDt(%Time():*Hours) * 10000) +
                     (%SubDt(%Time():*Minutes) * 100) +
                      %SubDt(%Time():*Seconds);

          //-------------------------------------
          // Obter moradas ou telefones
          //-------------------------------------
          WTelfone   = *BLANKS;
          WMorada    = *BLANKS;
          WCodPostal = *ZEROS;
          IF (ASNOME <> *BLANKS);
             CHAIN (WAaProc:WNuProc:WAsTpInt:WAsSqInt) GSININTF;
             IF (%FOUND());
                IF (INT_CPSEG = *ZEROS);
                   WTelfone   = INT_MORSEG;
                ELSE;
                   WMorada    = INT_MORSEG;
                   WCodPostal = INT_CPSEG;
                ENDIF;
             ENDIF;
          ENDIF;

          //-------------------------------------
          // Criar o registo (Seguradora)
          //-------------------------------------
          RecSeg.SNSEG   = WNumInt;
          RecSeg.SNOM    = WNomeOr;
          RecSeg.SCORSEL = 'N';
          RecSeg.SPILOTO = 'N';
          RecSeg.SSTESP  = 'N';
          RecSeg.SUSERID = 'INFOR';
          RecSeg.SDATALT = WDatInicio;
          RecSeg.STIMALT = WHorInicio;
          RecSeg.SNIF    = 999999990;
          RecSeg.SDTNASC = 20110101;
          RecSeg.STELEF  = WTelfone;
          RecSeg.SMORAD  = WMorada;
          RecSeg.SCPOST  = WCodPostal;
          RecSeg.SBI     = *ZEROS;
          RecSeg.STLM    = *ZEROS;
          WRITE FSEGF1 RecSeg;

          //-------------------------------------
          // Criar o registo (Corretor)
          //-------------------------------------
          RecCorr.CCREG   = 10;
          RecCorr.CNSEG   = WNumInt;
          RecCorr.CNOME   = WNomeOR;
          RecCorr.CNRC    = 999999990;
          RecCorr.CDUA    = WDatInicio - 20000000;
          RecCorr.CUUA    = 'INFOR';
          RecCorr.CTELEF  = WTelfone;
          RecCorr.CMORADA = WMorada;
          RecCorr.CCPOST  = WCodPostal;
          RecCorr.CCSIT   = *ZEROS;
          RecCorr.CCODALT = *ZEROS;
          RecCorr.CRACTIV = *ZEROS;
          RecCorr.CCPC    = *ZEROS;
          RecCorr.CCDINT  = *ZEROS;
          RecCorr.CNUA    = 1;
          RecCorr.CPAS    = *ZEROS;
          RecCorr.CVAS    = *ZEROS;
          RecCorr.CDTE    = *ZEROS;
          RecCorr.CTLM    = *ZEROS;
          RecCorr.CDIFP   = *ZEROS;
          WRITE MPESNPF1 RecCorr;

       ENDSR;
      /END-FREE
     OCLNUM     E            UPDNUMERO
     O                       NSOC                 6
     O                       NIND                12
      /COPY SOAPSERV/QRPGLESRC,RSINP100
