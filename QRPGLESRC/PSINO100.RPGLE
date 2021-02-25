     H*====================================================================
     H* Programa...: PSINP100
     H* Objectivo..: Actualizar relação entre os Intervinientes existentes
     H*              no AS/400 e MS Access.
     H*
     H*====================================================================
     H  ALWNULL(*USRCTL) DFTACTGRP(*NO)
     H*====================================================================
     FGSININT   IF   E           K DISK    EXTFILE('G.SININT') PREFIX(INT_)
     FGSINPRO   IF   E           K DISK    EXTFILE('G.SINPRO') PREFIX(PRO_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FFSINP102  IF   E           K DISK    PREFIX(C_)
     FFSINP103  IF   E           K DISK    PREFIX(M_)
     FFSINP104  IF   E           K DISK    PREFIX(T_)
     FFSINP114  UF A E           K DISK    RENAME(RSINP114:RSIN)
     FFSINP11401IF   E           K DISK    RENAME(RSINP114:RSINAS)
     FFSINP11402IF   E           K DISK    RENAME(RSINP114:RSINMS)
     F*====================================================================
     D*==> Chaves da parte do Registo de AS/400
     D WAaProcBk       S                   LIKE(AAPROC) INZ(*ZEROS)
     D WNuProcBk       S                   LIKE(NUPROC) INZ(*ZEROS)
     D WAaProc         S                   LIKE(AAPROC)
     D WNuProc         S                   LIKE(NUPROC)
     D WSequencia      S                   LIKE(SEQUENCIA)
     D WNumInt         S                   LIKE(NUMINT)
     D WAsTpInt        S                   LIKE(ASTPINT)
     D WAsTpSin        S                   LIKE(ASTPSIN)
     D WAsSqInt        S                   LIKE(ASSQINT)
     D WAsNome         S                   LIKE(ASNOME)
     D WAsNmMat        S                   LIKE(ASNMMAT)
     D WAsNuApo        S                   LIKE(ASNUAPO)
     D WMsTpInt        S                   LIKE(MSTPINT)
     D WMsTpSin        S                   LIKE(MSTPSIN)
     D WMsSqInt        S                   LIKE(MSSQINT)
     D WMsNome         S                   LIKE(MSNOME)
     D WMsNmMat        S                   LIKE(MSNMMAT)
     D WMsOrigem       S                   LIKE(MSORIGE)
     D WLastReg        S                   LIKE(SEQUENCIA)
     D WSavNUMINT      S                   LIKE(M_NUMINT)
     D WsavTipInt      S                   LIKE(M_TIPINT)
     D WIcLigar        S               N
     D WIcSegurado     S               N
     D WIcChanged      S               N
     D WIcEncontrou    S               N
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
     D*====================================================================
      /FREE
       EXSR UpdSegurado;// Obter Informação do Segurado
       EXSR UpdMSInter; // Obter Correspondência no MS Access
       EXSR Validate;   // Reclassificar

       UNLOCK FSINP114;

       *INLR = *OFF;
       RETURN;

       //================================================================
       // Subrotina.: UpdSegurado
       // Objectivo.: Actualizar Informação do Segurado
       //             Seleccionar apenas apólices do DL83/2006
       //================================================================
       BEGSR UpdSegurado;
          SETLL *LOVAL GSINPROF;
          READ GSINPROF;
          DOW (NOT %EOF());
             WAaProc  = PRO_AAPROC;  // Ano do Processo
             WNuProc  = PRO_NSEQ;    // Sequencia do Processo
             WAsSqInt = 1;           // Sequencia do Interviniente
             IF ((PRO_ACORDO = 'S') AND (PRO_NSEQ >= 6000));
                CHAIN (WAaProc:WNuProc:1) RSIN;
                IF (NOT %FOUND());
                   EXSR UpdASInter;  // Actualizar Intervenientes no AS400
                ENDIF;
             ENDIF;
             READ GSINPROF;
          ENDDO;
       ENDSR;

       //================================================================
       // Subrotina.: UpdASInter
       // Objectivo.: Actualizar Intervinientes do Processo AS/400
       //================================================================
       BEGSR UpdASInter;
          WIcSegurado = *OFF;
          WLastReg    = 1;
          SETLL (WAaProc:WNuProc) GSININTF;
          READ GSININTF;
          DOW (NOT %EOF());
             IF (WAaProc <> INT_AAPROC) OR (WNuProc <> INT_NSEQ);
                LEAVE;
             ENDIF;

             //-------------------------------------------
             // Parameterizar tipo de Sinistro
             //-------------------------------------------
             IF (INT_TPDN = 'F');
                WAsTpSin = 2;  // Danos Corporais
             ELSE;
                WAsTpSin = 1;  // Danos Materiais
             ENDIF;

             WAsTpInt = INT_CODINT;  // Tipo Interveniente
             WAsSqInt = INT_NREG;    // Sequencia do Interviniente
             IF (NOT WIcSegurado AND INT_CODINT = 'S');
                WIcSegurado = *ON;
                WSequencia  = 1;
                NUMINT    = PRO_NSEG;    // Número Segurado
                CHAIN (PRO_NSEG) FSEGF1;
                IF (%FOUND);             // Nome do Segurado
                   ASNOME  = SEG_NOM;
                ELSE;
                   ASNOME  = PRO_NOMPAR;
               ENDIF;
             ELSE;
                WLastReg  += 1;
                WSequencia = WLastReg;
                IF (INT_CODINT = 'S');
                   NUMINT = PRO_NSEG;    // Número Segurado
                ELSE;
                   NUMINT     = *ZEROS;
                ENDIF;
                EXSR GetInterv;  // Localizar nome do Interviniente
                ASNOME     = WNome;
             ENDIF;

             ASNUAPO   = INT_APOL3; // %CHAR(INT_CP3)+'-'+INT_APOL3;
             ASNMMAT   = INT_IMATR1;
             ASMARCA   = INT_IMARC1;
             ASCATV    = *ZEROS;
             ASCOMP    = INT_CP3;
             EXSR AddInterv;
             READ GSININTF;
          ENDDO;

          //-------------------------------------------------
          // Criar os dados do Segurado se não encontrou
          //-------------------------------------------------
          IF (NOT WIcSegurado);
             WSequencia = 1;
             WAsTpInt   = 'S';
             WAsSqInt   = *ZEROS;
             WASTPSIN   = *ZEROS;
             NUMINT     = PRO_NSEG;    // Número Segurado
             ASNUAPO    = %CHAR(PRO_RM) + '-' +%CHAR(PRO_AP);
             ASNMMAT    = PRO_MATR1;   // Matricula
             ASMARCA    = PRO_MARC1;   // Marca
             ASCATV     = PRO_CATV1;   // Categoria
             ASCOMP     = *ZEROS;      // Lusitania Azul
             CHAIN (PRO_NSEG) FSEGF1;
             IF (%FOUND);             // Nome do Segurado
                ASNOME  = SEG_NOM;
             ELSE;
                ASNOME  = PRO_NOMPAR;
             ENDIF;
             EXSR AddInterv;
          ENDIF;
       ENDSR;

       //================================================================
       // Subrotina.: ADDInterv
       // Objectivo.: Obter o Nome do Interveninente.
       //================================================================
       BEGSR AddInterv;
          //-------------------------------------------
          // Gravar o registo
          //-------------------------------------------
          AAPROC    = WaaProc;
          NUPROC    = WNuProc;
          SEQUENCIA = WSequencia;
          ASTPINT   = WAsTpInt;
          ASSQINT   = WAsSqInt;
          ASTPSIN   = WAsTpSin;
          MSTPINT   = *BLANKS;
          MSSQINT   = *ZEROS;
          MSTPSIN   = *ZEROS;
          MSNOME    = *BLANKS;
          MSNMMAT   = *BLANKS;
          DATENVIO  = %DATE(20000101:*ISO);
          HORENVIO  = %TIME(000000:*ISO);
          QTDENVIO  = *ZEROS;
          DATALTER  = %DATE();
          HORALTER  = %TIME();
          DATCRIA   = %DATE();
          HORCRIA   = %TIME();
          SEQLUSI   = *ZEROS;
          CODESTAD  = 'A';   // Activo
          WRITE RSIN;
       ENDSR;

       //================================================================
       // Subrotina.: GetInterv
       // Objectivo.: Obter o Nome do Interveninente.
       //================================================================
       BEGSR GetInterv;
          IgnoreInt(1).Nome  = 'PROPRIO';
          IgnoreInt(2).Nome  = 'PROPRIA';
          WIdx = 1;
          IF (WAsTpSin = 2);
             DOW (WIdx < 3);
                IF (%SCAN(%TRIM(IgnoreInt(WIdx).Nome):WNome) > 0);
                   WNome = INT_NOMSEG;
                   LEAVE;
                ELSE;
                   WNome = INT_NOMSIN;
                ENDIF;
                WIdx += 1;
             ENDDO;
          ELSE;
             WNome = INT_NOMSEG;
          ENDIF;
       ENDSR;

       //================================================================
       // Subrotina.: UpdMSInter
       // Objectivo.: Actualizar Intervinienets do MS Access
       //================================================================
       BEGSR UpdMSInter;
          //------------------------------------------------------------
          // Tratar ficheiro de Danos Corporais (FSINP102)
          //------------------------------------------------------------
          WMsOrigem = *BLANKS;
          SETLL *LOVAL FSINP102;
          READ RSINP102;
          DOW (NOT %EOF);
             //------------------------------------------------------
             // Validar se a relação já está criada
             //------------------------------------------------------
             MONITOR;
                WAaProc    = %DEC(%SUBST(C_NRPROC:1:2):2:0);
                WNuProc    = %DEC(%SUBST(C_NRPROC:4:4):4:0);
                WMsTpInt = C_SIN10201;
                WMsTpSin = 2;   // Danos Corporais
                %SUBST(WMsOrigem:1:1) = 'x';
                WMsSqInt = %DEC(C_NORDEM:2:0);
                WMsNome  = C_INTERVEN;
                WMsNmMat = *BLANKS;
                WSavNUMINT = C_NUMINT;
                WSavTipInt = C_TIPINT;
                EXSR UPDMSpart;
             ON-ERROR;
                WAaProc  = *ZEROS;
                WNuProc  = *ZEROS;
             ENDMON;
             READ RSINP102;
          ENDDO;

          //------------------------------------------------------------
          // Tratar ficheiro de Danos Materiais (FSINP103)
          //------------------------------------------------------------
          WMsOrigem = *BLANKS;
          SETLL *LOVAL FSINP103;
          READ RSINP103;
          DOW (NOT %EOF);
             //------------------------------------------------------
             // Validar se a relação já está criada
             //------------------------------------------------------
             MONITOR;
                WAaProc  = %DEC(%SUBST(M_NRPROC:1:2):2:0);
                WNuProc  = %DEC(%SUBST(M_NRPROC:4:4):4:0);
                WMsTpInt = M_SIN10301;
                WMsTpSin = 1;   // Danos Materiais
                %SUBST(WMsOrigem:2:1) = 'x';
                WMsSqInt = %DEC(M_NORDEM:2:0);
                WMsNome  = M_INTERVEN;
                WMsNmMat = M_MATRICULA;
                WSavNUMINT = M_NUMINT;
                WSavTipInt = M_TIPINT;
                EXSR UPDMSpart;
             ON-ERROR;
                WAaProc  = *ZEROS;
                WNuProc  = *ZEROS;
             ENDMON;
             READ RSINP103;
          ENDDO;

          //------------------------------------------------------------
          // Tratar ficheiro de Danos Materiais e Corporais (FSINP104)
          //------------------------------------------------------------
          WMsOrigem = *BLANKS;
          SETLL *LOVAL FSINP104;
          READ RSINP104;
          DOW (NOT %EOF);
             //------------------------------------------------------
             // Validar se a relação já está criada
             //------------------------------------------------------
             MONITOR;
                WAaProc  = %DEC(%SUBST(T_NRPROC:1:2):2:0);
                WNuProc  = %DEC(%SUBST(T_NRPROC:4:4):4:0);
                WMsTpInt = T_SIN10401;
                WMsTpSin = 3;   // Danos Materiais e Corporais
                %SUBST(WMsOrigem:3:1) = 'x';
                WMsSqInt = %DEC(T_NORDEM:2:0);
                WMsNome  = T_INTERVEN;
                WMsNmMat = T_MATRICULA;
                WSavNUMINT = T_NUMINT;
                WSavTipInt = T_TIPINT;
                EXSR UPDMSpart;
             ON-ERROR;
                WAaProc  = *ZEROS;
                WNuProc  = *ZEROS;
             ENDMON;
             READ RSINP104;
          ENDDO;
       ENDSR;

       //================================================================
       // Subrotina.: UpdMSpart
       // Objectivo.: Actualizar o Registo do MS Access
       //================================================================
       BEGSR UpdMSpart;
          //------------------------------------------------------
          // Se o numero for de uma participação então ignora
          //------------------------------------------------------
          IF (WNuProc < 6000);
             LEAVESR;
          ENDIF;

          MONITOR;
             CHAIN (WAaProc:WNuProc:WMsTpInt:WMsSqInt:WMsTpSin) RSINMS;
             IF (NOT %FOUND);
                IF (WAaProc <> WAaProcBk) OR (WNuProc <> WNuProcBk);
                   WAaProcBk = WAaProc;
                   WNuProcBk = WNuProc;
                   WLastReg = *ZEROS;
                ENDIF;

                //-------------------------------------------
                // Procurar Correspondência para o Registos
                //-------------------------------------------
                WIcEncontrou = *OFF;
                IF (WSavNumInt <> *ZEROS);
                   ASSQINT = WSavNUMINT;
                   ASTPINT = WSavTipInt;
                   CHAIN (WAaProc:WNuProc:WSavTipInt:WSavNumInt) RSINAS;
                   IF (%FOUND());
                      WIcEncontrou = *ON;
                      EXSR UpdMSAccess;
                   ENDIF;
                ENDIF;

                //-------------------------------------------
                // Procurar Correspondência para o Registos
                //-------------------------------------------
                IF (NOT WIcEncontrou);
                   SETLL (WAaProc:WNuProc) RSINAS;
                   READ RSINAS;
                   DOW (NOT %EOF) AND (WAaProc = AAPROC) AND (WNuProc = NUPROC);
                      WIcLigar = *OFF;
                      IF (ASTPSIN = WMsTpSin) AND ValName(ASNOME:WMsNome:0.5);
                         EXSR UpdMSAccess;
                         LEAVE;
                      ENDIF;
                      READ RSINAS;
                   ENDDO;
                ENDIF;
             ENDIF;
          ON-ERROR;
          ENDMON;
       ENDSR;

       //================================================================
       // Subrotina.: UpdMSAccess
       // Objectivo.: Actualziar registo do MS.Access
       //================================================================
       BEGSR UpdMSAccess;
          CHAIN (AAPROC:NUPROC:SEQUENCIA) RSIN;
          MSTPINT  = WMsTpInt;
          MSTPSIN  = WMsTpSin;
          MSSQINT  = WMsSqInt;
          MSNOME   = WMsNome;
          MSNMMAT  = WMsNMMat;
          WIcLigar = *ON;
          IF (%SUBST(WMsOrigem:1:1) = 'x');
             %SUBST(MSORIGE:1:1) = 'x';
          ELSEIF (%SUBST(WMsOrigem:2:1) = 'x');
             %SUBST(MSORIGE:2:1) = 'x';
          ELSEIF (%SUBST(WMsOrigem:3:1) = 'x');
             %SUBST(MSORIGE:3:1) = 'x';
          ENDIF;
          DATALTER  = %DATE();
          HORALTER  = %TIME();
          IF (WSavNumInt <> *ZEROS);
             ASSQINT = WSavNUMINT;
             ASTPINT = WSavTipInt;
          ENDIF;
          UPDATE RSIN %FIELDS(MSTPINT:MSTPSIN:MSSQINT:
                              MSNOME:MSNMMAT:MSORIGE:
                              DATALTER:HORALTER:
                              ASSQINT:ASTPINT);
       ENDSR;

       //================================================================
       // Subrotina.: Validate
       // Objectivo.: Corrigir registos sem correspondência ou mal class
       //================================================================
       BEGSR Validate;
          WAaProc = *ZEROS;
          WNuProc = *ZEROS;
          SETLL *LOVAL RSINMS;
          READ RSINMS;
          DOW (NOT %EOF());
             //----------------------------------
             // Associar com o Segurado ou outro
             //----------------------------------
             IF (ASNOME = *BLANKS);
                WAaProc    = AAPROC;
                WNuProc    = NUPROC;
                WSequencia = SEQUENCIA;
                WNumInt    = NUMINT;
                WAsTpInt   = ASTPINT;
                WAsTpSin   = ASTPSIN;
                WAsSqInt   = ASSQINT;
                WAsNome    = ASNOME;
                WAsNmMat   = ASNMMAT;
                WAsNuApo   = ASNUAPO;
                WMsTpInt   = MSTPINT;
                WMsTpSin   = MSTPSIN;
                WMsSqInt   = MSSQINT;
                WMsNome    = MSNOME;
                WMsNmMat   = MSNMMAT;
                WMsOrigem  = MSORIGE;
                WIcChanged = *OFF;

                //------------------------------------------------
                // Juntar porque tem nomes parecidos
                //------------------------------------------------
                SETLL (WAaProc:WNuProc) RSINAS;
                READ RSINAS;
                DOW (NOT %EOF());
                   IF (WAaProc <> AAPROC) OR (WNuProc <> NUPROC);
                      LEAVE;
                   ENDIF;
                   IF (MSNOME = *BLANKS);
                      IF (ValName(ASNOME:WMsNome:0.5));
                         WIcChanged = *ON;
                         EXSR MergeRecord;
                         LEAVE;
                      ENDIF;
                   ENDIF;
                   READ RSINAS;
                ENDDO;

                //------------------------------------------------
                // Juntar porque tem elementos comuns
                //------------------------------------------------
                IF (NOT WIcChanged);
                   SETLL (WAaProc:WNuProc) RSINAS;
                   READ RSINAS;
                   DOW (NOT %EOF());
                      IF (WAaProc <> AAPROC) OR (WNuProc <> NUPROC);
                         LEAVE;
                      ENDIF;
                      IF (MSNOME = *BLANKS);
                         IF (AsNmMat = WMsNmMat);
                            WIcChanged = *ON;
                            EXSR MergeRecord;
                            LEAVE;
                         ENDIF;
                      ENDIF;
                      READ RSINAS;
                   ENDDO;
                ENDIF;
             ENDIF;
             READ RSINMS;
          ENDDO;
       ENDSR;

       //================================================================
       // Subrotina.: MergeRecord
       // Objectivo.: Juntar os registos
       //================================================================
       BEGSR MergeRecord;
          IF (WIcChanged);
             CHAIN (WAaProc:WNuProc:SEQUENCIA) RSIN;
             MSTPINT = WMsTpInt;
             MSTPSIN = WMsTpSin;
             MSSQINT = WMsSqInt;
             MSNOME  = WMsNome;
             MSNMMAT = WMsNmMat;
             IF (%SUBST(WMsOrigem:1:1) = 'x');
                %SUBST(MSORIGE:1:1) = 'x';
             ELSEIF (%SUBST(WMsOrigem:2:1) = 'x');
                %SUBST(MSORIGE:2:1) = 'x';
             ELSEIF (%SUBST(WMsOrigem:3:1) = 'x');
                %SUBST(MSORIGE:3:1) = 'x';
             ENDIF;
             DATALTER  = %DATE();
             HORALTER  = %TIME();
             UPDATE RSIN %FIELDS(MSTPINT:MSTPSIN:
                                 MSSQINT:MSNOME:MSNMMAT:MSORIGE:
                                 DATALTER:HORALTER);
             WIcLigar = *ON;
             CHAIN (WAaProc:WNuProc:WSequencia) RSIN;
             DELETE RSIN;
          ENDIF;
       ENDSR;

       //================================================================
       // Subrotina.: GetLastRecord
       // Objectivo.: Obtyer o último número de sequencia disponível
       //================================================================
       BEGSR GetLastRecord;
          WLastReg = 1;
          SETLL (WAaProc:WNuProc) RSINAS;
          READ RSINAS;
          DOW (NOT %EOF) AND (WAaProc = AAPROC) AND (WNuProc = NUPROC);
             IF (SEQUENCIA >= WLastReg);
                WLastReg = SEQUENCIA + 1;
             ENDIF;
             READ RSINAS;
          ENDDO;
       ENDSR;
      /END-FREE
      /COPY #JMPRDSRC/QRPGLESRC,RSINP100
