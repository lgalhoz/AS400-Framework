     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PENTO002
     H* Descrição.: Actualizar informação
     H*==================================================================
     FGFSEG     IF   E           K DISK    PREFIX('S1_')
     FGFSEGEXT  IF   E           K DISK    PREFIX('S2_')
     FSD1PE     IF   E           K DISK    PREFIX('S3_')
     F                                     EXTFILE('SD.1.PE')
     FFENTP001  UF A E           K DISK    PREFIX('ENT_')
     FFENTP00101IF   E           K DISK    PREFIX('ENT_')
     F                                     RENAME(RENTP001:RENTP00101)
     FFENTP002  UF A E           K DISK    PREFIX('ENT_')
     FFENTP003  UF A E           K DISK    PREFIX('ENT_')
     FFENTP004  UF A E           K DISK    PREFIX('ENT_')
     FFENTP005  UF A E           K DISK    PREFIX('ENT_')
     FFENTP006  UF A E           K DISK    PREFIX('ENT_')
     F*================================================================
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SGRLO010

     D*==> Estrutura da Informação de Segurados
     D MntEntidade     PR              N
     D   PNome                             CONST LIKE(ENT_NOME)
     D   PApelido                          CONST LIKE(ENT_APELIDO)
     D   PDtNasc                           CONST LIKE(ENT_DTNASC)
     D   PNuNif                            CONST LIKE(ENT_NUNIF)
     D   PNuBI                             CONST LIKE(ENT_NUDOCUM)
     D   PTpEntdade                        CONST LIKE(ENT_TPENTID)
     D   PIcEspec                          CONST LIKE(ENT_ICESPEC)
     D   PNuTelef                          CONST LIKE(ENT_NUTELEF)
     D   PNuFax                            CONST LIKE(ENT_NUFAX)
     D   PeMail                            CONST LIKE(ENT_EMAIL)
     D   PDatAlt                           CONST LIKE(ENT_ENT00101)
     D   PUserID                           CONST LIKE(ENT_ENT00103)
     D
     D*==> Estrutura das Moradas do Segurado
     D MntMoradas      PR              N
     D   PMorada                           CONST LIKE(ENT_MORADA)
     D   PLocalidade                       CONST LIKE(ENT_LOCALID)
     D   PCodPostal                        CONST LIKE(ENT_CDPOSTAL)
     D
     D*==> Estrutura da Informação Adicional
     D MntAdicional    PR              N
     D   PCorsel                           CONST LIKE(ENT_CORSEL)
     D   PPiloto                           CONST LIKE(ENT_PILOTO)
     D   PIcDVen                           CONST LIKE(ENT_ICDVEN)
     D   PCdEmpreg                         CONST LIKE(ENT_CDEMPREG)
     D   PGest                             CONST LIKE(ENT_GEST)
     D   PTecn                             CONST LIKE(ENT_TECN)
     D   PConten                           CONST LIKE(ENT_CONTEN)
     D   PStesp                            CONST LIKE(ENT_STESP)
     D
     D*==> Actualizar a relação
     D MntRelacao      PR              N
     D   PTabela                           CONST LIKE(ENT_TABELA)
     D   PChave                            CONST LIKE(ENT_CHAVE)
     D   PDtAlte                           CONST LIKE(ENT_DtAlte)
     D   PHrAlte                           CONST LIKE(ENT_HRALTE)
     D   PUsAlte                           CONST LIKE(ENT_USALTE)
     D*================================================================
     D WNumEntidade    S              9S 0 INZ(*ZEROS)
     D WLstEntidade    S              9S 0 INZ(*ZEROS)
     D WCdRelac        S              9S 0 INZ(*ZEROS)
     D WS2ON           S               N   INZ(*OFF)
     D WS3ON           S               N   INZ(*OFF)
     D WModo           S              1S 0 INZ(*ZEROS)
     DWEMail           S                   LIKE(ENT_EMAIL)
     D
     D IO#Criar        C                   1
     D IO#Alterar      C                   2
     C*================================================================
      /FREE
        WLstEntidade = $GetNextKey('FENTP001':'CODIGO');

        //-----------------------------------------------------
        // Actualizar informação em função da CmpSeguro Azul
        //-----------------------------------------------------
        SETLL *LOVAL GFSEG;
        READ GFSEG;
        DOW NOT %EOF();
           //-------------------------------------------
           // Obter informação no ficheiro de extras
           //-------------------------------------------
           CHAIN (S1_NSEG) GFSEGEXT;
           IF (%FOUND());
   65         WEmail = S2_EMAIL01;
           ELSE;
   65         WEmail = S1_EMAIL;
           ENDIF;

           //-------------------------------------------
           // Obter informação no ficheiro do Corretor
           //-------------------------------------------
           CHAIN (S1_NSEG) SD1PE;
           IF (%FOUND());
              WS3ON = *ON;
           ELSE;
              WS3ON = *OFF;
           ENDIF;

           //------------------------------------
           // Actualizar o Registo
           //------------------------------------
           MntEntidade(S1_NOM:S1_Apelid:S1_DTNASC:S1_NIF:S1_BI:S1_TIPO:'N':
                       S1_TELEF:S1_NFAX:WEmail:$IntToDate(S1_DATALT):
                       S1_USERID);
           MntMoradas(S1_MORAD:S1_DESCP:S1_CPOST);
           MntAdicional(S1_CORSEL:S1_PILOTO:S2_ICDVEN:S2_CODEMP:
                        S1_GEST:S1_TECN:S1_CONTEN:S1_STESP);
           READ GFSEG;
        ENDDO;

        RETURN;

        //======================================================
        // Subrotina.: UpdRecord
        // Objectivo.: Actualizar Tabela de Entidades
        //======================================================
        BEGSR UpdRecord;
        ENDSR;


      /END-FREE
     P*===================================================================
     P* Processo..: MntEntidades
     P* Descrição.: Actualizar a informação das Entidade e dados
     P*             Complementares de Pessoas e Empresas
     P*===================================================================
     P MntEntidade     B                   EXPORT
     D MntEntidade     PI              N
     D   PNome                             CONST LIKE(ENT_NOME)
     D   PApelido                          CONST LIKE(ENT_APELIDO)
     D   PDtNasc                           CONST LIKE(ENT_DTNASC)
     D   PNuNIF                            CONST LIKE(ENT_NUNIF)
     D   PNuBI                             CONST LIKE(ENT_NUDOCUM)
     D   PTpEntdade                        CONST LIKE(ENT_TPENTID)
     D   PIcEspec                          CONST LIKE(ENT_ICESPEC)
     D   PNuTelef                          CONST LIKE(ENT_NUTELEF)
     D   PNuFax                            CONST LIKE(ENT_NUFAX)
     D   PeMail                            CONST LIKE(ENT_EMAIL)
     D   PDatAlt                           CONST LIKE(ENT_ENT00101)
     D   PUserID                           CONST LIKE(ENT_ENT00103)
     D
     D WIsEmpresa      S               N
     C*===================================================================
      /FREE
        //-------------------------------------
        // Validar Existência do Registo
        //-------------------------------------
        IF (PNuNIF > *ZEROS) AND (PNuNIF <> 999999990);
           CHAIN (PNuNIF) RENTP00101;
           IF (NOT %FOUND());
              WModo = IO#Criar;
              WLstEntidade += 1;
              WNumEntidade = WLstEntidade;
              WCdRelac = $GetNextKey('FENTP006':'CDRELAC');
           ELSE;
              WNumEntidade = ENT_CODIGO;
              WCdRelac     = ENT_CDRELAC;
              WModo = IO#Alterar;
           ENDIF;
        ELSE;
           WModo = IO#Criar;
           WLstEntidade += 1;
           WNumEntidade = WLstEntidade;
           WCdRelac = $GetNextKey('FENTP006':'CDRELAC');
        ENDIF;

        EXSR ValEmpresa;    // Validar se a Entidade é uma Empresa
        EXSR UpdEntidade;   // FENTP001
        IF (WIsEmpresa);
           EXSR UpdEmpresas;   // FENTP002
        ELSE;
           EXSR UpdPessoas;    // FENTP003
        ENDIF;
        Return *ON; // Operação com sucesso

        //======================================================
        // Subrotina.: ValEmpresa
        // Objectivo.: Validar se a Entidade é uma Empresa
        //======================================================
        BEGSR ValEmpresa;
           WIsEmpresa = *OFF;
           IF (PTpEntdade = 'I');
              WIsEmpresa = *OFF;
           ELSEIF (PTpEntdade = 'S');
              WIsEmpresa = *ON;
           ELSEIF (PNuNIF > *ZEROS) AND (PNuNIF < 500000000);
              WIsEmpresa = *OFF;
           ELSEIF (PNuNIF > *ZEROS) AND (PNuNIF >= 500000000);
              WIsEmpresa = *ON;
           ELSEIF (PNuBI > *ZEROS);
              WIsEmpresa = *OFF;
           ENDIF;
        ENDSR;

        //======================================================
        // Subrotina.: UpdEntidade
        // Objectivo.: Adicionar um registo de Entidade
        //======================================================
        BEGSR UpdEntidade;
           IF (WModo = IO#Criar);
              ENT_CODIGO   = WNumEntidade;
              ENT_NOME     = PNome;
              ENT_APELIDO  = PApelido;
              ENT_DTNASC   = PDtNasc;
              ENT_NUNIF    = PNuNif;
              ENT_TPENTID  = PTpEntdade;
              ENT_ICESPEC  = PIcEspec;
              ENT_NUTELEF  = PNuTelef;
              ENT_NUFAX    = PNuFax;
              ENT_EMAIL    = PeMail;
              ENT_ENT00101 = PDatAlt;
              ENT_ENT00102 = PDatAlt;
              ENT_ENT00103 = PUserID;
              ENT_CDRELAC  = WCdRelac;
              WRITE RENTP001;
           ENDIF;
        ENDSR;

        //======================================================
        // Subrotina.: UpdEmpresas
        // Objectivo.: Adicionar um registo de Empresa
        //======================================================
        BEGSR UpdEmpresas;
           IF (WModo = IO#Criar);
              ENT_CODIGO    = WNumEntidade;
              ENT_CDCAE     = *BLANKS;
              ENT_NUEMPREG  = *ZEROS;
              ENT_VLNEGOC   = *ZEROS;
              ENT_CDGRUPO   = *ZEROS;
              WRITE RENTP002;
           ENDIF;
        ENDSR;

        //======================================================
        // Subrotina.: UpdPessoas
        // Objectivo.: Adicionar um registo de Empresa
        //======================================================
        BEGSR UpdPessoas;
           IF (WModo = IO#Criar);
              ENT_CODIGO    = WNumEntidade;
              ENT_TPDOCUM   = 'AA1';
              ENT_NUDOCUM   = PNuBI;
              ENT_CDPROFIS  = *ZEROS;
              ENT_CDTITULO  = 'SR';
              WRITE  RENTP003;
           ENDIF;
        ENDSR;


      /END-FREE
     P MntEntidade     E

     P*===================================================================
     P* Processo..: MntMoradas
     P* Descrição.: Actualizar a informação das Moradas do Segurado
     P*
     P*===================================================================
     P MntMoradas      B                   EXPORT
     D MntMoradas      PI              N
     D   PMorada                           CONST LIKE(ENT_MORADA)
     D   PLocalidade                       CONST LIKE(ENT_LOCALID)
     D   PCodPostal                        CONST LIKE(ENT_CDPOSTAL)
     D*=================================================================
      /FREE
        ENT_NUSEQU    = $GetNextKey('FENTP004':'NUSEQU':'CODIGO = '+
                                     %CHAR(WNumEntidade));
        ENT_CODIGO    = WNumEntidade;
        ENT_TPMORADA  = 'RES';
        ENT_MORADA    = PMorada;
        ENT_LOCALID   = PLocalidade;
        ENT_CDPOSTAL  = PCodPostal;
        ENT_CDPAIS    = 351;
        ENT_CDRELAC   = WCdRelac;
        MntRelacao('QS36F/GFSEG':S1_NSEG:S1_DATALT:S1_TIMALT:S1_USERID);
        MntRelacao('QS36F/SD.1.PE':S3_NSEG:$CvtData(S3_DUA:'DMY':'YYMD'):
                   *ZEROS:S3_UUA);
        Return *ON; // Operação com sucesso
      /END-FREE
     P MntMoradas      E

     P*===================================================================
     P* Processo..: MntAdicional
     P* Descrição.: Actualizar a informação Adicional
     P*
     P*===================================================================
     P MntAdicional    B                   EXPORT
     D MntAdicional    PI              N
     D   PCorsel                           CONST LIKE(ENT_CORSEL)
     D   PPiloto                           CONST LIKE(ENT_PILOTO)
     D   PIcDVen                           CONST LIKE(ENT_ICDVEN)
     D   PCodEmp                           CONST LIKE(ENT_CDEMPREG)
     D   PGest                             CONST LIKE(ENT_GEST)
     D   PTecn                             CONST LIKE(ENT_TECN)
     D   PConten                           CONST LIKE(ENT_CONTEN)
     D   PStesp                            CONST LIKE(ENT_STESP)
     D*===================================================================
      /FREE
        IF (WModo = IO#Criar);
           ENT_CODIGO   = WNumEntidade;
           ENT_CORSEL   = PCorsel;
           ENT_PILOTO   = PPiloto;
           IF (WS2ON);
              ENT_ICDVEN   = PIcDven;
              ENT_CDEMPREG = PCodEmp;
           ELSE;
              ENT_ICDVEN   = 'N';
              ENT_CDEMPREG = *BLANKS;
           ENDIF;
           ENT_GEST     = PGest;
           ENT_TECN     = PTecn;
           ENT_CONTEN   = PConten;
           ENT_STESP    = PStesp;
           ENT_CDRELAC  = WCdRelac;
           WRITE RENTP005;
        ENDIF;
        Return *ON; // Operação com sucesso
      /END-FREE
     P MntAdicional    E

     P*===================================================================
     P* Processo..: MntRelacao
     P* Descrição.: Actualizar a Relação ou origem do registo
     P*
     P*===================================================================
     P MntRelacao      B                   EXPORT
     D MntRelacao      PI              N
     D   PTabela                           CONST LIKE(ENT_TABELA)
     D   PChave                            CONST LIKE(ENT_CHAVE)
     D   PDtAlte                           CONST LIKE(ENT_DtAlte)
     D   PHrAlte                           CONST LIKE(ENT_HRALTE)
     D   PUsAlte                           CONST LIKE(ENT_USALTE)
     D*===================================================================
      /FREE
        ENT_CDRELAC  = WCdRelac;
        ENT_NUSEQU   = $GetNextKey('FENTP006':'NUSEQU':'CDRELAC = '+
                                  %CHAR(WCdRelac));
        ENT_TABELA   = PTabela;
        ENT_CHAVE    = PChave;
        ENT_DTALTE   = PDtAlte;
        ENT_HRALTE   = PHrAlte;
        ENT_USALTE   = PUsAlte;
        WRITE RENTP006;
        Return *ON; // Operação com sucesso
      /END-FREE
     P MntRelacao      E
