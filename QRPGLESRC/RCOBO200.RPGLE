     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('JMDIR':'JMINT')
     H*==================================================================
     H* Programa..: RCOBO200
     H* Descrição.: Actualizar lista de recibos prestes a vencer e sem
     H*             Informação sobre boa cobrança.
     H*
     H*             São seleccionados os registos de Apólices pertencenete a
     H*             Mediadores com Cobrança e Directos.
     H*
     H* Observação: Excluir as ofertas da selecção, através da consulta aos bases, e do
     H*             Canal de Cobrança <> 8.
     H*
     H*--> Antes de Compilar, fazer:
     H* CRTDUPOBJ OBJ("fcobp010") FROMLIB("josemata") OBJTYPE(*FILE) TOLIB(QTEMP) NEWOBJ(fcobp010)
     H* CRTDUPOBJ OBJ("fcobp010") FROMLIB("josemata") OBJTYPE(*FILE) TOLIB(QTEMP) NEWOBJ("fcobp010")
     H* CRTDUPOBJ OBJ("fcob0001") FROMLIB("josemata") OBJTYPE(*FILE) TOLIB(QTEMP) NEWOBJ(fcobp01001)
     H* CRTDUPOBJ OBJ("fcobp022") FROMLIB("josemata") OBJTYPE(*FILE) TOLIB(QTEMP) NEWOBJ(fcobp022)
     H* CRTDUPOBJ OBJ("fcobp021") FROMLIB("josemata") OBJTYPE(*FILE) TOLIB(QTEMP) NEWOBJ(fcobp021)
     H*
     H*==================================================================
     F*==> Novo sistema
     FFCOBP010  UF A E           K DISK    PREFIX(COB_)
     F                                     EXTFILE('"josemata"/"fcobp010"')
     F                                     RENAME(FCOBP010:RCOBP010)
     FFCOBP01001IF   E           K DISK    PREFIX(COB_)
     F                                     RENAME(FCOBP010:RCOBP01001)
     F                                     EXTFILE('"josemata"/"fcob0001"')
     FFCOBP022  UF A E           K DISK    PREFIX(COB_)
     F                                     RENAME(FCOBP020:RCOBP022)
     F                                     EXTFILE('"josemata"/"fcobp022"')
     FFCOBP021  UF A E           K DISK    PREFIX(COB_)
     F                                     RENAME(FCOBP021:RCOBP021)
     F                                     EXTFILE('"josemata"/"fcobp021"')
     F
     F*==> Sistema Corretor
     FRECIBO    IF   F  326     7AIDISK    KEYLOC(270) EXTFILE('SD.1.REC')
     FMEDIADOR  IF   F  128     3AIDISK    KEYLOC(4) EXTFILE('SD.1.CR')
     FPESSOAS   IF   F  512     6AIDISK    KEYLOC(5) EXTFILE('SD.1.PE')
     FSD1RM     IF   F  128     3AIDISK    KEYLOC(5) EXTFILE('SD.1.RM')
     FSD1AP1    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP1')
     FSD1AP2    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP2')
     F
     F*==> Sistema Lusitania Azul
     FGBASEGRL  IF   E           K DISK    PREFIX(BAS_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FGFSEGEXT  IF   E           K DISK    PREFIX(SEG_)
     FFMED      IF   E           K DISK    PREFIX(COR_)
     FGCOBGR    IF   E           K DISK    PREFIX(RLS_)
     FTBIDDPF   IF   E           K DISK    PREFIX(IDD_)
     F*==================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010
      /COPY QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SINTO010

     D*==> Formatos de Registo
     D R#MEDIADOR    E DS                  EXTNAME('SD.1.CR') PREFIX('MED_')
     D R#RECIBO      E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D R#PESSOAS     E DS                  EXTNAME('SD.1.PE') PREFIX('PE_')
     D
     D*==> Tabela de Ramos - Corretor
     D R#SD1RM         DS
     D   RM_Tipo               1      4
     D   RM_Codigo             5      7S 0
     D   RM_DescCompl          8     30A
     D   RM_IcAnula           31     31A
     D   RM_DescAbrev         32     40A
     D   RM_SeqSumm           41     43S 0
     D   RM_EncFixos          44     46P 3
     D   RM_ComAngar          47     49P 3
     D   RM_ImpSelo           50     52P 3
     D   RM_ComCobra          53     55P 3
     D   RM_OutDesp           56     58P 3
     D   RM_ComCorr           59     61P 3
     D   RM_Filler            62    111A
     D   RM_NumAltera        112    114S 0
     D   RM_UseAltera        115    122A
     D   RM_DatAltera        123    128S 0
     D
     D*==> Registo de Apólices - Corretor
     D R#AP1         E DS                  EXTNAME('SD.1.AP1') PREFIX('AP_')
     D R#APKEY         DS
     D   AP_KEY                1     16
     D   APOLI                 1     10A
     D   CCOMP                11     13S 0
     D   CRAMO                14     16S 0
     D
     D*==> Campos de Trabalho
     D Destino         DS                  QUALIFIED
     D   Numero                       3S 0
     D   TipDest                      1A
     D   Codigo                       6S 0
     D   Nome                        50A
     D   EMail                      100A
     D
     D Periodo         DS                  QUALIFIED
     D   Minimo                       8S 0
     D   Maximo                       8S 0
     D
     D WCodErro        S              7A
     D WDatLimite      S              8S 0
     D WIcNotDiaUtil   S               N
     D WSeqInforma     S              9S 0
     D WVariaveis      S                   LIKE(COB_VARIAVEIS)
     D WCodMediador    S              3S 0
     D WNumSequ        S              3S 0
     D WNumRecibo      S              9S 0
     D WDescProduto    S             40A
     D WDescRisco      S             70A
     D WTipEntidade    S              1A
     D pData           S              8A
     D WData           S               D
     D WContacto       S             30A
     D
     D*==> Constantes
     D KTE#Dias        C                   3
     D TIP#Estorno     C                   21
     D TIP#Premio      C                   15
     D TIP#Indemniza   C                   43
     D TIP#Compensa    C                   32
     D SIT#Emitido     C                   0
     D SIT#Anulado     C                   3
     D SIT#Cobrado     C                   2
     D SIT#SemEfeito   C                   1
     D
     D ENT#Corretor    C                   'R'
     D ENT#Apolice     C                   'A'
     D ENT#Interface   C                   'I'
     D ENT#Lusitania   C                   'L'
     D
     D STS#Fechado     C                   'F'
     D STS#Aberto      C                   'A'
     D MOT#Atraso      C                   'AT'
     D CNT#Telefone    C                   'TL'
     D TPL#Atraso      C                   4
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    pData
     C
     C
      /FREE
       //-------------------------------------------------
       // Validar se está num dia útil
       //-------------------------------------------------
       MONITOR;
          WData = $IntToDate(%DEC(pData:8:0));
          IF (NOT $chkDUtil(%DEC(pData:8:0)));
             RETURN;
          ENDIF;
       ON-ERROR;
          WData = %DATE();
          IF (NOT $chkDUtil($DateToInt(%DATE())));
             RETURN;
          ENDIF;
       ENDMON;

       //-------------------------------------------------
       // Determinar Intervalo minimo e máximo
       //-------------------------------------------------
       EXSR GetExtremos;
       IF (WCodErro <> *BLANKS);
          RETURN;
       ENDIF;

       //-------------------------------------------------
       // Seleccionar os registos dentro do intervalo
       //-------------------------------------------------
       EXSR SelRecibos;

       RETURN;
       *INLR = *ON;

       //===================================================
       // Subrotina.: GetExtremos
       // Objectivo.: Determinar extreos para a Selecção
       //===================================================
       BEGSR GetExtremos;
          Periodo.Minimo = $DateToInt(WData+%DAYS(1));
          Periodo.Maximo = Periodo.Minimo;
          WIcNotDiaUtil = $chkDUtil(Periodo.Maximo);
          DOW (NOT WIcNotDiaUtil);
             Periodo.Maximo = $DateToInt($IntToDate(Periodo.Maximo)+%DAYS(1));
             WIcNotDiaUtil = $chkDUtil(Periodo.Maximo);
          ENDDO;
          WSeqInforma = $GetNextKey('"josemata"/"fcobp010"':
                                    'CDCONT');
       ENDSR;

       //===================================================
       // Subrotina.: SelRecibos
       // Objectivo.: Seleccionar os Recibos
       //===================================================
       BEGSR SelRecibos;
          READ RECIBO R#RECIBO;
          DOW (NOT %EOF(RECIBO));
             IF (REC_CODMOV = TIP#Premio) AND (REC_CODSIT = SIT#Emitido) AND
                (REC_MCCOB = *ZEROS);
                MONITOR;
                   WDatLimite = $CvtData(%DEC(REC_DTLIM:6:0):'DMY':'YYMD');
                ON-ERROR;
                   WDatLimite = *ZEROS;
                ENDMON;
                WCodErro = *BLANKS;
                EXSR CrtRegisto;
             ENDIF;
             READ RECIBO R#RECIBO;
          ENDDO;
       ENDSR;

       //===================================================
       // Subrotina.: CrtRegisto;
       // Objectivo.:
       //===================================================
       BEGSR CrtRegisto;
          //---------------------------------------------------------
          // Validar se é um recibo a seleccionar pelas datas
          //---------------------------------------------------------
          IF (WDatLimite < Periodo.Minimo) OR (WDatlimite > Periodo.Maximo);
             LEAVESR;
          ENDIF;

          //---------------------------------------------------------
          // Excluir recibos de Débitos Directos
          //---------------------------------------------------------
          WNumRecibo = %DEC(REC_NREC2:9:0);
          CHAIN (WNumRecibo) TBIDDPF;
          IF (%FOUND(TBIDDPF));
             LEAVESR;
          ENDIF;

          //---------------------------------------------------------
          // Excluir Recibos já registados
          //---------------------------------------------------------
          IF (REC_CCOMP = 54);
             WTipEntidade = ENT#Lusitania;
          ELSE;
             WTipEntidade = ENT#Corretor;
          ENDIF;

          CHAIN (WTipEntidade:REC_NORD2) RCOBP01001;
          IF (%FOUND(FCOBP01001));
             LEAVESR;
          ENDIF;

          //---------------------------------------------------------
          // Criar Registo de controlo
          //---------------------------------------------------------
          MONITOR;
             WSeqInforma += 1;
             COB_CDCONT   = WSeqInforma;         // Código do Contacto
             COB_MTCONT   = MOT#Atraso;          // Motivo do Contacto
             COB_TPENTID  = WTipEntidade;        // Tipo de Entidade
             COB_NUORDEM  = REC_NORD2;           // Numero de Ordem
             COB_DTPXCNT  = $DateToInt(WData);   // Data do Próximo Contacto
             COB_HRPXCNT  = 080000;              // Hora do Próximos Contacto
             COB_ICESTAD  = STS#Aberto;          // Estado do Contacto
             COB_CDUSER   = S#USR;               // Utilizador do Contacto
             COB_NUOPERA  = 1;                   // Número da Última Operação
             COB_DTINICIO = $DateToInt(WData);   // Data de Inicio do Contacto
             COB_DTTERMO  = 99991231;            // Data de Termo do Contacto
             WRITE RCOBP010;
          ON-ERROR;
             WCodErro = 'COB0209';
             LEAVESR;
          ENDMON;

          //---------------------------------------------------------
          // Criar lista de Distribuição de mails
          //---------------------------------------------------------
          EXSR ExtractVars;
          IF (WCodErro <> *BLANKS);
             LEAVESR;
          ENDIF;

          MONITOR;
             COB_CDCONT   = WSeqInforma; // Código do Contacto
             COB_CDMAIL   = 1;           // Código do Endereço de eMail
             COB_CDTEMPL  = TPL#Atraso;  // Template da Mensagem
             COB_VARIAVEIS= WVariaveis;  // Variáveis da Mensagem
             COB_ICENVIO  = *ZEROS;      // Registo Enviado
             COB_DTENVIO  = *ZEROS;      // Data do Envio
             COB_DTRESPO  = *ZEROS;      // Data da Resposta
             WRITE RCOBP022;
          ON-ERROR;
             WCodErro = 'COB0210';
             LEAVESR;
          ENDMON;

          EXSR GetSegurado;  // Obter os dados do Segurado
          EXSR GetMediador;  // Obter os dados do Mediador
          WVariaveis = $strReplace('&(CONTACTO)':%TRIM(WContacto):WVariaveis);
       ENDSR;

       //===================================================
       // Subrotina.: ExtractVars
       // Objectivo.: Adicionar destinatários da Mensagem
       //===================================================
       BEGSR ExtractVars;
          WVariaveis = '&(CMP)#&(RCBO)#&(REFM)#&(VTREC)#&(RMAPOL)#+
                        &(RISCO)#&(DESPROD)#&(DATA)#&(CONTACTO)';

          WVariaveis = $strReplace('&(CMP)':%CHAR(REC_CCOMP):WVariaveis);
          WVariaveis = $strReplace('&(DATA)':%EDITW(
                        $CvtData(WDatLimite:'YYMD':'DMYY'):'  /  /    ')
                          :WVariaveis);

          IF (WTipEntidade = ENT#Lusitania);
             EXSR GetLusitania;
          ELSE;
             EXSR GetOutras;
          ENDIF;
          EXSR GetApolices;
       ENDSR;

       //===================================================
       // Subrotina.: GetLusitania
       // Objectivo.: Obter informação das outras Companhias
       //===================================================
       BEGSR GetLusitania;
          //-------------------------------------------
          // Referencia do Recibo
          //-------------------------------------------
          WVariaveis = $strReplace('&(RCBO)':%TRIM(REC_NREC2):WVariaveis);

          //-------------------------------------------
          // Multibanco e Valor do Recibo
          //-------------------------------------------
          WVariaveis = $strReplace('&(REFM)':$GetRefMB(WNumRecibo:REC_VTREC):
                                    WVariaveis);
          WVariaveis = $strReplace('&(VTREC)':
                                    %TRIM(%EDITW(REC_VTREC:'           ,  ')):
                                    WVariaveis);

          //-------------------------------------------
          // O Ramo e Apólice Lusitânia Azul
          //-------------------------------------------
          CHAIN (WNumRecibo) GCOBGR;
          IF (%FOUND(GCOBGR));
             IF (RLS_GCMED2 = 8);
                WCodErro = 'COB0233';
             ELSE;
                WVariaveis = $strReplace('&(RMAPOL)':
                                    %TRIM(%EDITW(RLS_GCRAMO:'0  '))+'-' +
                                    %TRIM(%EDITW(RLS_GCAPOL:'0       ')):
                                    WVariaveis);
             ENDIF;
          ENDIF;
       ENDSR;


       //===================================================
       // Subrotina.: GetOutras
       // Objectivo.: Obter informação das outras Companhias
       //===================================================
       BEGSR GetOutras;
          //-------------------------------------------
          // Referencia do Recibo
          //-------------------------------------------
          WVariaveis = $strReplace('&(RCBO)':%CHAR(REC_NORD):WVariaveis);

          //-------------------------------------------
          // Multibanco e Valor do Recibo
          //-------------------------------------------
          WVariaveis = $strReplace('&(REFM)':'NULO':WVariaveis);
          WVariaveis = $strReplace('&(VTREC)':
                                    %TRIM(%EDITW(REC_VTREC:'           ,  ')):
                                    WVariaveis);

          //-------------------------------------------
          // A Apólice
          //-------------------------------------------
          WVariaveis = $strReplace('&(RMAPOL)':%TRIM(REC_APOLIC):WVariaveis);
       ENDSR;

       //===================================================
       // Subrotina.: GetApolices
       // Objectivo.: Obter informação das outras Companhias
       //===================================================
       BEGSR GetApolices;
          //-------------------------------------------
          // Obter o Objecto Seguro
          //-------------------------------------------
          WDescRisco = *BLANKS;
          APOLI = REC_APOLIC;
          CCOMP = REC_CCOMP;
          CRAMO = REC_CRAMO;
          CHAIN R#APKEY SD1AP1 R#AP1;
          IF (NOT %FOUND(SD1AP1));
             CHAIN R#APKEY SD1AP2 R#AP1;
             IF (NOT %FOUND(SD1AP1));
                WCodErro = 'COB0211';
             ENDIF;
          ENDIF;

          IF (WCodErro = *BLANKS);
             WDescRisco = %TRIM(AP_DVAR);
          ENDIF;
          WVariaveis = $strReplace('&(RISCO)':%TRIM(WDescRisco):WVariaveis);

          //-------------------------------------------
          // Obter Descrição do Produto
          //-------------------------------------------
          WDescProduto = *BLANKS;
          CHAIN REC_CRAMO SD1RM R#SD1RM;
          IF (%FOUND(SD1RM));
             WDescProduto = RM_DescCompl;
          ENDIF;
          WVariaveis = $strReplace('&(DESPROD)':
                                    %TRIM(WDescProduto):
                                    WVariaveis);
       ENDSR;

       //===================================================
       // Subrotina.: GetSegurado
       // Objectivo.: Adicionar destinatários da Mensagem
       //===================================================
       BEGSR GetSegurado;
          WNumSequ = 1;
          Destino.Email = *BLANKS;
          WContacto = *BLANKS;
          CHAIN (REC_CSEGUR) GFSEG;
          IF (%FOUND(GFSEG));
             Destino.Numero  = WNumSequ;
             Destino.TipDest = 'TO';
             Destino.Codigo  = SEG_NSEG;
             Destino.Nome    = SEG_NOM;
             IF (SEG_TLM > *ZEROS);
                WContacto = 'TLM.:' + %CHAR(SEG_TLM);
             ELSEIF (SEG_TELEF <> *BLANKS);
                WContacto = 'TLF.:' + %TRIM(SEG_TELEF);
             ELSEIF (SEG_NFAX <> *BLANKS);
                WContacto = 'FAX.:' + %TRIM(SEG_NFAX);
             ENDIF;
             CHAIN (REC_CSEGUR) GFSEGEXT;
             IF (%FOUND(GFSEGEXT));
                Destino.Email = SEG_EMAIL01;
             ENDIF;
          ELSE;
             WCodErro = 'COB0205'; // O Segurado não existe
          ENDIF;

          IF (Destino.Email = *BLANKS) OR (WContacto = *BLANKS);
             CHAIN REC_CSEGUR PESSOAS R#PESSOAS;
             IF (%FOUND(PESSOAS));
                IF (Destino.Email = *BLANKS);
                   Destino.Email = PE_EMAIL;
                   IF (%TRIM(Destino.EMail) = *BLANKS);
                      WCodErro = 'COB0203'; // O Mail nao tem valores ou é inválido
                   ENDIF;
                ENDIF;
                IF (WContacto = *BLANKS);
                   IF (PE_TLM > *ZEROS);
                      WContacto = 'TLM.:' + %CHAR(PE_TLM);
                   ELSEIF (PE_TELEF <> *BLANKS);
                      WContacto = 'TLF.:' + %TRIM(PE_TELEF);
                   ELSEIF (PE_NFAX <> *BLANKS);
                      WContacto = 'FAX.:' + %TRIM(PE_NFAX);
                   ELSEIF (PE_NFAX2 <> *BLANKS);
                      WContacto = 'FAX.:' + %TRIM(PE_NFAX2);
                   ELSE;
                      WContacto = '<Sem contatos directos>';
                   ENDIF;
                ENDIF;
             ELSE;
                IF (Destino.Email = *BLANKS);
                   WCodErro = 'COB0204'; // Não existe mail registado
                ENDIF;
             ENDIF;
          ENDIF;

          EXSR AddDestinatario;
          WNumSequ += 1;
       ENDSR;

       //===================================================
       // Subrotina.: GetMediador
       // Objectivo.: Adicionar destinatários da Mensagem
       //===================================================
       BEGSR GetMediador;
          Destino.Numero  = WNumSequ;
          Destino.TipDest = 'TO';
          Destino.Email   = *BLANKS;
          Destino.Codigo  = *ZEROS;
          Destino.Nome    = *BLANKS;

          IF (REC_MCCOB > *ZEROS);
             Destino.Codigo  = REC_MCCOB;
          ELSEIF (REC_MSCOB > *ZEROS);
             Destino.Codigo  = REC_MSCOB;
          ENDIF;

          //-----------------------------------------------
          // Obter informações sobre os Corretores
          //-----------------------------------------------
          IF (Destino.Codigo >= 900);
             CHAIN (Destino.Codigo) FMED;
             IF (%FOUND(FMED));
                Destino.Codigo += 60000;
                Destino.Nome    = COR_NOMMED;
             ELSE;
                WCodErro = 'COB0206';
             ENDIF;
          ENDIF;

          //-----------------------------------------------
          // Obter informações sobre os Mediadores
          //-----------------------------------------------
          IF (Destino.Codigo < 900) AND (Destino.Codigo > *ZEROS);
             WCodMediador = Destino.Codigo;
             CHAIN WCodMediador MEDIADOR R#MEDIADOR;
             IF (%FOUND(MEDIADOR));
                Destino.Codigo += 60000;
                Destino.Nome    = MED_NOMCR;
             ELSE;
                WCodErro = 'COB0207';
             ENDIF;
          ENDIF;

          //-----------------------------------------------
          // Obter o endereço de Mail do Mediador
          //-----------------------------------------------
          Destino.Email = *BLANKS;
          IF (Destino.Codigo >= 60000);
             CHAIN (Destino.Codigo) GFSEGEXT;
             IF (%FOUND(GFSEGEXT));
                Destino.Email = SEG_EMAIL01;
             ELSE;
                WCodErro = 'COB0208'; // O Mail nao tem valores ou é inválido
             ENDIF;
          ELSE;
          // Destino.Codigo  = 60277;
          // Destino.Nome    = 'Tesouraria - Jose Mata, Lda';
          // Destino.Email   = 'seguros@josemata.pt';
          ENDIF;

          IF (Destino.Email = *BLANKS);
             CHAIN REC_CSEGUR PESSOAS R#PESSOAS;
             IF (%FOUND(PESSOAS));
                Destino.Email = PE_EMAIL;
                IF (%TRIM(Destino.EMail) = *BLANKS);
                   WCodErro = 'COB0203'; // O Mail nao tem valores ou é inválido
                ENDIF;
             ELSE;
                WCodErro = 'COB0208'; // O Mail nao tem valores ou é inválido
             ENDIF;
          ENDIF;

          EXSR AddDestinatario;
          WNumSequ += 1;
       ENDSR;

       //===================================================
       // Subrotina.: AddDestinatario;
       // Objectivo.: Adicionar destinatários da Mensagem
       //===================================================
       BEGSR AddDestinatario;
          //---------------------------------------------------------
          // Criar lista de Distribuição de mails
          //---------------------------------------------------------
          MONITOR;
             COB_CDCONT   = WSeqInforma;      // Código do Contacto
             COB_CDMAIL   = 1;                // Código do Endereço de eMail
             COB_NUSEQU   = Destino.Numero;   // Template da Mensagem
             COB_TIPDEST  = Destino.TipDest;  //
             COB_NUMSEG   = Destino.Codigo;   // Código do Segurado
             COB_NOME     = Destino.Nome;     // Nome do Segurado
             COB_EMAIL    = Destino.EMail;    // Endereço de Mail do Segurado
             COB_DTRESPO  = *ZEROS;           // Data da Resposta
             WRITE RCOBP021;
          ON-ERROR;
          ENDMON;
       ENDSR;
      /END-FREE
