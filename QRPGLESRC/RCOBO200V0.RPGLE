     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('JMDIR':'JMINT')
     H*==================================================================
     H* Programa..: RCOBO200
     H* Descrição.: Actualizar lista de recibos prestes a vencer e sem
     H*             Informação sobre boa cobrança.
     H*==================================================================
     F*==> Novo sistema
     FFCOBP010  UF A E           K DISK    PREFIX(COB_)
     FFCOBP01001IF   E           K DISK    PREFIX(COB_)
     F                                     RENAME(RCOBP010:RCOBP01001)
     FFCOBP020  UF A E           K DISK    PREFIX(COB_)
     FFCOBP021  UF A E           K DISK    PREFIX(COB_)
     F
     F*==> Sistema Corretor
     FRECIBO    IF   F  326     7AIDISK    KEYLOC(270) EXTFILE('SD.1.REC')
     FMEDIADOR  IF   F  128     3AIDISK    KEYLOC(4) EXTFILE('SD.1.CR')
     FSD1RM     IF   F  128     3AIDISK    KEYLOC(5) EXTFILE('SD.1.RM')
     FSD1AP1    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP1')
     FSD1AP2    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP2')
     F
     F*==> Sistema Lusitania Azul
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FGFSEGEXT  IF   E           K DISK    PREFIX(SEG_)
     FFMED      IF   E           K DISK    PREFIX(COR_)
     FGCOBGR    IF   E           K DISK    PREFIX(RLS_)
     FGBASEGRL  IF   E           K DISK    PREFIX(ALS_)
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
     D WVariaveis      S            100A
     D WCodMediador    S              3S 0
     D WNumSequ        S              3S 0
     D WNumRecibo      S              9S 0
     D WDescProduto    S             40A
     D WDescRisco      S             70A
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
     D TIP#Recibo      C                   'R'
     D TIP#Apolice     C                   'A'
     D TIP#Interface   C                   'I'
     D STS#Fechado     C                   'F'
     D STS#Aberto      C                   'A'
     D MOT#Atraso      C                   'AT'
     D CNT#Telefone    C                   'TL'
     D TPL#Atraso      C                   4
     D*==================================================================
      /FREE
       //-------------------------------------------------
       // Validar se está num dia útil
       //-------------------------------------------------
       IF (NOT $chkDUtil($DateToInt(%DATE())));
          RETURN;
       ENDIF;

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
          Periodo.Minimo = $DateToInt(%DATE()+%DAYS(1));
          Periodo.Maximo = Periodo.Minimo;
          WIcNotDiaUtil = $chkDUtil(Periodo.Maximo);
          DOW (NOT WIcNotDiaUtil);
             Periodo.Maximo = $DateToInt($IntToDate(Periodo.Maximo)+%DAYS(1));
             WIcNotDiaUtil = $chkDUtil(Periodo.Maximo);
          ENDDO;
          WSeqInforma = $GetNextKey('FCOBP010':'CDCONT');
       ENDSR;

       //===================================================
       // Subrotina.: SelRecibos
       // Objectivo.: Seleccionar os Recibos
       //===================================================
       BEGSR SelRecibos;
          READ RECIBO R#RECIBO;
          DOW (NOT %EOF(RECIBO));
             IF (REC_CODMOV = TIP#Premio) AND (REC_CODSIT = SIT#Emitido);
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
          CHAIN (TIP#Recibo:REC_NORD2) RCOBP01001;
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
             COB_TPENTID  = TIP#Recibo;          // Tipo de Entidade
             COB_NUORDEM  = REC_NORD2;           // Numero de Ordem
             COB_DTPXCNT  = $DateToInt(%DATE()); // Data do Próximo Contacto
             COB_HRPXCNT  = 080000;              // Hora do Próximos Contacto
             COB_ICESTAD  = STS#Aberto;          // Estado do Contacto
             COB_CDUSER   = S#USR;               // Utilizador do Contacto
             COB_NUOPERA  = 1;                   // Número da Última Operação
             COB_DTINICIO = $DateToInt(%DATE()); // Data de Inicio do Contacto
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
             WRITE RCOBP020;
          ON-ERROR;
             WCodErro = 'COB0210';
             LEAVESR;
          ENDMON;

          EXSR GetSegurado;  // Obter os dados do Segurado
          EXSR GetMediador;  // Obter os dados do Mediador
       ENDSR;

       //===================================================
       // Subrotina.: ExtractVars
       // Objectivo.: Adicionar destinatários da Mensagem
       //===================================================
       BEGSR ExtractVars;
          WVariaveis = '&(CMP)#&(RCBO)#&(REFM)#&(VTREC)#&(RMAPOL)#+
                        &(RISCO)#&(DESPROD)';
          WVariaveis = $strReplace('&(CMP)':%CHAR(REC_CCOMP):WVariaveis);
          IF (REC_CCOMP = 54);
             EXSR GetLusitAzul;
          ELSE;
             EXSR GetOutras;
          ENDIF;
       ENDSR;

       //===================================================
       // Subrotina.: GetLusitAzul
       // Objectivo.: Obter informação da Lusitania Azul
       //===================================================
       BEGSR GetLusitAzul;
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
             WVariaveis = $strReplace('&(RMAPOL)':
                                    %TRIM(%EDITW(RLS_GCRAMO:'0  '))+'-' +
                                    %TRIM(%EDITW(RLS_GCAPOL:'0       ')):
                                    WVariaveis);
          ENDIF;

          //-------------------------------------------
          // Obter o Objecto Seguro
          //-------------------------------------------
          WDescRisco = *BLANKS;
          WDescProduto = *BLANKS;
          CHAIN (RLS_GCRAMO:RLS_GCAPOL) GBASEGRL;
          IF (%FOUND(GBASEGRL));
             IF (ALS_RAMO <> 50) AND (ALS_RAMO <> 51);
                WDescRisco = %TRIM(ALS_LRPS);
                IF (ALS_RAMO = 05) AND (ALS_CONDOM = 'C');
                   WDescProduto = 'MULTIRISCOS CONDOMINIO';
                ELSEIF (ALS_RAMO = 05) AND (ALS_CONDOM = *BLANKS);
                   WDescProduto = 'MULTIRISCOS HABITAÇÃO';
                ELSE;
                   WDescProduto = $GetDescricao(104:
                                        %TRIM(%EDITW(ALS_RAMO:'0  ')));
                ENDIF;
             ELSE;
                WDescRisco = 'MARCA:'+%TRIM(ALS_MARC1)+' '+
                             'MATRICULA:'+%TRIM(ALS_MATR1);
                WDescProduto = 'AUTOMÓVEL';
             ENDIF;
             WVariaveis = $strReplace('&(RISCO)':WDescRisco:WVariaveis);
          ENDIF;

          //-------------------------------------------
          // Obter Descrição do Produto
          //-------------------------------------------
          WVariaveis = $strReplace('&(DESPROD)':
                                    %TRIM(WDescProduto):
                                    WVariaveis);
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
             IF (%TRIM(AP_MATRI) <> *BLANKS);
                WDescRisco = %TRIM(AP_DVAR)+' '+
                             'MATRICULA:'+ %TRIM(AP_MATRI);
             ELSE;
                WDescRisco = %TRIM(AP_DVAR)+' '+%TRIM(AP_DVAR2);
             ENDIF;
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
          CHAIN (REC_CSEGUR) GFSEG;
          IF (%FOUND(GFSEG));
             Destino.Numero  = WNumSequ;
             Destino.TipDest = 'TO';
             Destino.Codigo  = SEG_NSEG;
             Destino.Nome    = SEG_NOM;
             CHAIN (REC_CSEGUR) GFSEGEXT;
             IF (%FOUND(GFSEGEXT));
                Destino.Email = SEG_EMAIL01;
                IF (%TRIM(Destino.EMail) <> *BLANKS);
                   EXSR AddDestinatario;
                   WNumSequ += 1;
                ELSE;
                   WCodErro = 'COB0203'; // O Mail nao tem valores ou é inválido
                ENDIF;
             ELSE;
                WCodErro = 'COB0204'; // Não existe mail registado
             ENDIF;
          ELSE;
             WCodErro = 'COB0205'; // O Segurado não existe
          ENDIF;
          IF (WCoderro <> *BLANKS);
             EXSR RegErro;
          ENDIF;
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
          IF (Destino.Codigo >= 60000);
             CHAIN (Destino.Codigo) GFSEGEXT;
             IF (%FOUND(GFSEGEXT));
                Destino.Email = SEG_EMAIL01;
             ELSE;
                WCodErro = 'COB0208';
             ENDIF;
          ELSE;
             Destino.Codigo  = 60277;
             Destino.Nome    = 'Tesouraria - Jose Mata, Lda';
             Destino.Email   = 'seguros@josemata.pt';
          ENDIF;
          EXSR AddDestinatario;
          WNumSequ += 1;
       ENDSR;

       //===================================================
       // Subrotina.: RegErro
       // Objectivo.: Adicionar destinatários da Mensagem
       //===================================================
       BEGSR RegErro;
       ENDSR;

       //===================================================
       // Subrotina.: AddDetinatario;
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
