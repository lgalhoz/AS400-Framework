     D*------------------------------------------------------
     D* Criar um ecr�.
     D*   Ex.: WScreenId = $AddScreen(<File>);
     D*------------------------------------------------------
     D $AddScreen      PR             3S 0
     D  FileName                     10A   Const
     D  FMTNAME                      10A   Const Options(*NOPASS)
     D
     D*------------------------------------------------------
     D* Definir ecr� corrente.
     D*   Ex.: WStatus = $SetScreen(WScreenID);
     D*------------------------------------------------------
     D $SetScreen      PR              N
     D   IDSCREEN                     3S 0 Const
     D   FMTNAME                     10A   Const Options(*NOPASS)
     D
     D*------------------------------------------------------
     D* Obter o ecr� corrente.
     D*   Ex.: WSceenID = $GetScreen();
     D*------------------------------------------------------
     D $GetScreen      PR             3S 0
     D
     D*------------------------------------------------------
     D* Remover um ecr�.
     D*   Ex.: WStatus = $RmvScreen(WScreenID);
     D*------------------------------------------------------
     D $RmvScreen      PR              N
     D   IDSCREEN                     3S 0 Const
     D
     D*------------------------------------------------------
     D* Gravar o endere�o do campo de controlo de atributo.
     D*   Ex.: WStatus = $AddScPFld(P-Field:<Field>);
     D*------------------------------------------------------
     D $AddScPFld      PR              N
     D   PFIELD                        *   Const
     D   FLDNAME                     10A   Const Options(*NOPASS)
     D
     D*------------------------------------------------------
     D* Gravar Endere�o das Coordenadas do ecr�
     D*   Ex.: WStatus = $AddScCoord(ROW:COL);
     D*------------------------------------------------------
     D $AddScCoord     PR              N
     D   SCRROW                        *   Const
     D   SCRCOL                        *   Const
     D
     D*------------------------------------------------------
     D* Gravar endere�o da Mensagem de Erro
     D*   Ex.: WStatus = $AddScMsg(MSG);
     D*------------------------------------------------------
     D $AddScMsg       PR              N
     D   MSGTEXT                       *   Const
     D
     D*------------------------------------------------------
     D* ForCompr a mensagem de Erro
     D*   Ex.: WStatus = $SetScError(<Field>:<MsgId>);
     D*------------------------------------------------------
     D  $SetScError    PR              N
     D     FLDNAME                   10A   Const
     D     MSGID                      7A   Const
     D
     D*------------------------------------------------------
     D* Proteger um campo de escrita
     D*   Ex.: WStatus = $SetScFldProt(<Field>);
     D*------------------------------------------------------
     D $SetScFldProt   PR              N
     D     FLDNAME                   10A   Const
     D
     D*------------------------------------------------------
     D* Proteger um campo de escrita
     D*   Ex.: WStatus = $SetScFldProt(<Field>);
     D*------------------------------------------------------
     D $SetScFldUnpr   PR              N
     D     FLDNAME                   10A   Const
     D
     D*------------------------------------------------------
     D* Obter o estado do ecr� (O=Sem erros; E = Com erros)
     D*   Ex.: WCdErro = $GetScStatus();
     D*------------------------------------------------------
     D  $GetScStatus   PR             1A
     D
     D*------------------------------------------------------
     D* Inicializar o ecr�
     D*   Ex.: WStatus   = $InzScStatus();
     D*------------------------------------------------------
     D  $InzScStatus   PR              N
     D
