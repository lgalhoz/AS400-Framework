     D**********************************************************************
     D*=======================================================
     D* Abrir um Ficheiro XML (Pode ser num servidor FTP)
     D*=======================================================
     D $OpenXMLData    PR             7A
     D   FileName                   140A   CONST
     D   Server                       5S 0 CONST OPTIONS(*NOPASS)
     D
     D*=========================================================
     D* Obter um valor do Ficheiro para um campo Alfanumérico
     D*=========================================================
     D $GetXMLAlfa     PR            40A
     D   TagNameIni                  40A   CONST
     D   TagNameFim                  40A   CONST OPTIONS(*NOPASS)
     D   Parametro                    2A   CONST OPTIONS(*NOPASS)
     D
     D*=========================================================
     D* Obter um valor do Ficheiro para um campo Numérico
     D*=========================================================
     D $GetXMLNumero   PR            15S 5
     D   TagNameIni                  40A   CONST
     D   TagNameFim                  40A   CONST OPTIONS(*NOPASS)
     D   Parametro                    2A   CONST OPTIONS(*NOPASS)
     D
     D*=========================================================
     D* Obter A quantidade de registos de uma Tag
     D*=========================================================
     D $GetXMLQtdReg   PR             4S 0
     D   TagName                     40A   CONST
     D
     D*=========================================================
     D* Posicionar-se numa TAG XML
     D*=========================================================
     D $GotoXMLTag     PR
     D   TagTree                    120A   CONST
     D   Registo                      4S 0 CONST OPTIONS(*NOPASS)
     D   Reset                         N   CONST OPTIONS(*NOPASS)
