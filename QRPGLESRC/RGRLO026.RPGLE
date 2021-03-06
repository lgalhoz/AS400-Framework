     H NOMAIN
     H*=======================================================
     H* Dat.Cria...: 2012-11-12
     H* Autor......: Luis Galhoz
     H* Objectivo..: Gerir Processos Batch em modo Autom�tico
     H*=======================================================
     FFGRLP020  UF A E           K DISK    USROPN
     FFGRLP021  UF A E           K DISK    USROPN
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010
     D
     D  REC#           S              4  0
     D
     P*=======================================================
     P* Processo..: $GetItemCod
     P* Descri��o.: Obter o �ltimo n�mero dispon�vel para o
     P*             Item.
     C*=======================================================
     P $GetItemCod     B                   EXPORT
     D                 PI             7S 0
     D    PTipItem                    3A   CONST
     D WRetCode        S              7S 0
     D*===========================================================
      /FREE
        RETURN *ZEROS;
      /END-FREE
     P $GetItemCod     E

     P*=======================================================
     P* Processo..: $UpdItemCod
     P* Descri��o.: Obter o �ltimo n�mero dispon�vel para o
     P*             Item.
     C*=======================================================
     P $UpdItemCod     B                   EXPORT
     D                 PI             7S 0
     D    PTipItem                    3A   CONST
     D    PCodItem                    7S 0 CONST
     D WRetCode        S              7S 0
     D*===========================================================
      /FREE
        RETURN *ZEROS;
      /END-FREE
     P $UpdItemCod     E

     P*=======================================================
     P* Processo..: $GetCodCarrega
     P* Descri��o.: Obter o �ltimo n�mero dispon�vel para o
     P*             Item.
     C*=======================================================
     P $GetCodCarrega  B                   EXPORT
     D                 PI             7S 0
     D*===========================================================
      /FREE
        RETURN *ZEROS;
      /END-FREE
     P $GetCodCarrega  E

     P*=======================================================
     P* Processo..: $AddCodCarrega
     P* Descri��o.: Adicionar um novo Carregamento
     C*=======================================================
     P $AddCodCarrega  B                   EXPORT
     D                 PI             7S 0
     D*===========================================================
      /FREE
        RETURN *ZEROS;
      /END-FREE
     P $AddCodCarrega  E
