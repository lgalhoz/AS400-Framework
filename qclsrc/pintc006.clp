/*******************************************************************/
/* Programa..: PINTC010                                            */
/* Objectivo.: Actualizar o referencial de Integridade da Base de  */
/*             dados.                                              */
/*******************************************************************/
/* Observação: Executar sempre que alterar alguma tabela deste     */
/*             grupo.                                              */
/*******************************************************************/
          PGM

          DCL  VAR(&MODULO) TYPE(*CHAR) LEN(3) VALUE('PRD')
          DCL  VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
          CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MODULO *CAT 'DBS')

             /****************************************************************/
             /* Adicionar Referencial de Integridade                         */
             /****************************************************************/
             RMVPFCST   FILE(&DBSLIB/FINTP003) CST(FINTP003_RL)
             RMVPFCST   FILE(&DBSLIB/FINTP004) CST(FINTP004_RL)
             RMVPFCST   FILE(&DBSLIB/FINTP002) CST(FINTP002_PK)
             RMVPFCST   FILE(&DBSLIB/FINTP002) CST(FINTP002_RL)
             RMVPFCST   FILE(&DBSLIB/FINTP001) CST(FINTP001_PK)

             ADDPFCST   FILE(&DBSLIB/FINTP001) TYPE(*PRIKEY) +
                          KEY(CDREMESSA) CST(FINTP001_PK)
             ADDPFCST   FILE(&DBSLIB/FINTP002) TYPE(*PRIKEY) +
                          KEY(CDREMESSA CDBATCH) CST(FINTP002_PK)
             ADDPFCST   FILE(&DBSLIB/FINTP002) TYPE(*REFCST) +
                          KEY(CDREMESSA) CST(FINTP002_RL) +
                          PRNFILE(&DBSLIB/FINTP001) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FINTP003) TYPE(*REFCST) +
                          KEY(CDREMESSA CDBATCH) CST(FINTP003_RL) +
                          PRNFILE(&DBSLIB/FINTP002) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FINTP004) TYPE(*REFCST) +
                          KEY(CDREMESSA) CST(FINTP004_RL) +
                          PRNFILE(&DBSLIB/FINTP001) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)
             ENDPGM
