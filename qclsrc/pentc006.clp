/*******************************************************************/
/* Programa..: PENTC010                                            */
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
             RMVPFCST   FILE(&DBSLIB/FENTP006) CST(FENTP006_RL)
             RMVPFCST   FILE(&DBSLIB/FENTP005) CST(FENTP005_RL)
             RMVPFCST   FILE(&DBSLIB/FENTP004) CST(FENTP004_RL)
             RMVPFCST   FILE(&DBSLIB/FENTP003) CST(FENTP003_RL)
             RMVPFCST   FILE(&DBSLIB/FENTP002) CST(FENTP002_RL)
             RMVPFCST   FILE(&DBSLIB/FENTP001) CST(FENTP001_PK)

             ADDPFCST   FILE(&DBSLIB/FENTP001) TYPE(*PRIKEY) +
                          KEY(CODIGO) CST(FENTP001_PK)

             ADDPFCST   FILE(&DBSLIB/FENTP002) TYPE(*REFCST) +
                          KEY(CODIGO) CST(FENTP002_RL) +
                          PRNFILE(&DBSLIB/FENTP001) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FENTP003) TYPE(*REFCST) +
                          KEY(CODIGO) CST(FENTP003_RL) +
                          PRNFILE(&DBSLIB/FENTP001) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FENTP004) TYPE(*REFCST) +
                          KEY(CODIGO) CST(FENTP004_RL) +
                          PRNFILE(&DBSLIB/FENTP001) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FENTP005) TYPE(*REFCST) +
                          KEY(CODIGO) CST(FENTP005_RL) +
                          PRNFILE(&DBSLIB/FENTP001) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FENTP006) TYPE(*REFCST) +
                          KEY(CDRELAC) CST(FENTP006_RL) +
                          PRNFILE(&DBSLIB/FENTP001) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ENDPGM
