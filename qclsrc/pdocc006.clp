/*******************************************************************/
/* Programa..: PDOCC010                                            */
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
             RMVPFCST   FILE(&DBSLIB/FDOCP003) CST(FDOCP003_RL)
             RMVPFCST   FILE(&DBSLIB/FDOCP002) CST(FDOCP002_RL)
             RMVPFCST   FILE(&DBSLIB/FDOCP004) CST(FDOCP004_RL)
             RMVPFCST   FILE(&DBSLIB/FDOCP002) CST(FDOCP002_PK)
             RMVPFCST   FILE(&DBSLIB/FDOCP001) CST(FDOCP001_PK)

             ADDPFCST   FILE(&DBSLIB/FDOCP001) TYPE(*PRIKEY) +
                          KEY(CDDOCUM)  CST(FDOCP001_PK)
             ADDPFCST   FILE(&DBSLIB/FDOCP002) TYPE(*PRIKEY) +
                          KEY(CDDOCUM NUPAGINA)  CST(FDOCP002_PK)

             ADDPFCST   FILE(&DBSLIB/FDOCP003) TYPE(*REFCST) +
                          KEY(CDDOCUM NUPAGINA) CST(FDOCP003_RL) +
                          PRNFILE(&DBSLIB/FDOCP002) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FDOCP002) TYPE(*REFCST) +
                          KEY(CDDOCUM) CST(FDOCP002_RL) +
                          PRNFILE(&DBSLIB/FDOCP001) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FDOCP004) TYPE(*REFCST) +
                          KEY(CDDOCUM) CST(FDOCP004_RL) +
                          PRNFILE(&DBSLIB/FDOCP001) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)
             ENDPGM
