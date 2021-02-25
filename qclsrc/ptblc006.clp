/*******************************************************************/
/* Programa..: PTBLC010                                            */
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
             RMVPFCST   FILE(&DBSLIB/FTBLP012) CST(FTBLP012_RL)
             RMVPFCST   FILE(&DBSLIB/FTBLP011) CST(FTBLP011_RL)
             RMVPFCST   FILE(&DBSLIB/FTBLP010) CST(FTBLP010_PK)

             ADDPFCST   FILE(&DBSLIB/FTBLP010) TYPE(*PRIKEY) +
                          KEY(CDINDICE) CST(FTBLP010_PK)
             ADDPFCST   FILE(&DBSLIB/FTBLP011) TYPE(*REFCST) +
                          KEY(CDINDICE) CST(FTBLP011_RL) +
                          PRNFILE(&DBSLIB/FTBLP010) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FTBLP012) TYPE(*REFCST) +
                          KEY(CDINDICE) CST(FTBLP012_RL) +
                          PRNFILE(&DBSLIB/FTBLP010) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)
             ENDPGM
