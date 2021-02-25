/*******************************************************************/
/* Programa..: PTECC006                                            */
/* Objectivo.: Actualizar o referencial de Integridade da Base de  */
/*             dados.                                              */
/*******************************************************************/
/* Observação: Executar sempre que alterar alguma tabela deste     */
/*             grupo.                                              */
/*******************************************************************/
          PGM

          DCL  VAR(&MODULO) TYPE(*CHAR) LEN(3) VALUE('PRD')
          DCL        VAR(&DBSLIB) TYPE(*CHAR) LEN(10)

          CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MODULO *CAT 'DBS')

             /****************************************************************/
             /* Adicionar Referencial de Integridade                         */
             /****************************************************************/
             RMVPFCST   FILE(&DBSLIB/FTECP011) CST(FTECP011_RL)
             RMVPFCST   FILE(&DBSLIB/FTECP012) CST(FTECP012_RL)
             RMVPFCST   FILE(&DBSLIB/FTECP011) CST(FTECP011_PK)
             RMVPFCST   FILE(&DBSLIB/FTECP010) CST(FTECP010_PK)

             ADDPFCST   FILE(&DBSLIB/FTECP010) TYPE(*PRIKEY) +
                          KEY(CDFILE) CST(FTECP010_PK)
             ADDPFCST   FILE(&DBSLIB/FTECP011) TYPE(*REFCST) +
                          KEY(CDFILE) CST(FTECP011_RL) +
                          PRNFILE(&DBSLIB/FTECP010) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FTECP011) TYPE(*PRIKEY) +
                          KEY(CDFILE CDFMT) CST(FTECP011_PK)
             ADDPFCST   FILE(&DBSLIB/FTECP012) TYPE(*REFCST) +
                          KEY(CDFILE CDFMT) CST(FTECP012_RL) +
                          PRNFILE(&DBSLIB/FTECP011) UPDRULE(*RESTRICT)
             ENDPGM
