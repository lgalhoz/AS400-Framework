/*******************************************************************/
/* Programa..: PTBLC010                                            */
/* Objectivo.: Actualizar o referencial de Integridade da Base de  */
/*             dados.                                              */
/*******************************************************************/
/* Observação: Executar sempre que alterar alguma tabela deste     */
/*             grupo.                                              */
/*******************************************************************/
             PGM

             DCL        VAR(&MODULO) TYPE(*CHAR) LEN(3) VALUE('PRD')
             DCL        VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
             CHGVAR     VAR(&DBSLIB) VALUE('#JM' *CAT &MODULO *CAT +
                          'DBS')

             /****************************************************************/
             /* Adicionar Referencial de Integridade                         */
             /****************************************************************/
             RMVPFCST   FILE(&DBSLIB/FCATP110) CST(FCATP110_RL)
             RMVPFCST   FILE(&DBSLIB/FCATP113) CST(FCATP113_RL)
             RMVPFCST   FILE(&DBSLIB/FCATP114) CST(FCATP114_RL)
             RMVPFCST   FILE(&DBSLIB/FCATP104) CST(FCATP104_RL)
             RMVPFCST   FILE(&DBSLIB/FCATP105) CST(FCATP105_RL1)
             RMVPFCST   FILE(&DBSLIB/FCATP105) CST(FCATP105_RL2)
             RMVPFCST   FILE(&DBSLIB/FCATP101) CST(FCATP101_RL)
             RMVPFCST   FILE(&DBSLIB/FCATP101) CST(FCATP101_PK)
             RMVPFCST   FILE(&DBSLIB/FCATP107) CST(FCATP107_RL)
             RMVPFCST   FILE(&DBSLIB/FCATP102) CST(FCATP102_RL1)
             RMVPFCST   FILE(&DBSLIB/FCATP102) CST(FCATP102_RL2)
             RMVPFCST   FILE(&DBSLIB/FCATP109) CST(FCATP109_RL)
             RMVPFCST   FILE(&DBSLIB/FCATP108) CST(FCATP108_RL)
             RMVPFCST   FILE(&DBSLIB/FCATP108) CST(FCATP108_PK)
             RMVPFCST   FILE(&DBSLIB/FCATP103) CST(FCATP103_RL1)
             RMVPFCST   FILE(&DBSLIB/FCATP103) CST(FCATP103_RL2)
             RMVPFCST   FILE(&DBSLIB/FCATP103) CST(FCATP103_PK)
             RMVPFCST   FILE(&DBSLIB/FCATP106) CST(FCATP106_RL1)
             RMVPFCST   FILE(&DBSLIB/FCATP106) CST(FCATP106_RL2)
             RMVPFCST   FILE(&DBSLIB/FCATP106) CST(FCATP106_PK)
             RMVPFCST   FILE(&DBSLIB/FCATP100) CST(FCATP100_PK)
             RMVPFCST   FILE(&DBSLIB/FCATP102) CST(FCATP102_PK)

             ADDPFCST   FILE(&DBSLIB/FCATP100) TYPE(*PRIKEY) +
                          KEY(CDPROD) CST(FCATP100_PK)
             ADDPFCST   FILE(&DBSLIB/FCATP101) TYPE(*PRIKEY) +
                          KEY(CDPROD CDRISCO) CST(FCATP101_PK)
             ADDPFCST   FILE(&DBSLIB/FCATP102) TYPE(*PRIKEY) +
                          KEY(CDPROD CDCOBERT) CST(FCATP102_PK)
             ADDPFCST   FILE(&DBSLIB/FCATP103) TYPE(*PRIKEY) +
                          KEY(CDPROD CDLIMITE) CST(FCATP103_PK)
             ADDPFCST   FILE(&DBSLIB/FCATP106) TYPE(*PRIKEY) +
                          KEY(CDPROD CDTEXTO) CST(FCATP106_PK)
             ADDPFCST   FILE(&DBSLIB/FCATP108) TYPE(*PRIKEY) +
                          KEY(CDPROD NUSEQU) CST(FCATP108_PK)

             ADDPFCST   FILE(&DBSLIB/FCATP110) TYPE(*REFCST) +
                          KEY(CDPROD) CST(FCATP110_RL) +
                          PRNFILE(&DBSLIB/FCATP100) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FCATP113) TYPE(*REFCST) +
                          KEY(CDPROD) CST(FCATP113_RL) +
                          PRNFILE(&DBSLIB/FCATP100) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FCATP114) TYPE(*REFCST) +
                          KEY(CDPROD) CST(FCATP114_RL) +
                          PRNFILE(&DBSLIB/FCATP100) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FCATP104) TYPE(*REFCST) +
                          KEY(CODPROD) CST(FCATP104_RL) +
                          PRNFILE(&DBSLIB/FCATP100) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FCATP105) TYPE(*REFCST) +
                          KEY(CDPROD CDRISCO) CST(FCATP105_RL1) +
                          PRNFILE(&DBSLIB/FCATP101) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FCATP105) TYPE(*REFCST) +
                          KEY(CDPROD CDTEXTO) CST(FCATP105_RL2) +
                          PRNFILE(&DBSLIB/FCATP106) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FCATP101) TYPE(*REFCST) +
                          KEY(CDPROD) CST(FCATP101_RL) +
                          PRNFILE(&DBSLIB/FCATP100) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FCATP107) TYPE(*REFCST) +
                          KEY(CDPROD CDCOBERT) CST(FCATP107_RL) +
                          PRNFILE(&DBSLIB/FCATP102) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FCATP102) TYPE(*REFCST) +
                          KEY(CDPROD) CST(FCATP102_RL1) +
                          PRNFILE(&DBSLIB/FCATP100) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FCATP102) TYPE(*REFCST) +
                          KEY(CDPROD CDSUPER) CST(FCATP102_RL2) +
                          PRNFILE(&DBSLIB/FCATP102) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FCATP103) TYPE(*REFCST) +
                          KEY(CDPROD CDCOBERT) CST(FCATP103_RL2) +
                          PRNFILE(&DBSLIB/FCATP102) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FCATP103) TYPE(*REFCST) +
                          KEY(CDPROD CDSUPER) CST(FCATP103_RL1) +
                          PRNFILE(&DBSLIB/FCATP103) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FCATP106) TYPE(*REFCST) +
                          KEY(CDPROD) CST(FCATP106_RL2) +
                          PRNFILE(&DBSLIB/FCATP100) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FCATP106) TYPE(*REFCST) +
                          KEY(CDPROD CDSUPER) CST(FCATP106_RL1) +
                          PRNFILE(&DBSLIB/FCATP106) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FCATP109) TYPE(*REFCST) +
                          KEY(CDPROD) CST(FCATP109_RL) +
                          PRNFILE(&DBSLIB/FCATP100) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ENDPGM
