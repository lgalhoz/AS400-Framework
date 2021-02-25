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
          DCL        VAR(&DBSLIB) TYPE(*CHAR) LEN(10)

          CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MODULO *CAT 'DBS')

             /****************************************************************/
             /* Adicionar Referencial de Integridade                         */
             /****************************************************************/
             RMVPFCST   FILE(&DBSLIB/FPRDP003) CST(FPRDP003_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP008) CST(FPRDP008_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP009) CST(FPRDP009_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP022) CST(FPRDP022_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP030) CST(FPRDP030_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP011) CST(FPRDP011_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP014) CST(FPRDP014_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP016) CST(FPRDP016_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP018) CST(FPRDP018_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP010) CST(FPRDP010_RL1)
             RMVPFCST   FILE(&DBSLIB/FPRDP010) CST(FPRDP010_RL2)
             RMVPFCST   FILE(&DBSLIB/FPRDP012) CST(FPRDP012_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP013) CST(FPRDP013_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP015) CST(FPRDP015_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP017) CST(FPRDP017_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP010) CST(FPRDP010_PK)
             RMVPFCST   FILE(&DBSLIB/FPRDT001) CST(FPRDT001_PK)

             /* CONTROLO DE INDEXAÇÃO */
             RMVPFCST   FILE(&DBSLIB/FPRDP121) CST(FPRDP121_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP122) CST(FPRDP122_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP123) CST(FPRDP123_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP122) CST(FPRDP122_PK)
             RMVPFCST   FILE(&DBSLIB/FPRDP120) CST(FPRDP120_PK)

             ADDPFCST   FILE(&DBSLIB/FPRDT001) TYPE(*PRIKEY) +
                          KEY(CDRAMO NUAPOL) CST(FPRDT001_PK)
             ADDPFCST   FILE(&DBSLIB/FPRDP010) TYPE(*PRIKEY) +
                          KEY(CDRAMO NUAPOL NUSEQU) CST(FPRDP010_PK)

             ADDPFCST   FILE(&DBSLIB/FPRDP003) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL) CST(FPRDP003_RL) +
                          PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP008) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL) CST(FPRDP008_RL) +
                          PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP030) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL) CST(FPRDP030_RL) +
                          PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP009) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL) CST(FPRDP009_RL) +
                          PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP022) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL) CST(FPRDP022_RL) +
                          PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP011) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL) CST(FPRDP011_RL) +
                          PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP014) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL) CST(FPRDP014_RL) +
                          PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP016) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL) CST(FPRDP016_RL) +
                          PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP018) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL) CST(FPRDP018_RL) +
                          PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)

             ADDPFCST   FILE(&DBSLIB/FPRDP010) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL) CST(FPRDP010_RL1) +
                          PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP010) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL NUSQSUP) +
                          CST(FPRDP010_RL2) +
                          PRNFILE(&DBSLIB/FPRDP010) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP012) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL NUSEQU) +
                          CST(FPRDP012_RL) +
                          PRNFILE(&DBSLIB/FPRDP010) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP013) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL NUSEQU) +
                          CST(FPRDP013_RL) +
                          PRNFILE(&DBSLIB/FPRDP010) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP015) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL NUSEQU) +
                          CST(FPRDP015_RL) +
                          PRNFILE(&DBSLIB/FPRDP010) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP017) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL NUSEQU) +
                          CST(FPRDP017_RL) +
                          PRNFILE(&DBSLIB/FPRDP010) UPDRULE(*RESTRICT)


             /* CONTROLO DE INDEXAÇÕES */
             ADDPFCST   FILE(&DBSLIB/FPRDP120) TYPE(*PRIKEY) +
                          KEY(CODIDX) CST(FPRDP120_PK)
             ADDPFCST   FILE(&DBSLIB/FPRDP122) TYPE(*PRIKEY) +
                          KEY(CODIDX NUSEQU) CST(FPRDP122_PK)

             ADDPFCST   FILE(&DBSLIB/FPRDP121) TYPE(*REFCST) +
                          KEY(CODIDX) CST(FPRDP121_RL) +
                          PRNFILE(&DBSLIB/FPRDP120) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP122) TYPE(*REFCST) +
                          KEY(CODIDX) CST(FPRDP122_RL) +
                          PRNFILE(&DBSLIB/FPRDP120) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FPRDP123) TYPE(*REFCST) +
                          KEY(CODIDX NUSEQU) CST(FPRDP123_RL) +
                          PRNFILE(&DBSLIB/FPRDP122) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ENDPGM
