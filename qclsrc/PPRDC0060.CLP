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
             RMVPFCST   FILE(&DBSLIB/FPRDP010) CST(FPRDP010_RL2)
             RMVPFCST   FILE(&DBSLIB/FPRDP010) CST(FPRDP010_RL1)
             RMVPFCST   FILE(&DBSLIB/FPRDP012) CST(FPRDP012_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP013) CST(FPRDP013_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP015) CST(FPRDP015_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP010) CST(FPRDP012_PK)
             RMVPFCST   FILE(&DBSLIB/FPRDP017) CST(FPRDP017_RL)
             RMVPFCST   FILE(&DBSLIB/FPRDP010) CST(FPRDP010_PK)
             RMVPFCST   FILE(&DBSLIB/FPRDP012) CST(FPRDP012_PK)

             ADDPFCST   FILE(&DBSLIB/FPRDP010) TYPE(*PRIKEY) +
                          KEY(CDRAMO NUAPOL NUSEQU) CST(FPRDP010_PK)
             ADDPFCST   FILE(&DBSLIB/FPRDP010) TYPE(*REFCST) +
                          KEY(CDRAMO NUAPOL NUSQSUP) CST(FPRDP010_RL1) +
                          PRNFILE(&DBSLIB/FPRDP010) UPDRULE(*RESTRICT)

         /*  ADDPFCST   FILE(&DBSLIB/FPRDP010) TYPE(*REFCST) +            */
         /*               KEY(CDRAMO NUAPOL) CST(FPRDP010_RL2) +          */
         /*               PRNFILE(&DBSLIB/FPRDT001) UPDRULE(*RESTRICT)    */

             ADDPFCST   FILE(&DBSLIB/FPRDP012) TYPE(*PRIKEY) +
                          KEY(CDRAMO NUAPOL NUSEQU NUSEQHST) +
                          CST(FPRDP012_PK)

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
             ENDPGM
