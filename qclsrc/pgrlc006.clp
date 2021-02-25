/*******************************************************************/
/* Programa..: PGRLP010                                            */
/* Objectivo.: Actualizar o referencial de Integridade da Base de  */
/*             dados.                                              */
/*******************************************************************/
/* Observação: Executar sempre que alterar alguma tabela deste     */
/*             grupo.                                              */
/*******************************************************************/
             PGM

             DCL VAR(&MODULO) TYPE(*CHAR) LEN(3) VALUE('PRD')
             DCL VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
             CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MODULO *CAT 'DBS')

          /*============================================================================*/
          /* Adicionar Referencial de Integridade                                       */
          /*============================================================================*/
          /* RMVPFCST   FILE(&DBSLIB/FGRLP010) CST(FGRLP010_RL)                         */
          /* RMVPFCST   FILE(&DBSLIB/FGRLP008) CST(FGRLP008_PK)                         */
          /*                                                                            */
          /* ADDPFCST   FILE(&DBSLIB/FGRLP008) TYPE(*PRIKEY) +                          */
          /*               KEY(CDPAGINA)   CST(FGRLP008_PK)                             */
          /*  ADDPFCST   FILE(&DBSLIB/FGRLP010) TYPE(*REFCST) +                         */
          /*               KEY(CDPAGINA) CST(FGRLP010_RL) +                             */
          /*               PRNFILE(&DBSLIB/FGRLP008) DLTRULE(*CASCADE) +                */
          /*               UPDRULE(*RESTRICT)                                           */
          /*                                                                            */
          /*============================================================================*/
          /* Adicionar Referencial de Integridade                       /* RELA -> PKEY */
          /*============================================================================*/
             RMVPFCST   FILE(&DBSLIB/FGRLP025) CST(FGRLP025_RL1)        /* P025 -> P024 */
             RMVPFCST   FILE(&DBSLIB/FGRLP025) CST(FGRLP025_RL2)        /* P025 -> P027 */
             RMVPFCST   FILE(&DBSLIB/FGRLP026) CST(FGRLP026_RL2)        /* P026 -> P027 */
             RMVPFCST   FILE(&DBSLIB/FGRLP023) CST(FGRLP023_RL2)        /* P023 -> P024 */
             RMVPFCST   FILE(&DBSLIB/FGRLP026) CST(FGRLP026_RL1)        /* P026 -> P023 */
             RMVPFCST   FILE(&DBSLIB/FGRLP023) CST(FGRLP023_RL1)        /* P023 -> P020 */

             RMVPFCST   FILE(&DBSLIB/FGRLP020) CST(FGRLP020_PK)
             ADDPFCST   FILE(&DBSLIB/FGRLP020) TYPE(*PRIKEY) KEY(CDBATCH) CST(FGRLP020_PK)
             RMVPFCST   FILE(&DBSLIB/FGRLP023) CST(FGRLP023_PK)
             ADDPFCST   FILE(&DBSLIB/FGRLP023) TYPE(*PRIKEY) KEY(CDBATCH NUSEQU) CST(FGRLP023_PK)
             RMVPFCST   FILE(&DBSLIB/FGRLP024) CST(FGRLP024_PK)
             ADDPFCST   FILE(&DBSLIB/FGRLP024) TYPE(*PRIKEY) KEY(CDFILE) CST(FGRLP024_PK)
             RMVPFCST   FILE(&DBSLIB/FGRLP027) CST(FGRLP027_PK)
             ADDPFCST   FILE(&DBSLIB/FGRLP027) TYPE(*PRIKEY) KEY(CDFIELD) CST(FGRLP027_PK)

             ADDPFCST   FILE(&DBSLIB/FGRLP023) TYPE(*REFCST) +
                          KEY(CDBATCH) CST(FGRLP023_RL1) +
                          PRNFILE(&DBSLIB/FGRLP020) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)                            /* P023 -> P020 */
             ADDPFCST   FILE(&DBSLIB/FGRLP024) TYPE(*REFCST) +
                          KEY(CDFILE) CST(FGRLP023_RL2) +
                          PRNFILE(&DBSLIB/FGRLP024) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)                            /* P023 -> P024 */
             ADDPFCST   FILE(&DBSLIB/FGRLP025) TYPE(*REFCST) +
                          KEY(CDFILE) CST(FGRLP025_RL1) +
                          PRNFILE(&DBSLIB/FGRLP024) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)                            /* P025 -> P024 */
             ADDPFCST   FILE(&DBSLIB/FGRLP025) TYPE(*REFCST) +
                          KEY(CDFIELD) CST(FGRLP025_RL2) +
                          PRNFILE(&DBSLIB/FGRLP027) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)                            /* P025 -> P027 */
             ADDPFCST   FILE(&DBSLIB/FGRLP026) TYPE(*REFCST) +
                          KEY(CDBATCH NUSEQU) CST(FGRLP026_RL1) +
                          PRNFILE(&DBSLIB/FGRLP023) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)                            /* P026 -> P023 */
             ADDPFCST   FILE(&DBSLIB/FGRLP026) TYPE(*REFCST) +
                          KEY(CDFIELD) CST(FGRLP026_RL2) +
                          PRNFILE(&DBSLIB/FGRLP027) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)                            /* P026 -> P027 */
             ENDPGM
