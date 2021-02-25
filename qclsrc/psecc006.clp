/*******************************************************************/
/* Programa..: PSECC010                                            */
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
             RMVPFCST   FILE(&DBSLIB/FSECP030) CST(FSECP030_RL1)
             RMVPFCST   FILE(&DBSLIB/FSECP030) CST(FSECP030_RL2)
             RMVPFCST   FILE(&DBSLIB/FSECP012) CST(FSECP012_RL)
             RMVPFCST   FILE(&DBSLIB/FSECP011) CST(FSECP011_RL)
             RMVPFCST   FILE(&DBSLIB/FSECP014) CST(FSECP014_RL)
             RMVPFCST   FILE(&DBSLIB/FSECP012) CST(FSECP012_PK)
             RMVPFCST   FILE(&DBSLIB/FSECP011) CST(FSECP011_PK)
             RMVPFCST   FILE(&DBSLIB/FSECP010) CST(FSECP010_PK)
             RMVPFCST   FILE(&DBSLIB/FSECP020) CST(FSECP020_PK)
             RMVPFCST   FILE(&DBSLIB/FSECP016) CST(FSECP016_RL)
             RMVPFCST   FILE(&DBSLIB/FSECP015) CST(FSECP015_PK)
             RMVPFCST   FILE(&DBSLIB/FSECP030) CST(FSECP071_RL)
             RMVPFCST   FILE(&DBSLIB/FSECP015) CST(FSECP070_PK)

             /*=====================================================*/
             /* REGISTO DE LOG DE OPERAÇOES                         */
             /*=====================================================*/
             RMVPFCST   FILE(&DBSLIB/FSECP051) CST(FSECP051_RL)
             RMVPFCST   FILE(&DBSLIB/FSECP053) CST(FSECP053_RL)
             RMVPFCST   FILE(&DBSLIB/FSECP052) CST(FSECP052_PK)
             RMVPFCST   FILE(&DBSLIB/FSECP052) CST(FSECP052_RL)
             RMVPFCST   FILE(&DBSLIB/FSECP050) CST(FSECP050_RL)
             RMVPFCST   FILE(&DBSLIB/FSECP050) CST(FSECP050_PK)

             /*=====================================================*/
             /* REGRAS                                              */
             /*=====================================================*/
             RMVPFCST   FILE(&DBSLIB/FSECP041) CST(FSECP041_RL)
             RMVPFCST   FILE(&DBSLIB/FSECP040) CST(FSECP040_PK)

             ADDPFCST   FILE(&DBSLIB/FSECP020) TYPE(*PRIKEY) +
                          KEY(CDUSER)   CST(FSECP020_PK)
             ADDPFCST   FILE(&DBSLIB/FSECP010) TYPE(*PRIKEY) +
                          KEY(CDPROGR) CST(FSECP010_PK)
             ADDPFCST   FILE(&DBSLIB/FSECP011) TYPE(*PRIKEY) +
                          KEY(CDPROGR CDFUNCT) CST(FSECP011_PK)
             ADDPFCST   FILE(&DBSLIB/FSECP012) TYPE(*PRIKEY) +
                          KEY(CDPROGR CDFUNCT CDOPERA) CST(FSECP012_PK)
             ADDPFCST   FILE(&DBSLIB/FSECP015) TYPE(*PRIKEY) +
                          KEY(CDLIBL)   CST(FSECP015_PK)

             ADDPFCST   FILE(&DBSLIB/FSECP012) TYPE(*REFCST) +
                          KEY(CDPROGR CDFUNCT) CST(FSECP012_RL) +
                          PRNFILE(&DBSLIB/FSECP011) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FSECP011) TYPE(*REFCST) +
                          KEY(CDPROGR) CST(FSECP011_RL) +
                          PRNFILE(&DBSLIB/FSECP010) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FSECP014) TYPE(*REFCST) +
                          KEY(CDMENU) CST(FSECP014_RL) +
                          PRNFILE(&DBSLIB/FSECP010) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FSECP030) TYPE(*REFCST) +
                          KEY(CDPROGR CDFUNCT CDOPERA) +
                          CST(FSECP030_RL1) PRNFILE(&DBSLIB/FSECP012) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FSECP030) TYPE(*REFCST) +
                          KEY(CDUSER) CST(FSECP030_RL2) +
                          PRNFILE(&DBSLIB/FSECP020) DLTRULE(*CASCADE) +
                          UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FSECP016) TYPE(*REFCST) +
                          KEY(CDLIBL) CST(FSECP016_RL) +
                          PRNFILE(&DBSLIB/FSECP015) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             /*=====================================================*/
             /* REGRAS                                              */
             /*=====================================================*/
             ADDPFCST   FILE(&DBSLIB/FSECP040) TYPE(*PRIKEY) +
                          KEY(SQREGRA)   CST(FSECP040_PK)
             ADDPFCST   FILE(&DBSLIB/FSECP050) TYPE(*PRIKEY) +
                          KEY(SQLOG)   CST(FSECP050_PK)
             ADDPFCST   FILE(&DBSLIB/FSECP052) TYPE(*PRIKEY) +
                          KEY(SQLOG NUSEQ) CST(FSECP052_PK)

             ADDPFCST   FILE(&DBSLIB/FSECP041) TYPE(*REFCST) +
                          KEY(SQREGRA) CST(FSECP041_RL) +
                          PRNFILE(&DBSLIB/FSECP040) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FSECP051) TYPE(*REFCST) +
                          KEY(SQLOG) CST(FSECP051_RL) +
                          PRNFILE(&DBSLIB/FSECP050) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FSECP052) TYPE(*REFCST) +
                          KEY(SQLOG) CST(FSECP052_RL) +
                          PRNFILE(&DBSLIB/FSECP050) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ADDPFCST   FILE(&DBSLIB/FSECP053) TYPE(*REFCST) +
                          KEY(SQLOG NUSEQ) CST(FSECP053_RL) +
                          PRNFILE(&DBSLIB/FSECP052) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)

             /*=====================================================*/
             /* CONTROLO DE CADEIAS BATCH                           */
             /*=====================================================*/
             ADDPFCST   FILE(&DBSLIB/FSECP070) TYPE(*PRIKEY) +
                          KEY(CDBATCH)   CST(FSECP070_PK)

             ADDPFCST   FILE(&DBSLIB/FSECP071) TYPE(*REFCST) +
                          KEY(CDBATCH) CST(FSECP071_RL) +
                          PRNFILE(&DBSLIB/FSECP070) +
                          DLTRULE(*CASCADE) UPDRULE(*RESTRICT)
             ENDPGM
