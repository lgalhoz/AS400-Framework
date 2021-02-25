/*******************************************************************/
/* Programa..: PPRDC005                                            */
/* Objectivo.: Criar as Tabelas.                                   */
/*******************************************************************/
/* Observação: É testada a existência das tabelas na biblioteca de */
/*             objectos de destino, e se existir então altera.     */
/*******************************************************************/
          PGM
          DCL  VAR(&MOD) TYPE(*CHAR) LEN(3) VALUE('PRD')
          DCL  VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
          DCL  VAR(&SRCLIB) TYPE(*CHAR) LEN(10)

          CHGVAR VAR(&MOD) VALUE('PRD')
          CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MOD *CAT 'DBS')
          CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &MOD *CAT 'SRC')

          /****************************************************************/
          /* Gerar a base de dados                                        */
          /****************************************************************/
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP001)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDT001)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP003)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP008)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP00801)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP009)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP00901)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP010)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP01001)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP01002)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP011)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP01101)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP012)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP013)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP014)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP01401)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP015)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP016)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP017)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP018)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP020)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP022)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP030)

             /*==> TABELAS USADAS PELAS INDEXAÇÃO  */
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP120)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP121)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP122)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP12201)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP123)

             /*==> TABELAS USADAS PELO MS.ACCESS */
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP100)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP101)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP10101)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP102)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP103)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP104)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP105)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FPRDP106)
          ENDPGM
