/*******************************************************************/
/* Programa..: PSECC005                                            */
/* Objectivo.: Criar as Tabelas.                                   */
/*******************************************************************/
/* Observação: É testada a existência das tabelas na biblioteca de */
/*             objectos de destino, e se existir então altera.     */
/*******************************************************************/
          PGM
          DCL  VAR(&MOD) TYPE(*CHAR) LEN(3) VALUE('PRD')
          DCL  VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
          DCL  VAR(&SRCLIB) TYPE(*CHAR) LEN(10)

          CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MOD *CAT 'DBS')
          CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &MOD *CAT 'SRC')

          /****************************************************************/
          /* Gerar a base de dados                                        */
          /****************************************************************/
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP010)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP01001)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP01002)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP011)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP012)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP013)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP014)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP015)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP016)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP017)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP020)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP021)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP030)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP031)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP03101)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP032)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP033)

          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP040)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP041)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP050)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP051)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP052)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP053)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP060)

          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP070)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSECP071)
          ENDPGM
