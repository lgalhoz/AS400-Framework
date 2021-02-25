/*******************************************************************/
/* Programa..: PDBSSSIN05                                          */
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
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP100)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP101)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP102)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP10201)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP103)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP10301)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP104)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP10401)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP105)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP106)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP107)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP108)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP109)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP110)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP111)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP112)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP114)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP11401)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FSINP11402)
          ENDPGM
