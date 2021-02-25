/*******************************************************************/
/* Programa..: PCATC005                                            */
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
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP010)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP01001)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP100)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP10001)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP101)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP102)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP10201)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP10202)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP103)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP10301)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP10302)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP104)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP105)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP106)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP10601)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP107)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP10701)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP108)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP10801)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP109)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP110)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP11001)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP111)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP112)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP113)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP114)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP115)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP116)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP117)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP118)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP11801)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FCATP120)
          ENDPGM
