/*******************************************************************/
/* Programa..: PTBLC005                                            */
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
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FTBLP010)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FTBLP011)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FTBLP012)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FTBLP01201)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FTBLP01202)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FTBLP013)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FTBLP01301)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FTBLP01302)
             ENDPGM
