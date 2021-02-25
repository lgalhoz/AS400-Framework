/*******************************************************************/
/* Programa..: PGRLC005                                            */
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
          /*                                                              */
          /****************************************************************/
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP008)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP010)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP012)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP013)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP021)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP022)

          /****************************************************/
          /* Controlo de Ficheiros Temporários                */
          /****************************************************/
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP020)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP023)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP024)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP025)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP026)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP027)

          /****************************************************/
          /*                                                  */
          /****************************************************/
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP200)
          CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FGRLP201)
          ENDPGM
