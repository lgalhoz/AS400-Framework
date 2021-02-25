/*******************************************************************/
/* Programa..: PINTC005                                            */
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
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FINTP001)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FINTP002)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FINTP003)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FINTP004)
             ENDPGM
