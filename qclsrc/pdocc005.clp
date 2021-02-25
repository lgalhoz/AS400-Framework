/*******************************************************************/
/* Programa..: PDOCC005                                            */
/* Objectivo.: Criar as Tabelas.                                   */
/*******************************************************************/
/* Observação: É testada a existência das tabelas na biblioteca de */
/*             objectos de destino, e se existir então altera.     */
/*******************************************************************/
          PGM
          DCL  VAR(&MOD) TYPE(*CHAR) LEN(3) VALUE('PRD')
          DCL  VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
          DCL  VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
          DCL  VAR(&SRCLIB) TYPE(*CHAR) LEN(10)

          CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MOD *CAT 'DBS')
          CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &MOD *CAT 'SRC')

             /****************************************************************/
             /* Gerar a base de dados                                        */
             /****************************************************************/
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP001)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP002)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP003)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP004)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP005)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP015)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP01501)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP016)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP01601)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP017)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP030)
             CALL PGM(&SRCLIB/PSYSC001) PARM(&MOD FDOCP03001)
             ENDPGM
