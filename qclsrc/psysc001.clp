/*******************************************************************/
/* Programa..: PGRLC001                                            */
/* Objectivo.: Criar/Alterar um ficheiro físico                    */
/*******************************************************************/
/* Observação: É testada a existência das tabelas na biblioteca de */
/*             objectos de destino, e se existir então altera.     */
/*******************************************************************/
             PGM  PARM(&AMB &FILNAME)
             DCL  VAR(&AMB) TYPE(*CHAR) LEN(3)
             DCL  VAR(&MOD) TYPE(*CHAR) LEN(3)
             DCL  VAR(&REM) TYPE(*CHAR) LEN(2)
             DCL  VAR(&SRCLIB) TYPE(*CHAR) LEN(10)
             DCL  VAR(&FILNAME) TYPE(*CHAR) LEN(10)
             DCL  VAR(&APPNAME) TYPE(*CHAR) LEN(10)

             /****************************************************************/
             /* CHAMAR PROGRAMA EM FUNÇÃO DO TIPO DE FICHEIRO                */
             /****************************************************************/
             CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &AMB *CAT 'SRC')
             CHGVAR VAR(&MOD) VALUE(%SST(&FILNAME 2 3))
             CHGVAR VAR(&REM) VALUE(%SST(&FILNAME 9 2))
             CHGVAR VAR(&APPNAME) VALUE('PSYSC003')
             IF         COND(&REM *EQ '  ') THEN(DO)
             CHGVAR VAR(&APPNAME) VALUE('PSYSC002')
             ENDDO
             CALL       PGM(&SRCLIB/&APPNAME) PARM(&AMB &FILNAME)
             ENDPGM
