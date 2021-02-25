/*******************************************************************/
/* Programa..: PCATC004                                            */
/* Objectivo.: Criar as Tabelas.                                   */
/*******************************************************************/
/* Observação: É testada a existência das tabelas na biblioteca de */
/*             objectos de destino, e se existir então altera.     */
/*******************************************************************/
             PGM    PARM(&AMB &MOD)
             DCL    VAR(&MOD) TYPE(*CHAR) LEN(3)
             DCL    VAR(&AMB) TYPE(*CHAR) LEN(3)
             DCL    VAR(&SRCLIB) TYPE(*CHAR) LEN(10)
             DCL    VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
             DCL    VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
             DCL    VAR(&APP001) TYPE(*CHAR) LEN(10)
             DCL    VAR(&APP002) TYPE(*CHAR) LEN(10)

             CHGVAR VAR(&OBJLIB) VALUE('#JM' *CAT &AMB *CAT 'OBJ')
             CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &AMB *CAT 'DBS')
             CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &AMB *CAT 'SRC')
             CHGVAR VAR(&APP001) VALUE('P' *CAT &MOD *CAT 'C005')
             CHGVAR VAR(&APP002) VALUE('P' *CAT &MOD *CAT 'C006')

             CHGLIBL    LIBL(QTEMP QGPL &OBJLIB &DBSLIB &SRCLIB)

             CALL       PGM(&SRCLIB/&APP001) PARM(&AMB)
             CALL       PGM(&SRCLIB/&APP002) PARM(&AMB)

             ENDPGM
