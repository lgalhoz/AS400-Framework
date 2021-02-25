/*******************************************************************/
/* Programa..: PSYSC020                                            */
/* Objectivo.: Actualizar Todas as Bibliotecas de Fun��es          */
/*******************************************************************/
/* Observa��o: Executar sempre que realizar alguma altera��o ou    */
/*             inclus�o de um novo m�dulo.                         */
/*             Os programa tem de ser compilados para reflectir as */
/*             novas altera��es.                                   */
/*******************************************************************/
             PGM
             DCL  VAR(&MOD) TYPE(*CHAR) LEN(3)  VALUE('PRD')
             DCL  VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
             DCL  VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
             DCL  VAR(&SRCLIB) TYPE(*CHAR) LEN(10)

             CHGVAR VAR(&OBJLIB) VALUE('#JM' *CAT &MOD *CAT 'OBJ')
             CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MOD *CAT 'DBS')
             CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &MOD *CAT 'SRC')


             /****************************************************************/
             /* Correr os Programas Para actualiza��o do Direct�rio fun��es  */
             /****************************************************************/
             CALL       PGM(&SRCLIB/PGRLC020) /* PARM(&MOD) */
             CALL       PGM(&SRCLIB/PDBSC020) /* PARM(&MOD) */
             CALL       PGM(&SRCLIB/PCATC020) /* PARM(&MOD) */
             CALL       PGM(&SRCLIB/PENTC020) /* PARM(&MOD) */
             CALL       PGM(&SRCLIB/PPRDC020) /* PARM(&MOD) */
             CALL       PGM(&SRCLIB/PSECC020) /* PARM(&MOD) */
             CALL       PGM(&SRCLIB/PTBLC020) /* PARM(&MOD) */
             ENDPGM
