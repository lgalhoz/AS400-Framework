/*******************************************************************/
/* Programa..: PGRLC010                                            */
/* Objectivo.: Actualizar BIBLIOTECAS DE FUNÇÕES.                  */
/*******************************************************************/
             PGM
             DCL        VAR(&MOD) TYPE(*CHAR) LEN(3) VALUE('PRD')
             DCL        VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
             DCL        VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
             DCL        VAR(&SRCLIB) TYPE(*CHAR) LEN(10)

             CHGVAR VAR(&OBJLIB) VALUE('#JM' *CAT &MOD *CAT 'OBJ')
             CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &MOD *CAT 'DBS')
             CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &MOD *CAT 'SRC')

             /****************************************************************/
             /* GERAR PROGRAMAS DO SISTEMA                                   */
             /****************************************************************/
             /* CALL       PGM(&SRCLIB/PDBSC025) PARM(&MOD)                  */
             CALL       PGM(&SRCLIB/PCATC025) PARM(&MOD)
             CALL       PGM(&SRCLIB/PGRLC025) PARM(&MOD)
             CALL       PGM(&SRCLIB/PINTC025) PARM(&MOD)
             CALL       PGM(&SRCLIB/PPRDC025) PARM(&MOD)
             CALL       PGM(&SRCLIB/PSECC025) PARM(&MOD)
             /* CALL       PGM(&SRCLIB/PENTC025) PARM(&MOD)                  */
             CALL       PGM(&SRCLIB/PTBLC025) PARM(&MOD)
             ENDPGM
