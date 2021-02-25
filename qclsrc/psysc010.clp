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

             CHGLIBL    LIBL(QTEMP QGPL &OBJLIB &DBSLIB &SRCLIB)
             /****************************************************************/
             /* VALIDAR SE EXISTE O DIRECTORIO                               */
             /****************************************************************/
             RTVOBJD    OBJ(&OBJLIB/JMDIR) OBJTYPE(*BNDDIR)
             MONMSG     MSGID(CPF9812 CPF9801) EXEC(DO)
             CRTBNDDIR  BNDDIR(&OBJLIB/JMDIR)
             ENDDO

             /****************************************************************/
             /* GERAR PROGRAMAS DE SERVIÇO                                   */
             /****************************************************************/
             CALL       PGM(&SRCLIB/PGRLC020) PARM(&MOD)
             CALL       PGM(&SRCLIB/PCATC020) PARM(&MOD)
             CALL       PGM(&SRCLIB/PDBSC020) PARM(&MOD)
             CALL       PGM(&SRCLIB/PPRDC020) PARM(&MOD)
             CALL       PGM(&SRCLIB/PENTC020) PARM(&MOD)
             CALL       PGM(&SRCLIB/PSECC020) PARM(&MOD)
             CALL       PGM(&SRCLIB/PTBLC020) PARM(&MOD)
             ENDPGM
