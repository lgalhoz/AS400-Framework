/*******************************************************************/
/* Programa..: PGRLC001                                            */
/* Objectivo.: Criar/Alterar um ficheiro físico                    */
/*******************************************************************/
/* Observação: É testada a existência das tabelas na biblioteca de */
/*             objectos de destino, e se existir então altera.     */
/*******************************************************************/
             PGM  PARM(&AMB &FILNAME)
             DCL  VAR(&AMB) TYPE(*CHAR) LEN(3)
             DCL  VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
             DCL  VAR(&SRCLIB) TYPE(*CHAR) LEN(10)
             DCL  VAR(&OBJATR) TYPE(*CHAR) LEN(10)
             DCL  VAR(&FILNAME) TYPE(*CHAR) LEN(10)
             DCL  VAR(&FISICO) TYPE(*CHAR) LEN(10)
             DCL  VAR(&ICEXIST) TYPE(*CHAR) LEN(1)

             CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &AMB *CAT 'DBS')
             CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &AMB *CAT 'SRC')
             CHGVAR VAR(&FISICO) VALUE(%SST(&FILNAME 1 8))

             /****************************************************************/
             /* VALIDAR SE O FICHEIRO EXISTE                                 */
             /****************************************************************/
             CHGVAR     VAR(&ICEXIST) VALUE('S')
             RTVOBJD    OBJ(&DBSLIB/&FILNAME) OBJTYPE(*FILE) +
                          OBJATR(&OBJATR)
             MONMSG     MSGID(CPF9812) EXEC(CHGVAR VAR(&ICEXIST) +
                          VALUE('N'))
             IF         COND(&ICEXIST = 'S') THEN(DO)
             DLTF       FILE(&DBSLIB/&FILNAME)
             ENDDO
             CRTLF      FILE(&DBSLIB/&FILNAME) +
                          SRCFILE(&SRCLIB/QDDSSRC) SRCMBR(&FILNAME) +
                          DTAMBRS((&DBSLIB/&FISICO (&FISICO))) +
                          OPTION(*NOSOURCE *NOLIST)
             ENDPGM
