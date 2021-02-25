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
             DCL  VAR(&ICEXIST) TYPE(*CHAR) LEN(1)

             CHGVAR VAR(&DBSLIB) VALUE('#JM' *CAT &AMB *CAT 'DBS')
             CHGVAR VAR(&SRCLIB) VALUE('#JM' *CAT &AMB *CAT 'SRC')

             /****************************************************************/
             /* VALIDAR SE O FICHEIRO EXISTE                                 */
             /****************************************************************/
             CHGVAR     VAR(&ICEXIST) VALUE('S')
             RTVOBJD    OBJ(&DBSLIB/&FILNAME) OBJTYPE(*FILE) +
                          OBJATR(&OBJATR)
             MONMSG     MSGID(CPF9812) EXEC(CHGVAR VAR(&ICEXIST) +
                          VALUE('N'))
             IF         COND(&ICEXIST = 'S') THEN(DO)
             ENDJRNPF   FILE(&DBSLIB/&FILNAME) JRN(&DBSLIB/JMDBS)
             MONMSG     MSGID(CPF9801)
             CHGPF      FILE(&DBSLIB/&FILNAME) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSOURCE +
                          *NOLIST) SIZE(*NOMAX)
             ENDDO
             ELSE       CMD(DO)
             CRTPF      FILE(&DBSLIB/&FILNAME) +
                          SRCFILE(&SRCLIB/QDDSSRC) OPTION(*NOSRC +
                          *NOLIST) SIZE(*NOMAX)
             ENDDO

             /*===================================================*/
             /* VALIDIAR A EXISTÊNCIA DO DIÁRIO                   */
             /*===================================================*/
             CHGVAR     VAR(&ICEXIST) VALUE('S')
             RTVOBJD    OBJ(&DBSLIB/JMDBS) OBJTYPE(*JRN) +
                          OBJATR(&OBJATR)
             MONMSG     MSGID(CPF9801) EXEC(CHGVAR VAR(&ICEXIST) +
                          VALUE('N'))
             IF         COND(&ICEXIST = 'N') THEN(DO)
             CRTJRNRCV  JRNRCV(&DBSLIB/JMDBS)
             CRTJRN     JRN(&DBSLIB/JMDBS) JRNRCV(&DBSLIB/JMDBS) +
                          DLTRCV(*YES)
             ENDDO
             STRJRNPF   FILE(&DBSLIB/&FILNAME) JRN(&DBSLIB/JMDBS) +
                          IMAGES(*BOTH) OMTJRNE(*OPNCLO)
             ENDPGM
