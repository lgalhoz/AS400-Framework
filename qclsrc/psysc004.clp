/*******************************************************************/
/* Programa..: PCATC004                                            */
/* Objectivo.: Criar as Tabelas.                                   */
/*******************************************************************/
/* Observa��o: � testada a exist�ncia das tabelas na biblioteca de */
/*             objectos de destino, e se existir ent�o altera.     */
/*******************************************************************/
             PGM
             DCL    VAR(&SRCLIB) TYPE(*CHAR) LEN(10)
             DCL    VAR(&DBSLIB) TYPE(*CHAR) LEN(10)
             DCL    VAR(&OBJLIB) TYPE(*CHAR) LEN(10)

             CHGVAR VAR(&OBJLIB) VALUE('#JMPRDDBS')
             CHGVAR VAR(&DBSLIB) VALUE('#JMPRDOBJ')
             CHGVAR VAR(&SRCLIB) VALUE('#JMPRDSRC')

             CHGLIBL    LIBL(QTEMP QGPL SEXTRAS QS36F LIB.GSIN +
                          SOURCE &DBSLIB &OBJLIB)

             ENDPGM
