             PGM        PARM(&LIB &QRY &GRUPO &SEQ)
             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
             DCL        VAR(&QRY) TYPE(*CHAR) LEN(10)
             DCL        VAR(&GRUPO) TYPE(*CHAR) LEN(3)
             DCL        VAR(&SEQ) TYPE(*CHAR) LEN(3)
             DCL        VAR(&NOMC) TYPE(*CHAR) LEN(10)
             DCL        VAR(&NOMF) TYPE(*CHAR) LEN(10)

             /*============================================================*/
             /* CRIAR O OBJECTO                                            */
             /*============================================================*/
             CHGVAR     VAR(&NOMC) VALUE('Q' *CAT &GRUPO *CAT 'S' +
                          *CAT &SEQ)
             RTVQMQRY   QMQRY(&LIB/&QRY) SRCFILE(#JMPRDSRC/QQRYSRC) +
                          SRCMBR(&NOMC) ALWQRYDFN(*YES)
             CRTQMQRY   QMQRY(#JMPRDOBJ/&NOMC) +
                          SRCFILE(#JMPRDSRC/QQRYSRC) SRCMBR(&NOMC)

             /*============================================================*/
             /* CRIAR A FRAME                                              */
             /*============================================================*/
             CHGVAR     VAR(&NOMF) VALUE('Q' *CAT &GRUPO *CAT 'R' +
                          *CAT &SEQ)
             RTVQMFORM  QMFORM(&LIB/&QRY) +
                          SRCFILE(#JMPRDSRC/QQRYSRC) SRCMBR(&NOMF) +
                          ALWQRYDFN(*YES)
             CRTQMFORM  QMFORM(#JMPRDOBJ/&NOMF) +
                          SRCFILE(#JMPRDSRC/QQRYSRC) SRCMBR(&NOMF)

             /*============================================================*/
             /* EXECUTAR                                                   */
             /*============================================================*/
             STRQMQRY   QMQRY(#JMPRDOBJ/&NOMC) OUTPUT(*PRINT) +
                          QMFORM(#JMPRDOBJ/&NOMF)
             ENDPGM
