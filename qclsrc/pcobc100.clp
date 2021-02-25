             PGM
             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
             RTVJOBA    USER(&USER)

             IF         COND((&USER *EQ 'TESTE') *OR (&USER *EQ +
                          'LGALHOZ2')) THEN(DO)
                STRCMTCTL  LCKLVL(*ALL) CMTSCOPE(*JOB) +
                             DFTJRN(#JMPRDDBS/JMDBS)
             ENDDO
             /*===================================================================*/
             /* PREPARAR FICHEIROS DO BASE APÓLICES                               */
             /*===================================================================*/
             CPYF       FROMFILE(QS36F/GBASEGRO) +
                          TOFILE(QTEMP/GBASEGRO) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             CPYF       FROMFILE(QS36F/GBASEDIO) +
                          TOFILE(QTEMP/GBASEDIO) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             CPYF       FROMFILE(QS36F/GBASEGRA) +
                          TOFILE(QTEMP/GBASEGRA) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             CPYF       FROMFILE(QS36F/GBASEDIA) +
                          TOFILE(QTEMP/GBASEDIA) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             CPYF       FROMFILE(QS36F/GREGAPOL) +
                          TOFILE(QTEMP/GREGAPOL) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             CPYF       FROMFILE(QS36F/FALTER) +
                          TOFILE(QTEMP/FALTER) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             CPYF       FROMFILE(QS36F/GREGAPO2) +
                          TOFILE(QTEMP/GREGAPO2) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             CPYF       FROMFILE(#JMPRDDBS/FPRDT001) +
                          TOFILE(QTEMP/FPRDT001) MBROPT(*REPLACE) +
                          CRTFILE(*YES)

             /*===================================================================*/
             /* PREPARAR A NOVA ESTRUTURA DE CONTROLO                             */
             /*===================================================================*/
             CPYF       FROMFILE(#JMPRDDBS/FCOBP220) +
                          TOFILE(QTEMP/FCOBP220) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)
             STRJRNPF   FILE(QTEMP/FCOBP220) JRN(#JMPRDDBS/JMDBS) OMTJRNE(*OPNCLO)
             CPYF       FROMFILE(#JMPRDDBS/FCOBP221) +
                          TOFILE(QTEMP/FCOBP221) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)
             STRJRNPF   FILE(QTEMP/FCOBP221) JRN(#JMPRDDBS/JMDBS) OMTJRNE(*OPNCLO)
             CPYF       FROMFILE(#JMPRDDBS/FCOBP222) +
                          TOFILE(QTEMP/FCOBP222) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)
             STRJRNPF   FILE(QTEMP/FCOBP222) JRN(#JMPRDDBS/JMDBS) OMTJRNE(*OPNCLO)
             CRTLF      FILE(QTEMP/FCOBP22001) SRCFILE(#JMPRDSRC/QDDSSRC) OPTION(*NOLIST)
             MONMSG     MSGID(CPF7302 CPF5813)
             CRTLF      FILE(QTEMP/FCOBP22002) SRCFILE(#JMPRDSRC/QDDSSRC) OPTION(*NOLIST)
             MONMSG     MSGID(CPF7302 CPF5813)

             /*===================================================================*/
             /* ANTIGA ESTRUTURA DE DADOS DOS DEBITOS DIRECTOS                    */
             /*===================================================================*/
             CPYF       FROMFILE(QS36F/TBADCAF) +
                          TOFILE(QTEMP/TBADCAF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBADCPF) +
                          TOFILE(QTEMP/TBADCPF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CRTLF      FILE(QTEMP/TBADCPL1) SRCFILE(SOURCE/QDDSSRC) +
                          SRCMBR(TBADCPL1) DTAMBRS((QTEMP/TBADCPF +
                          (TBADCPF))) OPTION(*NOLIST)
             MONMSG     MSGID(CPF7302 CPF5813)

             CRTLF      FILE(QTEMP/TBIDDPL3) SRCFILE(SOURCE/QDDSSRC) +
                          SRCMBR(TBIDDPL3) OPTION(*NOLIST)
             MONMSG     MSGID(CPF7302 CPF5813)

             CPYF       FROMFILE(QS36F/TBADCEF) +
                          TOFILE(QTEMP/TBADCEF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBADCFF) +
                          TOFILE(QTEMP/TBADCFF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBADCIF) +
                          TOFILE(QTEMP/TBADCIF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBADCOF) +
                          TOFILE(QTEMP/TBADCOF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBADCRF) +
                          TOFILE(QTEMP/TBADCRF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBIDDEF) +
                          TOFILE(QTEMP/TBIDDEF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBIDDFF) +
                          TOFILE(QTEMP/TBIDDFF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBIDDGF) +
                          TOFILE(QTEMP/TBIDDGF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBIDDIF) +
                          TOFILE(QTEMP/TBIDDIF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBIDDOF) +
                          TOFILE(QTEMP/TBIDDOF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/TBIDDPF) +
                          TOFILE(QTEMP/TBIDDPF) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)
             STRJRNPF   FILE(QTEMP/TBADCPF) JRN(#JMPRDDBS/JMDBS) OMTJRNE(*OPNCLO)
             STRJRNPF   FILE(QTEMP/TBADCAF) JRN(#JMPRDDBS/JMDBS) OMTJRNE(*OPNCLO)

             /*===================================================================*/
             /* ALTERAR OS PROGRAMAS BASES NA QTEMP.                              */
             /*===================================================================*/
             CRTDSPF    FILE(QTEMP/GP010XFM) SRCFILE(SOURCE/QDDSSRC) +
                          SRCMBR(GP010XFM) OPTION(*NOLIST) +
                          REPLACE(*YES)
             CRTBNDRPG  PGM(QTEMP/GP010X) SRCFILE(SOURCE/QRPGLESRC) +
                          SRCMBR(GP010X) OUTPUT(*NONE) REPLACE(*YES)

             CRTDSPF    FILE(QTEMP/GP015XFM) SRCFILE(SOURCE/QDDSSRC) +
                          SRCMBR(GP015XFM) OPTION(*NOLIST) +
                          REPLACE(*YES)
             CRTBNDRPG  PGM(QTEMP/GP015X) SRCFILE(SOURCE/QRPGLESRC) +
                          SRCMBR(GP015X) OUTPUT(*NONE) REPLACE(*YES)

             IF         COND((&USER *EQ 'TESTE')) THEN(DO)
                CALL       PGM(#JMPRDOBJ/PPRDO010)
                SIGNOFF
             ENDDO
             IF         COND((&USER *EQ 'LGALHOZ2')) THEN(DO)
                CALL       PGM(#JMPRDOBJ/PGRLO010)
                SIGNOFF
             ENDDO
             ENDPGM
