/***********************************************************************************/
/* PROGRAMA..: PAUDC010                                                            */
/*---------------------------------------------------------------------------------*/
/* DESCRIÇÃO.: Programa executado a pedido, com o objectivo de identificar         */
/*             diferenças entre o que foi enviado pela José Mata e o que foi       */
/*             integrado na Lusitânia.                                             */
/*                                                                                 */
/* EXECUÇÃO..: Deverá converter o ficheiro de excel recebido da Lusitânia, para    */
/*             o formato CSV, e gravar para a pasta '/interfaces/Extractos'.       */
/*             O nome do ficheiro deverá ser: 'Extracto_<ano>.csv', com o ano      */
/*             com quatro digitos, por exemplo 'Extracto_2013.csv'.                */
/*                                                                                 */
/*             Será gerado também o histórico de ficheiros de interfaces enviados  */
/*             para determinar que informação foi enviada e quando.                */
/*                                                                                 */
/***********************************************************************************/
             PGM        PARM(&ANO &INZ)
             DCL        VAR(&ANO) TYPE(*CHAR) LEN(4)
             DCL        VAR(&INZ) TYPE(*CHAR) LEN(1)
             DCL        VAR(&LNNAME) TYPE(*CHAR) LEN(50)

             CHGVAR     VAR(&LNNAME) +
                          VALUE('/INTERFACES/EXTRACTOS/EXTRACTO_' +
                          *CAT &ANO *CAT '.CSV')


             CRTPF   FILE(QTEMP/CSVCONT) RCDLEN(200) SIZE(*NOMAX)
             MONMSG  MSGID(CPF5813 CPF7302)
             /*=================================================================*/
             /* CRIAR FICHEIRO TEMPORÁRIO                                       */
             /*=================================================================*/
             IF COND(&INZ = 'S') THEN(DO)
                DLTF    FILE(QTEMP/FAUDP020)
                MONMSG  MSGID(CPF2105)
                CRTPF   FILE(QTEMP/FAUDP020) +
                          SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
                DLTSPLF FILE(FAUDP020)

                DLTF    FILE(QTEMP/FAUDP021)
                MONMSG  MSGID(CPF2105)
                CRTPF   FILE(QTEMP/FAUDP021) +
                          SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
                DLTSPLF FILE(FAUDP021)

                DLTF    FILE(QTEMP/FAUDP022)
                MONMSG  MSGID(CPF2105)
                CRTPF   FILE(QTEMP/FAUDP022) +
                          SRCFILE(#JMPRDSRC/QDDSSRC) +
                          SRCMBR(FAUDP022) SIZE(*NOMAX)
                DLTSPLF FILE(FAUDP022)

                DLTF    FILE(QTEMP/FINTT010)
                MONMSG  MSGID(CPF2105)
                CRTPF   FILE(QTEMP/FINTT010) +
                          SRCFILE(#JMPRDSRC/QDDSSRC) +
                          SRCMBR(FINTT010) SIZE(*NOMAX)
                DLTSPLF FILE(FINTT010)

                CPYF FROMFILE(QS36F/LFREC) TOFILE(QTEMP/FINTT010) +
                          MBROPT(*REPLACE) FMTOPT(*NOCHK)
             ENDDO
             CPYFRMSTMF FROMSTMF(&LNNAME) +
                          TOMBR('/QSYS.LIB/QTEMP.LIB/CSVCONT.FILE/CSVC+
                          ONT.MBR') MBROPT(*REPLACE)


             IF COND(&INZ = 'S') THEN(DO)
                CPYFRMIMPF FROMFILE(QTEMP/CSVCONT) +
                             TOFILE(QTEMP/FAUDP020) MBROPT(*REPLACE) +
                             RCDDLM(*LF) FLDDLM(';') DECPNT(*COMMA) +
                             FROMRCD(2) RPLNULLVAL(*FLDDFT)
             ENDDO
             ELSE CMD(DO)
                CPYFRMIMPF FROMFILE(QTEMP/CSVCONT) +
                             TOFILE(QTEMP/FAUDP020) MBROPT(*ADD) +
                             RCDDLM(*LF) FLDDLM(';') DECPNT(*COMMA) +
                             FROMRCD(2) RPLNULLVAL(*FLDDFT)
             ENDDO

             DLTF       FILE(QTEMP/CSVCONT)
             ENDPGM
