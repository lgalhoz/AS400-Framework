/***********************************************************************************/
/* PROGRAMA..: PAUDC010                                                            */
/*---------------------------------------------------------------------------------*/
/* DESCRI��O.: Programa executado a pedido, com o objectivo de identificar         */
/*             diferen�as entre o que foi enviado pela Jos� Mata e o que foi       */
/*             integrado na Lusit�nia.                                             */
/*                                                                                 */
/* EXECU��O..: Dever� converter o ficheiro de excel recebido da Lusit�nia, para    */
/*             o formato CSV, e gravar para a pasta '/interfaces/Extractos'.       */
/*             O nome do ficheiro dever� ser: 'Extracto_<ano>.csv', com o ano      */
/*             com quatro digitos, por exemplo 'Extracto_2013.csv'.                */
/***********************************************************************************/
             PGM        PARM(&ANO &INZ)
             DCL        VAR(&ANO) TYPE(*CHAR) LEN(4)
             DCL        VAR(&INZ) TYPE(*CHAR) LEN(1)
             DCL        VAR(&LNNAME) TYPE(*CHAR) LEN(50)

             CHGVAR     VAR(&LNNAME) +
                          VALUE('/INTERFACES/EXTRACTOS/EXTRACTO_' +
                          *CAT &ANO *CAT '.CSV')


             /*=================================================================*/
             /* CRIAR FICHEIRO TEMPOR�RIO                                       */
             /*=================================================================*/
             IF COND(&INZ = 'S') THEN(DO)
                CRTPF   FILE(QTEMP/CSVCONT) RCDLEN(200) SIZE(*NOMAX)
                MONMSG  MSGID(CPF5813 CPF7302)

                DLTF    FILE(QTEMP/FINTT030)
                MONMSG  MSGID(CPF2105)
                CRTPF   FILE(QTEMP/FINTT030) +
                          SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
                DLTSPLF FILE(FINTT030)

                DLTF    FILE(QTEMP/FINTT031)
                MONMSG  MSGID(CPF2105)
                CRTPF   FILE(QTEMP/FINTT031) +
                          SRCFILE(#JMPRDSRC/QDDSSRC) +
                          SRCMBR(FINTT031) SIZE(*NOMAX)
                DLTSPLF FILE(FINTT031)
             ENDDO
             CPYFRMSTMF FROMSTMF(&LNNAME) +
                          TOMBR('/QSYS.LIB/QTEMP.LIB/CSVCONT.FILE/CSVC+
                          ONT.MBR') MBROPT(*REPLACE)


             IF COND(&INZ = 'S') THEN(DO)
                CPYFRMIMPF FROMFILE(QTEMP/CSVCONT) +
                             TOFILE(QTEMP/FINTT030) MBROPT(*REPLACE) +
                             RCDDLM(*LF) FLDDLM(';') DECPNT(*COMMA) +
                             FROMRCD(2) RPLNULLVAL(*FLDDFT)
             ELSE CMD(DO)
                CPYFRMIMPF FROMFILE(QTEMP/CSVCONT) +
                             TOFILE(QTEMP/FINTT030) MBROPT(*ADD) +
                             RCDDLM(*LF) FLDDLM(';') DECPNT(*COMMA) +
                             FROMRCD(2) RPLNULLVAL(*FLDDFT)
             ENDDO

             DLTF       FILE(QTEMP/CSVCONT)
             CALL       PGM(#JMPRDOBJ/PAUDO010)
             ENDPGM
