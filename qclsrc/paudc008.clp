/***********************************************************************************/
/* PROGRAMA..: PAUDC008                                                            */
/*---------------------------------------------------------------------------------*/
/* DESCRI��O.: Programa executado a pedido, com o objectivo de identificar         */
/*             diferen�as entre o que foi enviado pela Jos� Mata e o que foi       */
/*             integrado na Lusit�nia.                                             */
/*                                                                                 */
/* EXECU��O..: Dever� converter o ficheiro de excel recebido da Lusit�nia, para    */
/*             o formato CSV, e gravar para a pasta '/interfaces/Extractos'.       */
/*             O nome do ficheiro dever� ser: 'Extracto_<ano>.csv', com o ano      */
/*             com quatro digitos, por exemplo 'Extracto_2013.csv'.                */
/*                                                                                 */
/*             Ser� gerado tamb�m o hist�rico de ficheiros de interfaces enviados  */
/*             para determinar que informa��o foi enviada e quando.                */
/*                                                                                 */
/***********************************************************************************/
             PGM
             DCL        VAR(&ANO) TYPE(*CHAR) LEN(4)
             DCL        VAR(&INZ) TYPE(*CHAR) LEN(1)
             DCL        VAR(&LNNAME) TYPE(*CHAR) LEN(50)

             /*=================================================================*/
             /* COPIAR FICHEIROS DE EXTRACTO                                    */
             /*=================================================================*/
             CALL    PGM(#JMPRDSRC/PAUDC009)

             /*=================================================================*/
             /* CRIAR FICHEIRO TEMPOR�RIO                                       */
             /*=================================================================*/
             DLTF    FILE(QTEMP/FAUDP020)
             MONMSG  MSGID(CPF2105)
             CRTPF   FILE(QTEMP/FAUDP020) +
                       SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
             DLTSPLF FILE(FAUDP020)

             DLTF    FILE(QTEMP/FAUDP023)
             MONMSG  MSGID(CPF2105)
             CRTPF   FILE(QTEMP/FAUDP023) +
                       SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
             DLTSPLF FILE(FAUDP023)

             DLTF    FILE(QTEMP/FAUDP022)
             MONMSG  MSGID(CPF2105)
             CRTPF   FILE(QTEMP/FAUDP022) +
                       SRCFILE(#JMPRDSRC/QDDSSRC) +
                       SRCMBR(FAUDP022) SIZE(*NOMAX)
             DLTSPLF FILE(FAUDP022)

             /*=================================================================*/
             /* COPIAR INTERFACE DE RECIBOS ENVIADOS                            */
             /*=================================================================*/
             DLTF    FILE(QTEMP/FINTT010)
             MONMSG  MSGID(CPF2105)
             CRTPF   FILE(QTEMP/FINTT010) +
                       SRCFILE(#JMPRDSRC/QDDSSRC) +
                       SRCMBR(FINTT010) SIZE(*NOMAX)
             DLTSPLF FILE(FINTT010)
             CPYF FROMFILE(QS36F/LFREC) TOFILE(QTEMP/FINTT010) MBROPT(*REPLACE) FMTOPT(*NOCHK)

             /*=================================================================*/
             /* ACTUALIZAR FAUDP020                                             */
             /*=================================================================*/
             CALL       PGM(#JMPRDSRC/PAUDO009)
             ENDPGM
