/***********************************************************************************/
/* PROGRAMA..: PAUDC008                                                            */
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
             PGM
             DCL        VAR(&ANO) TYPE(*CHAR) LEN(4)
             DCL        VAR(&INZ) TYPE(*CHAR) LEN(1)
             DCL        VAR(&LNNAME) TYPE(*CHAR) LEN(50)

             /*=================================================================*/
             /* COPIAR FICHEIROS DE EXTRACTO                                    */
             /*=================================================================*/
             CALL    PGM(#JMPRDSRC/PAUDC009)

             /*=================================================================*/
             /* CRIAR FICHEIRO TEMPORÁRIO                                       */
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
