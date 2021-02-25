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
             /* CRIAR FICHEIRO TEMPORÁRIO                                       */
             /*=================================================================*/
             CRTPF   FILE(QTEMP/CSVCONT) RCDLEN(200) SIZE(*NOMAX)
             MONMSG  MSGID(CPF5813 CPF7302)
             CLRPFM     FILE(QTEMP/CSVCONT)
             CHGVAR VAR(&LNNAME) VALUE('/INTERFACES/EXTRACTOS/EXTRACTO_2010.CSV')
             CPYFRMSTMF FROMSTMF(&LNNAME) +
                          TOMBR('/QSYS.LIB/QTEMP.LIB/CSVCONT.FILE/CSVCONT.MBR') MBROPT(*REPLACE)
             CHGVAR VAR(&LNNAME) VALUE('/INTERFACES/EXTRACTOS/EXTRACTO_2011.CSV')
             CPYFRMSTMF FROMSTMF(&LNNAME) +
                          TOMBR('/QSYS.LIB/QTEMP.LIB/CSVCONT.FILE/CSVCONT.MBR') MBROPT(*ADD)
             CHGVAR VAR(&LNNAME) VALUE('/INTERFACES/EXTRACTOS/EXTRACTO_2012.CSV')
             CPYFRMSTMF FROMSTMF(&LNNAME) +
                          TOMBR('/QSYS.LIB/QTEMP.LIB/CSVCONT.FILE/CSVCONT.MBR') MBROPT(*ADD)
             CHGVAR VAR(&LNNAME) VALUE('/INTERFACES/EXTRACTOS/EXTRACTO_2013.CSV')
             CPYFRMSTMF FROMSTMF(&LNNAME) +
                          TOMBR('/QSYS.LIB/QTEMP.LIB/CSVCONT.FILE/CSVCONT.MBR') MBROPT(*ADD)

             ENDPGM
