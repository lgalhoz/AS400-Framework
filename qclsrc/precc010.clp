             /**************************************************************************/
             /* Programa...: PRECC010                                                  */
             /* Descrição..: Actualizar temporário de Recibos Jose Mata                */
             /* Observação.: Actualizar o temporário com base na informação existente  */
             /*              no ficheiros de recibos SD.1.RCA e SD.1.RCM               */
             /**************************************************************************/
             PGM
             /*-----------------------------------------------------------------------*/
             /* Procede à Criação do ficheiro temporário de Recibos.                  */
             /*-----------------------------------------------------------------------*/
             MONMSG     MSGID(CPF7302)
             CRTPF      FILE(#JMPRDDBS/FRECT010) +
                          SRCFILE(#JMPRDSRC/QDDSSRC) +
                          SRCMBR(FRECT010) SIZE(*NOMAX)
             CPYF       FROMFILE(QS36F/SD.1.RCA) +
                          TOFILE(#JMPRDDBS/FRECT010) +
                          MBROPT(*REPLACE) FMTOPT(*NOCHK)
             CPYF       FROMFILE(QS36F/SD.1.RCM) +
                          TOFILE(#JMPRDDBS/FRECT010) +
                          MBROPT(*ADD)     FMTOPT(*NOCHK)

             /*-----------------------------------------------------------------------*/
             /* Transformar as datas de DDMMYY para YYYYMMDD                          */
             /*-----------------------------------------------------------------------*/
             ENDPGM
