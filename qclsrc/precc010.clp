             /**************************************************************************/
             /* Programa...: PRECC010                                                  */
             /* Descri��o..: Actualizar tempor�rio de Recibos Jose Mata                */
             /* Observa��o.: Actualizar o tempor�rio com base na informa��o existente  */
             /*              no ficheiros de recibos SD.1.RCA e SD.1.RCM               */
             /**************************************************************************/
             PGM
             /*-----------------------------------------------------------------------*/
             /* Procede � Cria��o do ficheiro tempor�rio de Recibos.                  */
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
