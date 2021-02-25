   PGM
   DCL VAR(&MES) TYPE(*DEC) LEN(2 0)
   /*===============================================================*/
   /* CRIAR FICHEIRO COM O LAYOUT DE RECIBOS                        */
   /*===============================================================*/
   DLTF FILE(QTEMP/FINTT010)
   MONMSG MSGID(CPF0000)
   CRTPF FILE(QTEMP/FINTT010) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)

   /*===============================================================*/
   /* CARREGAR FICHEIROS                                            */
   /*===============================================================*/
   INICIO:     DO
   ENDDO
   ENDPGM
