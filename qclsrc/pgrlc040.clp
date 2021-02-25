             PGM
             DLYJOB     DLY(60) /* Aguardar 1 Minuto */
             STRCMTCTL  LCKLVL(*ALL) CMTSCOPE(*JOB) +
                          DFTJRN(#JMPRDDBS/JMDBS) OMTJRNE(*LUWID)
             DLYJOB     DLY(60) /* Aguardar 1 Minuto */
             CALL       PGM(#JMPRDOBJ/PGRLO001) /* Actualziar Inventário de Temporários */
             DLYJOB     DLY(60) /* Aguardar 1 Minuto */
             CALL       PGM(#JMPRDOBJ/PGRLO002) /* Extrair Metadados                    */
             DLYJOB     DLY(60) /* Aguardar 1 Minuto */
             ENDPGM
