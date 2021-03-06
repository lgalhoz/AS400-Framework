     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
      *=======================================================
      * Autor......: Luis Galhoz
      * Processo...: ActApolices
      * Objectivo..: Actualizar ficheiro f�sico com as ap�lices
      *=======================================================
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SGRLO010

     F*=====================================================================
      /FREE
         //----------------------------------------------
         // Eliminar tempor�rio
         //----------------------------------------------
         // $shellExec('CHGPF FILE(SOAPSERV/FPRDT001) SRCFILE(SOAPSERV+
         //            /QDDSSRC) SRCMBR(FPRDT001) SIZE(*NOMAX)');
         //
         EXEC SQL
           SET OPTION
              COMMIT=*NONE,
              DATFMT=*ISO;

         //------------------------------------------------------
         // Carregar Ap�lices Auto - Diario (considerar fecho)
         //------------------------------------------------------
         IF ($checkObject('GP010A':'QS36F':'*FILE'));
            EXEC SQL
               DELETE FROM #JMPRDDBS/FPRDT001 WHERE NUAPOL IN (SELECT
               APOL FROM SEXTRAS/JBASEDIA);
            EXEC SQL
               INSERT INTO #JMPRDDBS/FPRDT001 (CDRAMO, NUAPOL, NUSEG,
               NOSEG, NUNIF, APELIDO, STATUS, MATRIC)
               (SELECT RAMO, APOL, A.NSEG, NOM, NIF, APELID, SIT, MATR1
               FROM SEXTRAS/JBASEDIA A INNER
               JOIN QS36F/GFSEG B ON (A.NSEG = B.NSEG)
               LEFT JOIN #JMPRDDBS/FPRDT001 ON (RAMO=CDRAMO AND APOL=NUAPOL)
               WHERE CDRAMO IS NULL);
         ELSE;
            EXEC SQL
               DELETE FROM #JMPRDDBS/FPRDT001 WHERE NUAPOL IN (SELECT
               APOL FROM QS36F/GBASEDIA);
            EXEC SQL
               INSERT INTO #JMPRDDBS/FPRDT001 (CDRAMO, NUAPOL, NUSEG,
               NOSEG, NUNIF, APELIDO, STATUS, MATRIC)
               (SELECT RAMO, APOL, A.NSEG, NOM, NIF, APELID, SIT, MATR1
               FROM QS36F/GBASEDIA A INNER
               JOIN QS36F/GFSEG B ON (A.NSEG = B.NSEG)
               LEFT JOIN #JMPRDDBS/FPRDT001 ON (RAMO=CDRAMO AND APOL=NUAPOL)
               WHERE CDRAMO IS NULL);
         ENDIF;

         //------------------------------------------------------
         // Carregar Ap�lices Auto - Base
         //------------------------------------------------------
         EXEC SQL
            INSERT INTO #JMPRDDBS/FPRDT001 (CDRAMO, NUAPOL, NUSEG, NOSEG,
            NUNIF, APELIDO, STATUS, MATRIC)
            (SELECT RAMO, APOL, A.NSEG, NOM, NIF, APELID, SIT, MATR1
            FROM QS36F/GBASEGRA A INNER
            JOIN QS36F/GFSEG B ON (A.NSEG = B.NSEG)
            LEFT JOIN #JMPRDDBS/FPRDT001 ON (RAMO=CDRAMO AND APOL=NUAPOL)
            WHERE CDRAMO IS NULL);

         //------------------------------------------------------
         // Carregar Ap�lices Outros Ramos - Di�rio
         //------------------------------------------------------
         IF ($checkObject('GP015A':'QS36F':'*FILE'));
            EXEC SQL
               DELETE FROM #JMPRDDBS/FPRDT001 WHERE NUAPOL IN (SELECT
               APOL FROM SEXTRAS/JBASEDIO);
            EXEC SQL
               INSERT INTO #JMPRDDBS/FPRDT001 (CDRAMO, NUAPOL, NUSEG,
               NOSEG, NUNIF, APELIDO, STATUS, LCRISK)
               (SELECT RAMO, APOL, A.NSEG, NOM, NIF, APELID, SIT, LRPS
               FROM SEXTRAS/JBASEDIO A INNER
               JOIN QS36F/GFSEG B ON (A.NSEG = B.NSEG)
               LEFT JOIN #JMPRDDBS/FPRDT001 ON (RAMO=CDRAMO AND APOL=NUAPOL)
               WHERE CDRAMO IS NULL);
         ELSE;
            EXEC SQL
               DELETE FROM #JMPRDDBS/FPRDT001 WHERE NUAPOL IN (SELECT
               APOL FROM QS36F/GBASEDIO);
            EXEC SQL
               INSERT INTO #JMPRDDBS/FPRDT001 (CDRAMO, NUAPOL, NUSEG,
               NOSEG, NUNIF, APELIDO, STATUS, LCRISK)
               (SELECT RAMO, APOL, A.NSEG, NOM, NIF, APELID, SIT, LRPS
               FROM QS36F/GBASEDIO A INNER
               JOIN QS36F/GFSEG B ON (A.NSEG = B.NSEG)
               LEFT JOIN #JMPRDDBS/FPRDT001 ON (RAMO=CDRAMO AND APOL=NUAPOL)
               WHERE CDRAMO IS NULL);
         ENDIF;

         //------------------------------------------------------
         // Carregar Ap�lices Outros Ramos - Base
         //------------------------------------------------------
         EXEC SQL
            INSERT INTO #JMPRDBS/FPRDT001 (CDRAMO, NUAPOL, NUSEG, NOSEG,
            NUNIF, APELIDO, STATUS, LCRISK)
            (SELECT RAMO, APOL, A.NSEG, NOM, NIF, APELID, SIT, LRPS
            FROM QS36F/GBASEGRO A INNER
            JOIN QS36F/GFSEG B ON (A.NSEG = B.NSEG)
            LEFT JOIN #JMPRDDBS/FPRDT001 ON (RAMO=CDRAMO AND APOL=NUAPOL)
            WHERE CDRAMO IS NULL);

         //------------------------------------------------------
         // Carregar Ap�lices Tempor�rias (Outros Ramos)
         //------------------------------------------------------
         EXEC SQL
            INSERT INTO #JMPRDDBS/FPRDT001 (CDRAMO, NUAPOL, NUSEG, NOSEG,
            NUNIF, APELIDO, STATUS, LCRISK)
            (SELECT RAMO, APOL, A.NSEG, NOM, NIF, APELID, SIT, LRPS
            FROM QS36F/BASEGROT A INNER
            JOIN QS36F/GFSEG B ON (A.NSEG = B.NSEG)
            LEFT JOIN #JMPRDDBS/FPRDT001 ON (RAMO=CDRAMO AND APOL=NUAPOL)
            WHERE CDRAMO IS NULL);

         //------------------------------------------------------
         // Carregar Ap�lices Tempor�rias (Autom�vel)
         //------------------------------------------------------
         EXEC SQL
            INSERT INTO #JMPRDDBS/FPRDT001 (CDRAMO, NUAPOL, NUSEG, NOSEG,
            NUNIF, APELIDO, STATUS)
            (SELECT RAMO, APOL, A.NSEG, NOM, NIF, APELID, SIT
            FROM QS36F/BASEGRAT A INNER
            JOIN QS36F/GFSEG B ON (A.NSEG = B.NSEG)
            LEFT JOIN #JMPRDDBS/FPRDT001 ON (RAMO=CDRAMO AND APOL=NUAPOL)
            WHERE CDRAMO IS NULL);

        RETURN;
      /END-FREE
