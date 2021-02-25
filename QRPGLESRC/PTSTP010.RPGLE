     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP('JMGRP')
     H*==================================================================
     D*==> Obter a quantidade de ficheiros existentes na Biblioteca
     D getQtdeFiles    PR             5S 0
     D  WNomLib                      10A   Const
     D
     D*==> Obter o ficheiro a partir de uma dada posição
     D getFileInfo     PR                  Like(Object)
     D  WRecNbr                       5S 0 Const
     D
     D*==> Estrutura de dados para conter os dados do objecto
     D Object          DS
     D  Name                   1     10A
     D  Library               11     20A
     D  Type                  21     30A
     D  Status                31     31A
     D  Attrib                32     41A
     D  Description           42     91A
     D  UserAttrib            92    101A
     D  Reserved             102    108A
     D  AspNumber            109    112B 0
     D  Owner                113    122A
     D  Domain               123    124A
     D  DatCria              125    132A
     D  DatAltera            133    140A
     D
     D*==> Campos de Trabalho
     D WCount          S              5S 0
     D WNomLib         S             10A   INZ('QS36F')
     D*==================================================================
      /FREE
         WCount = *ZEROS;
         WCount = getQtdeFiles(%TRIM(WNomLib));

         IF (WCount > *ZEROS);
            Object = getFileInfo(4);
            EVAL Name = *BLANKS;
         ENDIF;

         *INLR = *ON;
      /END-FREE
