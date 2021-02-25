     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: PTSTO012
     H* Descrição.: Testar a chamada ao programa de escrita de Apólices
     H*
     H*==================================================================
     FGBASEGRO  IF   E           K DISK    PREFIX(OR_)
     F
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO014
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SSECO010
     D
     D RetTexto        PR                  EXTPGM('RPRDO008')
     D   WGexap                        *   CONST
     D   WGBaseDio                     *   CONST
     D   #RetTexto                         CONST LIKE(#RETTEXTO) DIM(1000)
     D   CdGrupo                      5S 0 CONST
     D
     D*==> Estrutura de dados do Registo
     D RBASEGRO      E DS                  ExtName(GBASEGRO)
     D                                     Prefix(OR_)
     D*                                    Based(ptrDio)
     D RBASEDIA      E DS                  ExtName(GBASEDIA)
     D                                     Prefix(AU_)
     D                                     Based(ptrDia)
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(PRD_)
     D                                     Based(ptrHist)
     D
     D*==> Lista de Textos a escrever
     D #RetTexto       DS                  QUALIFIED DIM(1000)
     D   Texto                      100A
     D   IsEMpty                       N
     D WIdx            S              5S 0 INZ(*ZEROS)
     D WPtrDiario      S               *
     D WPtrHistorico   S               *
     D WDetail         S             60A
     D*WRamo           S              2S 0
     D*WApolice        S              7S 0
     D Cur_Line        S              5S 0
     D*=================================================================
     I*BASEGRFO  AA
     I*                                  1  700  RBASEGRO
     I*=================================================================
      /FREE

        WRamo = 5;
        WApolice = 9900006;
        CHAIN (WRamo:WApolice) BASEGRFO;
        IF (%FOUND());
           WPtrHistorico = %ADDR(RGEXAP);
           WPtrDiario    = %ADDR(RBASEGRO);
           RetTexto(WPtrHistorico:WPtrDiario:#RetTexto:2);

||         WIdx = 1;
           MONITOR;
||+--         DOW (NOT #RetTexto(WIdx).IsEmpty);
|||+-            IF  (CUR_LINE = 82);
||||                 WDetail = $AlignText('.../...':90:'R');
||||                 // EXCEPT LDETLIN05;
||||                 // EXCEPT BRKPAGE;
|||+-            ENDIF;
|||              WDetail = #RetTexto(WIdx).Texto;
|||              // EXCEPT LDETLIN05;
|||              WIdx += 1;
||+--         ENDDO;
           ON-ERROR;
           ENDMON;
        ENDIF;

        *INLR = *ON;
        RETURN;
      /END-FREE
