     H DFTACTGRP(*NO) BNDDIR('QC2LE')
     H*NOMAIN
     H*==================================================================
     H* Função....: RGRLO028
     H* Descrição.: Validar o nome de uma String
     H*             Este processo corresponde a uma adaptação do programa
     H*             original de Scott Klement, July 13, 2006, criado para
     H*             validar endereços de email.
     H*             O Programa Original tem a denominação de MAILCHK.
     H*==================================================================
      /COPY QRPGLESRC,SGRLO022

     D*==> Validar se a String é válida
     D $ValStrStruct   PR              N
     D    StrToValid                 10A   Const
     D    Pattern                    50A   Const varying
     D
     D*==> Campos de Trabalho
     D WFileName       S             10A
     D WTemplate       S             50A
     D*=======================================================
      /FREE

        WFileName = 'AGRA1503';
        WTemplate = '^(AGRA)([0-9]{2})(0[1-9]{1}|1[0-2]{1})$';

        IF ($ValStrStruct(WFileName:WTemplate));
           DSPLY 'Sucesso';
        ELSE;
           DSPLY 'Erro';
        ENDIF;

        RETURN;
      /END-FREE

     P*=======================================================
     P* Processo..: $ValStrStruct
     P* Descrição.: Validar se a estrutura da String é
     P*             válida.
     P*=======================================================
     P $ValStrStruct   B                   EXPORT
     D                 PI              N
     D    StrToValid                 10A   Const
     D    Pattern                    50A   Const varying
     D
     D*==> Campos de Trabalho
     D WIsValido       S               N
     D WMsgErro        S            512A   varying
     D WReg            DS                  likeds(regex_t)
     D match           DS                  likeds(regmatch_t)
     D rc              S             10I 0
     D Buf             S            512A
     D*======================================================
      /FREE
        WMsgErro  = *BLANKS;
        WIsValido = *OFF;

        //--------------------------------------------------
        //  Testar a Pattern, para ver se é válida.
        //--------------------------------------------------
        rc = regcomp( WReg
                     : %TRIM(Pattern)
                     : REG_EXTENDED + REG_ICASE + REG_NOSUB );
        if (rc <> *ZEROS);
           regerror(rc: WReg: %addr(Buf): %size(buf));
           WMsgErro  = %str(%addr(buf));
           RETURN WIsValido;
        endif;


        //--------------------------------------------------
        //  Testar a Palavra
        //--------------------------------------------------
        if (regexec( WReg
                   : %trim(StrToValid)
                   : 0
                   : match
                   : 0 ) = *ZEROS);
           WIsValido = *on;
        ENDIF;

        return WIsValido;
      /END-FREE
     P $ValStrStruct   E
