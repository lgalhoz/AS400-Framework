     H NOMAIN
     H*==================================================================
     H* Fun��o....: RGRLO029
     H* Descri��o.: Fun��es Matem�tica, calculos
     H*
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010

     P*=======================================================
     P* Processo..: $IsMultiplo
     P* Descri��o.: Validar se um n�mero � o n�o Multiplo
     P*=======================================================
     P $IsMultiplo     B                   EXPORT
     D                 PI              N
     D    Numero                      5S 0 Const
     D    Divisor                     5S 0 Const
     D
     D*==> Campos de Trabalho
     D WNumero         S              5S 0
     D WResult         S               N
     D*===========================================================
      /FREE
         WNumero = Numero;
         DOW (WNumero > *ZEROS);
            WNumero -= Divisor;
         ENDDO;
         IF (WNumero = *ZEROS);
            RETURN *ON;
         ELSE;
            RETURN *OFF;
         ENDIF;
      /END-FREE
     P $IsMultiplo     E
