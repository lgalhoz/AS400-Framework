     A**********************************************************
     A* Utilização de P-Fields em Ecrãs
     A**********************************************************
     A*           CAMPO         10A  O      DSPATR(&ATRIB)
     A*           ATRIB          1A  P
     A**********************************************************
     D
     D FLD001          S              1A
     D
     D*==> Atributo de Cores
     D Green           C                   Const(x'20')
     D Pink            C                   Const(x'38')
     D Red             C                   Const(x'28')
     D Turquoise       C                   Const(x'30')
     D White           C                   Const(x'22')
     D Yellow          C                   Const(x'32')
     D
     D*==> Atributos de Modo (usar com %BitOr(<campo>:valor))
     D Protect         C                   Const(x'80')
     D Reverse         C                   Const(x'01')
     D Undeline        C                   Const(x'04')
     D Blink           C                   Const(x'2A')
     D NonDisplay      C                   Const(x'27')
      /FREE

             FLD001 = Red;
             FLD001 = %BitOr(Fld001:Protect);
             *INLR = *ON;
             RETURN;
      /END-FREE
