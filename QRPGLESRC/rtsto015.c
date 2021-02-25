//----------------------------------------------------------------//
// Compilar com OPTION(*STDINC)                                   //
//----------------------------------------------------------------//
//    CRTBNDC PGM(SOAPSERV/RSECO015) SRCFILE(SOAPSERV/QRPGLESRC)  //
//      SRCMBR(RSECO015) OPTION(*STDINC) REPLACE(*YES)            //
//----------------------------------------------------------------//

//========================================================
// Função....: strToUpper
// Objectivo.: Colocar em Maiusuclas uma String
//========================================================
char* strToUpper(char * s) {
  // int i;
  // for (i = 0; i < 100; i++) {
  //    if (s[i] != '\0') {
  //       s[i] = strtoupper(s[i]);
  //    } else {
  //      return s;
  //    }
  // }
  return s;
}

//========================================================
// Função....: strChange
// Objectivo.: Recebe uma String e Tranforma
//========================================================
char* strChange(char * s) {
   char result[10] = "Amarelo456";
   int i;

   for (i = 0; i < 10; i++) {
      if (s[i] != '\0') {
         result[i] = s[i];
      }
   }

   return result;
}

//========================================================
// Função....: addInt
// Objectivo.: Recebe um interio e Transoforma
//========================================================
int addInt(int i) {
   int i2;
   i2 = 20+i;
   return i2;
}

//========================================================
// Função....: random
// Objectivo.: Usar funções Standard do C (Random)
//========================================================
int random() {
  int     rand_num;
  rand_num = rand() %100 + 1;
  return(rand_num);
}

//========================================================
// Função....: main
// Objectivo.: Estrutura de uma função main
//========================================================
int main(int argc, char* argv[]){
  int i;
  char* c;
  i = 4;
  for(i=0;i < argc;i++) {
      printf("Valor = %c", argv[i]);
      printf("Parametro  = %d\n", i);
  }
  return 66;
}
