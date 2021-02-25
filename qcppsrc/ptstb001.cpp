//=====================================================
// Classe.....: CRectangle
// Descrição..: Classe com metodos declarados no Código
//=====================================================
#include <iostream.h>
#include <iomanip.h>
#include <fstream.h>
#include <stdio.h>
using namespace std;

//=====================================================
// Classe.....: CRectangle
// Descrição..: Classe com metodos declarados no Código
//=====================================================
class CRectangle {
  int x, y;

  public:
    int valor(int z) {
       return z*6;
    }

    void set_values (int,int);
    int area (void);
} rect;

//------------------------------------------------------
// Descrição..: Metodos da classe CRectangle declarados
//              ou redefinidos fora da Classe
//------------------------------------------------------
int CRectangle::area(void) {
   return 36;
}

void CRectangle::set_values (int a, int b) {
   x = a;
   y = b;
}


//=====================================================
// Descrição..: Execução da classe
//=====================================================
int main() {
   int x;

   CRectangle rectx;
   CRectangle recty;
   rectx.set_values (3,4);
   printf("Area %d metros e %d cm\n",rectx.area(),rectx.valor(3));

   recty.set_values (3,4);
   printf("Area %d metros e %d cm\n",recty.area(),recty.valor(4));
   return 0;
}
