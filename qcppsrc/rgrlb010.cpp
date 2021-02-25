//----------------------------------------------------------------//
//  CRTBNDCPP PGM(#JMPRDSRC/RGRLB002) SRCFILE(#JMPRDSRC/QCPPSRC)  //
//      SRCMBR(RGRLB010) REPLACE(*YES)                            //
//----------------------------------------------------------------//
#include <stdio.h>
#include <stdlib.h>

//========================================================
// Fun��o....: random
// Objectivo.: Usar fun��es Standard do C (Random)
//========================================================
int random() {
  int     rand_num;
  rand_num = rand() %100 + 1;
  return(rand_num);
}

//========================================================
// Objecto...: Point
// Objectivo.: Usar objectos em C++ no AS400
//========================================================
class Point {
  private:
    double x;
    double y;
  public:
    virtual void Translate(double dx, double dy);
    virtual void Scale(double fx, double fy);
    virtual void Rotate(double angle);
    virtual double DistanceTo (const Point& other) const;
}


//========================================================
// Fun��o....: main
// Objectivo.: Estrutura de uma fun��o main
//========================================================
int main(int argc, char* argv[]){
  int i;
  char* c;
  i = 4;
  for(i=0;i < argc;i++) {
      printf("Valor = %c", argv[i]);
      printf("Parametro  = %d\n", i);
  }
  return 0;
}
