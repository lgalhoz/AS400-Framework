#include <iostream>
#include <iomanip>
#include <fstream>
using namespace std;

int main() {
   int sum = 0;
   char str[250];
   int x;
   ifstream inFile;

 inFile.open("/interfaces/JMClubeTAP/ZURVID_RECEMI_20141007_101433568_DD.csv");
   if (!inFile) {
      cout << "Unable to open file";
      exit(1); // terminate with error
   }

   while(inFile) {
      inFile.getline(str, 255);
      cout << str << endl;
   }
   inFile.close();
   return 0;
}
