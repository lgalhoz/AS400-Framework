#include <iostream>
#include <string.h>
#include "CurrencyQuoteBean.hpp"

using namespace std;
#pragma map (convert(xsd__string,float), "CONVERT")

float convert(xsd__string devise, float amount) {
   // create the service proxy object
   // give the service port as parameter

   CurrencyQuoteBean* cb = new CurrencyQuoteBean(
    "http://9.212.15.63:9080/CurrencyQuote/services/CurrencyQuoteBean");

   // invoke the web service by calling the business method.
   float result = cb->convert(devise,amount);

   return result;
}
