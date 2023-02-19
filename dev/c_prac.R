library(Rcpp)

cppFunction('#include <iostream>
using namespace std;

int main() {
cout << "Welcome To CodeWithHarry";
return 0;
}')

