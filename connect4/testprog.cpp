#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

int main(){

  int num1, num2, num3, num5, array[3][7], fileindex;
  bool num4;
  string filename;
  stringstream ss;

  ifstream input1, input2;
  ofstream out1, out2;
  
  fileindex = 0;
  ss << fileindex;
  cout << "stringstream: " << ss.str() << endl;
  //filename = "output/board_info_0.txt";
  filename = "output/board_info_" + ss.str() + ".txt";
  input1.open(filename.c_str());
  if (input1.fail()){
    cout << "shit failed yo" << endl;
    return 0;
  }
  
  input1 >> num1 >> num2;
  input1 >> num3 >> num4 >> num5;
  cout << num1 << " " << num2 << " " << num3 << " " << num4 << " " << num5 << endl;
  
  for (int x1=0; x1<3; x1++)
    for (int x2=0; x2<7; x2++)
      input1 >> array[x1][x2];
  
  input1.close();
  
  //fileindex = 1;
  //ss.str("");
  //ss.clear();
  //ss << fileindex;
  //cout << "stringstream 2: " << ss.str() << endl;
  
  // see if we can print out what we read
  cout << num1 << endl;
  cout << num2 << endl;
  cout << num3 << endl;
  cout << num4 << endl;
  cout << num5 << endl;
  for (int x1=0; x1<3; x1++){
    for (int x2=0; x2<7; x2++)
      cout << "  " << array[x1][x2];
    cout << endl;
  }
  cin.get();
  
  // out1.open("output/test.txt");
//   if (out1.fail()){
//     cout << "shit failed in the output yo." << endl;
//     return 0;
//   }
//
//   out1 << num1 << endl;
//   out1 << endl;
//   out1 << num2 << endl;
//   out1 << "  " << num3 << "  " << num4 << endl;
//   for (int x1=0; x1<3; x1++){
//     for (int x2=0; x2<7; x2++)
//       out1 << "  " << array[x1][x2];
//     out1 << endl;
//   }
//   out1.close();
  
  return 0;
}

