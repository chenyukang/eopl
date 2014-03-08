#include <stdio.h>
#include <iostream>
using namespace std;

class Demo {
public:
    virtual void func1() { std:: cout << "func1 " << std::endl; }
    virtual void func2() { this->func1(); }
};


class Drived : public Demo {
public:
    void func1() { std:: cout << "derived: func1 " << std::endl; }
};


int main() {
    Drived* d = new Drived();
    d->func2();
    return 0;
}
