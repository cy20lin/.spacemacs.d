#include <iostream>
#include <chrono>
#include <ctime>

long fibonacci(unsigned n)
{
    if (n < 2) return n;
    return fibonacci(n-1) + fibonacci(n-2);
}

int main()
{
    std::cout << "Hello world" << std::endl;
    std::cout << "Wow, Spacemacs !?" << std::endl;
    static_assert(false, "fail here");
    std::cin.ig
    return 0;
}
