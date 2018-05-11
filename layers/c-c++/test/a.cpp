#include <iostream>
#include <cstdint>
#include <functional>
#include <boost/asio.hpp>
#include <boost/algorithm/algorithm.hpp>
#include <boost/winapi/waitable_timer.hpp>

struct A {
    int x;
    int y;
    int z;
};

namespace cy {
int add(int, int) {
    return 0;
}

}

int main() {
    A a = {1, 2, 3};
    cy::add(1,2);
    boost::asio::io_context ioc;
    auto f = [](int i) -> int { return 0; };
    auto b = a;
    auto [x,y,z] = a;
}
