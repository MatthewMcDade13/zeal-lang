#include <nanobench.h>

#include <atomic>
#include <cassert>

int main() {
    int y = 0;

    std::atomic<int> x(0);

    ankerl::nanobench::Bench().run("compare_exchange_strong", [&] {
        // zeal::core::BoxStr a = zeal::core::BoxStr::create("testing123lol");
        // assert(a.as_view() == "testing123lol");

        // std::cout << (std::string(a.as_view())) << "\n";
        x.compare_exchange_strong(y, 0);
    });
}
