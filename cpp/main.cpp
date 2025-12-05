#include "days/day01.h"
#include "days/day03.h"
#include "days/utils.h"

#include <print>
#include <string>

void runDay(uint32_t n);

// TODO: Find why const argv can be const const const const const const const const const con...
int main(const int argc, const char* const* const argv)
{
    if (argc != 2)
    {
        std::println("Usage: program <day_number>");
        return 1;
    }

    const auto day = std::stoi(argv[1]);
    runDay(day);
    return 0;
}

template <typename T>
concept Day = requires(T a, std::string s) {
    { T{ s } };
    { a.part1() } -> std::integral;
    { a.part2() } -> std::integral;
};

template <Day T>
void runParts(const std::string& input)
{
    const T day{input};
    const auto p1 = day.part1();
    const auto p2 = day.part2();

    std::println("Part 1: {}", p1);
    std::println("Part 2: {}", p2);
}

void runDay(uint32_t n)
{
    const std::string input = read_input(n);

    switch (n)
    {
    case 1:
        runParts<day01::Day01>(input);
        break;
    case 3:
        runParts<day03::Day03>(input);
        break;
    default:
        std::println("Err: Unknown day {}", n);
    }
}