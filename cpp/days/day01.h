#ifndef CPP_DAY01_H
#define CPP_DAY01_H

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

namespace day01
{
    class Left {};
    class Right {};
    class Direction : public std::variant<Left, Right> {};
    struct Rotation
    {
        Rotation(const Direction direction, const uint32_t count) : direction(direction), count(count) {}

        Direction direction;
        uint32_t count;
    };

    struct ParsedData
    {
        std::vector<Rotation> rotations;
    };

    class Day01 final
    {
    public:
        explicit Day01(const std::string& input) noexcept;
        [[nodiscard]] uint64_t part1() const noexcept;
        [[nodiscard]] uint64_t part2() const noexcept;
    private:
        ParsedData parsedData;
    };
}


#endif //CPP_DAY01_H