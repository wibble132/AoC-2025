#ifndef CPP_DAY03_H
#define CPP_DAY03_H

#include <cstdint>
#include <string>
#include <vector>

namespace day03
{
    struct ParsedData
    {
        std::vector<std::vector<uint8_t>> numbers;
    };

    class Day03 final
    {
    public:
        explicit Day03(const std::string& input) noexcept;
        [[nodiscard]] uint64_t part1() const noexcept;
        [[nodiscard]] uint64_t part2() const noexcept;
    private:
        ParsedData parsedData;
    };
}


#endif //CPP_DAY03_H