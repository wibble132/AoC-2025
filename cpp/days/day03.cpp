#include "day03.h"

#include <algorithm>
#include <ranges>
#include <print>
#include <string>
#include <span>
#include <cmath>

namespace
{
    template <typename... Ts>
    [[nodiscard]]
    uint64_t chooseNumbers(const std::vector<uint8_t> &numbers, Ts... indices) noexcept
    {
        uint64_t result = 0;
        ((result = result * 10 + static_cast<uint64_t>(numbers[indices])), ...);
        return result;
    }

    template <uint8_t N>
    [[nodiscard]]
    constexpr uint64_t chooseNumbers(const std::span<const uint8_t> numbers)
    {
        if constexpr (N == 0)
        {
            return 0;
        }

        else if constexpr (N == 1)
        {
            return std::ranges::max(numbers);
        } else {

            // Find the largest number that has (N-1) numbers after it
            uint8_t maxVal = 0;
            size_t maxIdx = -1;
            if (numbers.size() < N) {
                return chooseNumbers<N - 1>(numbers);
            }
            for (size_t i = 0; i < numbers.size() - (N - 1); ++i)
            {
                if (numbers[i] > maxVal)
                {
                    maxVal = numbers[i];
                    maxIdx = i;
                }
            }

            if (maxIdx == -1) {
                return chooseNumbers<N - 1>(numbers);
            }

            uint64_t digit = static_cast<uint64_t>(maxVal) * static_cast<uint64_t>(std::pow(10, N - 1));
            return digit + chooseNumbers<N - 1>(numbers.subspan(maxIdx + 1));
        }
    }

    namespace _tests
    {
        static_assert(chooseNumbers<1>(std::array<uint8_t, 3> {1, 2, 3}) == 3);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 3> {2, 2, 2}) == 22);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 3> {2, 2, 3}) == 23);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 4> {2, 2, 3, 2}) == 32);
        static_assert(chooseNumbers<4>(std::array<uint8_t, 4> {2, 2, 3, 2}) == 2232);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 15> {9,8,7,6,5,4,3,2,1,1,1,1,1,1,1}) == 98);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 15> {8,1,1,1,1,1,1,1,1,1,1,1,1,1,9}) == 89);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 15> {2,3,4,2,3,4,2,3,4,2,3,4,2,7,8}) == 78);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 15> {8,1,8,1,8,1,9,1,1,1,1,2,1,1,1}) == 92);
        static_assert(chooseNumbers<12>(std::array<uint8_t, 15> {9,8,7,6,5,4,3,2,1,1,1,1,1,1,1}) == 987654321111);
        static_assert(chooseNumbers<12>(std::array<uint8_t, 15> {8,1,1,1,1,1,1,1,1,1,1,1,1,1,9}) == 811111111119);
        static_assert(chooseNumbers<12>(std::array<uint8_t, 15> {2,3,4,2,3,4,2,3,4,2,3,4,2,7,8}) == 434234234278);
        static_assert(chooseNumbers<12>(std::array<uint8_t, 15> {8,1,8,1,8,1,9,1,1,1,1,2,1,1,1}) == 888911112111);

    }
}

day03::Day03::Day03(const std::string &input) noexcept
{
    auto lines = std::views::split(input, '\n');

    auto transformedLines = std::views::transform(lines, [](auto &&line)
        { return std::views::transform(line, [](char c)
            { return static_cast<uint8_t>(c - '0'); });
        });

    parsedData.numbers = std::ranges::to<std::vector<std::vector<uint8_t>>>(transformedLines);
}

uint64_t day03::Day03::part1() const noexcept
{
    uint64_t total = 0;
    for (const auto &bank : parsedData.numbers)
    {
    //     uint64_t max = 0;
    //     for (auto i = 0; i < bank.size(); ++i)
    //     {
    //         for (auto j = i + 1; j < bank.size(); ++j)
    //         {
    //             auto num = chooseNumbers(bank, i, j);
    //             if (num > max)
    //             {
    //                 max = num;
    //             }
    //         }
    //     }

    //     std::println("{}", max);
    //     total += max;

        if (bank.size() > 0) 
        total += chooseNumbers<2>(std::span(bank.data(), bank.size()));
    }

    return total;
}

uint64_t day03::Day03::part2() const noexcept
{
        uint64_t total = 0;
    for (const auto &bank : parsedData.numbers)
    {
    // uint64_t total = 0;
    // for (const auto &bank : parsedData.numbers)
    // {
    //     uint64_t max = 0;
    //     for (auto i = 0; i < bank.size(); ++i)
    //         for (auto j = i + 1; j < bank.size(); ++j)
    //             for (auto k = j + 1; k < bank.size(); ++k)
    //                 for (auto l = k + 1; l < bank.size(); ++l)
    //                     for (auto m = l + 1; m < bank.size(); ++m)
    //                         for (auto n = m + 1; n < bank.size(); ++n)
    //                             for (auto o = n + 1; o < bank.size(); ++o)
    //                                 for (auto p = o + 1; p < bank.size(); ++p)
    //                                     for (auto q = p + 1; q < bank.size(); ++q)
    //                                         for (auto r = q + 1; r < bank.size(); ++r)
    //                                             for (auto s = r + 1; s < bank.size(); ++s)
    //                                                 for (auto t = s + 1; t < bank.size(); ++t)
    //                                                 {
    //                                                     auto num = chooseNumbers(bank, i, j, k, l, m, n, o, p, q, r, s, t);
    //                                                     if (num > max)
    //                                                     {
    //                                                         max = num;
    //                                                     }
    //                                                 }

    //     std::println("{}", max);
        // total += max;
        if (bank.size() > 0) 
        total += chooseNumbers<12>(std::span(bank.data(), bank.size()));
    }

    return total;
}
