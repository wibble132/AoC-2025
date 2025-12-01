#include "day01.h"
#include "utils.h"

#include <charconv>
#include <print>
#include <ranges>

day01::Day01::Day01(const std::string& input) noexcept
{
    std::vector<Rotation> rotations{};

    for (const auto& line : input | std::views::split('\n'))
    {
        if (line.empty()) continue;

        const char directionChar = line[0];
        const Direction direction = directionChar == 'R' ? Direction{Right{}} : Direction{Left{}};
        uint32_t x = 0;
        std::string_view line_view{line};
        static_assert(std::is_same_v<std::ranges::range_value_t<decltype(line)>, char>);
        std::from_chars_result res = std::from_chars(line_view.begin() + 1, line_view.end(), x);
        if (res.ptr != line_view.end())
        {
            std::println("Day 1: Failed to parse input. Failed at line: \"{}\"", line_view);
            std::terminate();
        }

        rotations.emplace_back(direction, x);
    }

    parsedData = {std::move(rotations)};
}

uint64_t day01::Day01::part1() const noexcept
{
    int32_t current_position = 50;
    uint32_t totalClicks = 0;

    for (const auto& [direction, count] : parsedData.rotations)
    {
        std::visit(overloaded{
            [&current_position, count](const Left& ) -> void {current_position -= static_cast<int32_t>(count); },
            [&current_position, count](const Right&) -> void {current_position += static_cast<int32_t>(count); },
        }, direction);

        totalClicks += current_position % 100 == 0;
    }

    return uint64_t{totalClicks};
}

uint64_t day01::Day01::part2() const noexcept
{
    int32_t current_position = 50;
    uint32_t totalClicks = 0;

    for (const auto& [direction, count] : parsedData.rotations)
    {
        for (uint32_t i = 0; i < count; ++i)
        {
            std::visit(overloaded{
                [&current_position](const Left&)  -> void {current_position -= 1;},
                [&current_position](const Right&) -> void {current_position += 1;},
            }, direction);

            totalClicks += current_position % 100 == 0;
        }
    }

    return uint64_t{totalClicks};
}
