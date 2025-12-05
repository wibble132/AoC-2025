#include "day03.h"

#include <algorithm>
#include <complex>
#include <functional>
#include <ranges>
#include <print>
#include <string>
#include <span>

namespace
{
    template <uint8_t N>
    constexpr auto ipow10() -> uint64_t
    {
        if constexpr (N == 0)
            return 1;
        else return 10 * ipow10<N - 1>();
    }

    template <uint8_t N>
    [[nodiscard]]
    __attribute__((always_inline)) constexpr
    auto chooseNumbers(const std::span<const uint8_t> numbers) -> uint64_t
    {
        if constexpr (N == 0) return 0;
        else if constexpr (N == 1) return std::ranges::max(numbers);
        else
        {
            // Find the largest number that has (N-1) numbers after it
            if (numbers.size() < N)
            {
                return chooseNumbers<N - 1>(numbers);
            }

            const auto digitsToSearch = numbers.size() - (N - 1);

            // Can't point to a specific overload of std::get, so need to craft my own type if I want to type out the
            // template parameters of max_element below.
            const auto proj = [](
                const std::tuple<
                    std::ranges::range_difference_t<std::span<const uint8_t>>,
                    std::ranges::range_value_t<std::span<const uint8_t>>
                >& x) ->
                std::remove_reference_t<std::ranges::range_value_t<std::span<const uint8_t>>>
            {
                return std::get<
                    1,
                    std::ranges::range_difference_t<std::span<const uint8_t>>,
                    std::ranges::range_value_t<std::span<const uint8_t>>
                >(x);
            };

            const auto& [maxIdx, maxVal] = *std::ranges::max_element.operator()
                <std::ranges::enumerate_view<std::span<const uint8_t>>, decltype(proj), std::ranges::less>
                (numbers.subspan(0, digitsToSearch) | std::views::enumerate, {}, proj);

            uint64_t digit = static_cast<uint64_t>(maxVal) * ipow10<N - 1>();
            return digit + chooseNumbers<N - 1>(numbers.subspan(maxIdx + 1));
        }
    }

    namespace _tests
    {
        // @formatter:off
        static_assert(chooseNumbers<1>(std::array<uint8_t, 3>{1, 2, 3}) == 3);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 3>{2, 2, 2}) == 22);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 3>{2, 2, 3}) == 23);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 3>{2, 3, 2}) == 32);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 4>{2, 2, 3, 2}) == 32);
        static_assert(chooseNumbers<4>(std::array<uint8_t, 4>{2, 2, 3, 2}) == 2232);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 15>{9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1}) == 98);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 15>{8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9}) == 89);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 15>{2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8}) == 78);
        static_assert(chooseNumbers<2>(std::array<uint8_t, 15>{8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1}) == 92);
        static_assert(chooseNumbers<12>(std::array<uint8_t, 15>{9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1}) == 987654321111);
        static_assert(chooseNumbers<12>(std::array<uint8_t, 15>{8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9}) == 811111111119);
        static_assert(chooseNumbers<12>(std::array<uint8_t, 15>{2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8}) == 434234234278);
        static_assert(chooseNumbers<12>(std::array<uint8_t, 15>{8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1}) == 888911112111);
        // @formatter:on
    }
}

day03::Day03::Day03(const std::string& input) noexcept
{
    auto lines = std::views::split(input, '\n');

    auto transformedLines = std::views::transform(lines, [](auto&& line)
    {
        return std::views::transform(line, [](const char c)
        {
            return static_cast<uint8_t>(c - '0');
        });
    });

    auto nonEmpty = std::views::filter(transformedLines, [](auto&& line) { return !line.empty(); });

    parsedData.numbers = std::ranges::to<std::vector<std::vector<uint8_t>>>(nonEmpty);
}

template <uint8_t N, std::ranges::range R> requires
    std::convertible_to<std::ranges::range_value_t<R>, std::span<const uint8_t>>
[[nodiscard]] constexpr auto doPart(R&& range) -> uint64_t
{
    return std::ranges::fold_left(
        std::views::transform(range, [](auto&& bank) { return chooseNumbers<N>(bank); }),
        0, std::plus{});
}

uint64_t day03::Day03::part1() const noexcept
{
    return doPart<2>(parsedData.numbers);
}

uint64_t day03::Day03::part2() const noexcept
{
    return doPart<12>(parsedData.numbers);
}
