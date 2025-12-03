#ifndef CPP_UTILS_H
#define CPP_UTILS_H

#include <format>
#include <fstream>
#include <filesystem>
#include <iterator>
#include <print>

template <typename... Ts>
struct overloaded : Ts...
{
    using Ts::operator()...;
};

template <typename... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

inline auto read_input(uint32_t day) -> std::string
{
    const std::filesystem::path filePath{std::format("../../inputs/day{:02}.txt", day)};
    std::ifstream infile{filePath};
    std::string result =  {std::istreambuf_iterator<char>{infile}, std::istreambuf_iterator<char>{}};

    if (result.empty())
    {
        std::println("Failed to read file at path {}", filePath.string());
        std::exit(1);
    }

    return result;
}

#endif //CPP_UTILS_H
