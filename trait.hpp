#pragma once

#include <functional>
#include <tuple>
#include <type_traits>
#include <variant>


namespace traits::____detail {

template<typename Tuple, typename... Args>
struct tuple_types_compatible;

template<typename... TupleTypes, typename... Args>
struct tuple_types_compatible<std::tuple<TupleTypes...>, Args...> {
    static constexpr bool value =
        sizeof...(TupleTypes) == sizeof...(Args) &&
        (std::is_convertible_v<std::decay_t<Args>, std::decay_t<TupleTypes>> && ...);
};

template<typename Tuple, typename... Args>
inline constexpr bool tuple_types_compatible_v = tuple_types_compatible<Tuple, Args...>::value;

}


#define trait_add(retype, name, ...) \
    namespace impl { [[maybe_unused]] void name(const decltype([]{})&&){} } \
    template <typename... Args> \
    auto name(Args&&... args) { \
        using DefArgs = std::tuple<__VA_ARGS__>; \
        static_assert(sizeof...(Args) == std::tuple_size_v<DefArgs>); \
        static_assert(traits::____detail::tuple_types_compatible_v<DefArgs, Args...>, "Trait argument types do not match definition"); \
        auto args_tuple = std::make_tuple(std::forward<Args>(args)...); \
        return [args_tuple = std::move(args_tuple)](auto&& _this) mutable -> retype { \
            return std::apply( \
                [&](auto&&... unpacked_args) -> retype { \
                    return impl::name(std::forward<decltype(_this)>(_this), std::forward<decltype(unpacked_args)>(unpacked_args)...); \
                }, \
                args_tuple \
            ); \
        }; \
    }

    
#define trait_add_tmpl(retype, name,...) \
namespace impl { [[maybe_unused]] void name(const decltype([]{})&&){} } \
template <typename T, typename... Args> \
auto name(Args&&... args) { \
    auto args_tuple = std::make_tuple(std::forward<Args>(args)...); \
    using DefArgs = std::tuple<__VA_ARGS__>; \
    static_assert(sizeof...(Args) == std::tuple_size_v<DefArgs>); \
    static_assert(traits::____detail::tuple_types_compatible_v<DefArgs, Args...>, "Trait argument types do not match definition"); \
    return [args_tuple = std::move(args_tuple)](auto&& _this) mutable -> retype { \
        return std::apply( \
            [&](auto&&... unpacked_args) -> retype { \
                return impl::name<T>(std::forward<decltype(_this)>(_this), std::forward<decltype(unpacked_args)>(unpacked_args)...); \
            }, \
            args_tuple \
        ); \
    }; \
}

template <typename T, typename F,
          typename = std::enable_if_t<std::is_invocable_v<F, T>>>
constexpr auto operator->*(T&& lhs, F&& rhs) 
    noexcept(noexcept(std::invoke(std::forward<F>(rhs), std::forward<T>(lhs))))
{
    return std::invoke(std::forward<F>(rhs), std::forward<T>(lhs));
}

template <typename... Ts, typename F,
          typename = std::enable_if_t<(std::is_invocable_v<F, Ts> && ...)>>
constexpr auto operator->*(std::variant<Ts...>& var, F&& rhs)
{
    return std::visit([&](auto&& val) {
        return std::forward<decltype(val)>(val)->*std::forward<F>(rhs);
    }, var);
}

template <typename F, typename... Ts,
          typename = std::enable_if_t<(std::is_invocable_v<F, const Ts> && ...)>>
constexpr auto operator->*(const std::variant<Ts...>& var, F&& rhs)
{
    return std::visit([&](const auto& val) -> decltype(auto) {
        return val->*std::forward<F>(rhs);
    }, var);
}
