#pragma once
#include <type_traits>

namespace radix {
    namespace detail {
        // Implement std::void_t (C++17).
        template<typename ...>
        using Void_t = void;

        // Check whether InIt is iterator
        // by checking typename InIt::iterator_category
        template<typename InIt, typename = void>
        struct is_iterator : public std::false_type {};

        // Check whether InIt is iterator.
        // by checking typename InIt::iterator_category
        template<typename InIt>
        struct is_iterator<InIt, Void_t<
            typename std::iterator_traits<InIt>::iterator_category
        > > : public std::true_type {};
    }
}