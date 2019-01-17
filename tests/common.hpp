#include <random>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>

#include <gtest/gtest.h>

#include "radix_tree.hpp"

// this file contains some common code for all tests to reduce the number of copypaste lines

typedef radix::radix_map<std::string, int> tree_t;
typedef std::vector<tree_t::iterator> vector_found_t;
typedef std::unordered_map<std::string, int> map_found_t;

constexpr std::size_t DEFAULT_TEST_SIZE = 128;
constexpr std::size_t DEFAULT_MIN_KEYLEN = 0;
constexpr std::size_t DEFAULT_MAX_KEYLEN = 5;

template< typename T, size_t N >
std::vector<T> make_vector( const T (&data)[N]) {
    return std::vector<T>(data, data+N);
}

template<typename Eng>
std::string make_key(Eng &&eng, std::size_t min_len = DEFAULT_MIN_KEYLEN,
    std::size_t max_len = DEFAULT_MAX_KEYLEN) {
    std::uniform_int_distribution<> rand_elem('a', 'z');
    std::string s;
    size_t len = std::uniform_int_distribution<size_t>{ min_len, max_len }(eng);
    while (len--)
        s.push_back(rand_elem(eng));
    return s;
}

template<typename Eng>
typename std::enable_if<
    !std::is_arithmetic<typename std::decay<Eng>::type>::value,
    std::vector<std::string>>::type
    get_unique_keys(Eng &&eng, std::size_t sz = DEFAULT_TEST_SIZE,
        std::size_t min_len = DEFAULT_MIN_KEYLEN, 
        std::size_t max_len = DEFAULT_MAX_KEYLEN) {
    std::unordered_set<std::string> set;
    set.reserve(sz);
    while (set.size() < sz)
        set.emplace(make_key(eng, min_len, max_len));
    return std::vector<std::string>(set.cbegin(), set.cend());
}

std::vector<std::string> get_unique_keys(
    std::size_t sz = DEFAULT_TEST_SIZE, 
    std::size_t min_len = DEFAULT_MIN_KEYLEN, 
    std::size_t max_len = DEFAULT_MAX_KEYLEN) {
    return get_unique_keys(
        std::default_random_engine{ std::random_device{}() }, 
        sz, min_len, max_len);
}

map_found_t vec_found_to_map(const vector_found_t& vec) {
    map_found_t result;
    for (size_t i = 0; i < vec.size(); i++) {
        tree_t::iterator it = vec[i];
        result[it->first] = it->second;
    }
    return result;
}

template<typename Cont>
bool check_size(Cont &&cont) {
    using size_type = decltype(cont.size());
    return static_cast<size_type>(
            std::distance(std::begin(cont), std::end(cont))) == cont.size()
        && static_cast<size_type>(
            std::distance(std::rbegin(cont), std::rend(cont))) == cont.size();
}
