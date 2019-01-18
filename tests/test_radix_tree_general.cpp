#include <cstddef>
#include <cstdint>
#include <map>
#include <algorithm>
#include <numeric>
#include <functional>

#include "common.hpp"

using namespace std;

struct LexicographicalGreater {
    bool operator()(const std::string &x, const std::string &y) const {
        return std::lexicographical_compare(x.begin(), 
            x.end(), y.begin(), y.end(), std::greater<>());
    }
};

using trie_map = radix::radix_map<string, int, LexicographicalGreater>;
using std_map = std::map<string, int, LexicographicalGreater>;

bool iterator_equal(trie_map::const_iterator trie_it, 
    std_map::const_iterator rb_tree_it,
    const trie_map &trie, const std_map &rb_tree) {
    return (trie_it == trie.end()) == (rb_tree_it == rb_tree.end())
        && (trie_it == trie.end() || *trie_it == *rb_tree_it);
}

template<typename Cont1, typename Cont2>
bool container_equal(Cont1 &&x, Cont2 &&y) {
    return x.size() == y.size() 
        && std::equal(x.begin(), x.end(), y.begin());
}

TEST(general, diff_with_std_map) {
    default_random_engine eng{ random_device{}() };
    uniform_int_distribution<int> rand;
    
    radix::radix_map<string, int, LexicographicalGreater> trie;
    std::map<string, int, LexicographicalGreater> rb_tree;

    vector<int> probs;
    vector<std::function<void()>> funcs;

    // Insert
    probs.push_back(2);
    funcs.emplace_back([&] {
        string key = make_key(eng);
        int n = rand(eng);
        trie_map::iterator trie_it; bool trie_inserted;
        std_map::iterator rb_tree_it; bool rb_tree_inserted;
        std::tie(trie_it, trie_inserted) = trie.emplace(key, n);
        std::tie(rb_tree_it, rb_tree_inserted) = rb_tree.emplace(key, n);
        ASSERT_TRUE(trie_inserted == rb_tree_inserted);
        ASSERT_TRUE(iterator_equal(trie_it, rb_tree_it, trie, rb_tree));
        ASSERT_TRUE(container_equal(trie, rb_tree));
    });

    // Insert or assign
    probs.push_back(1);
    funcs.emplace_back([&] {
        string key = make_key(eng);
        int n = rand(eng);
        trie[key] = n;
        rb_tree[key] = n;
        ASSERT_TRUE(container_equal(trie, rb_tree));
    });

    // Lookup
    probs.push_back(1);
    funcs.emplace_back([&] {
        string key = make_key(eng);

        ASSERT_EQ(trie.count(key), rb_tree.count(key));

        trie_map::iterator trie_it = trie.find(key);
        std_map::iterator rb_tree_it = rb_tree.find(key);
        ASSERT_TRUE(iterator_equal(trie_it, rb_tree_it, trie, rb_tree));
        
        trie_map::iterator trie_lb = trie.lower_bound(key);
        std_map::iterator rb_tree_lb = rb_tree.lower_bound(key);
        ASSERT_TRUE(iterator_equal(trie_lb, rb_tree_lb, trie, rb_tree));

        trie_map::iterator trie_ub = trie.upper_bound(key);
        std_map::iterator rb_tree_ub = rb_tree.upper_bound(key);
        ASSERT_TRUE(iterator_equal(trie_ub, rb_tree_ub, trie, rb_tree));
        
        std::tie(trie_lb, trie_ub) = trie.equal_range(key);
        std::tie(rb_tree_lb, rb_tree_ub) = rb_tree.equal_range(key);
        ASSERT_TRUE(iterator_equal(trie_lb, rb_tree_lb, trie, rb_tree));
        ASSERT_TRUE(iterator_equal(trie_ub, rb_tree_ub, trie, rb_tree));
    });

    // Erase node
    probs.push_back(1);
    funcs.emplace_back([&] {
        if (!trie.empty()) {
            size_t k = std::uniform_int_distribution<size_t>{ 0, trie.size() - 1 }(eng);
            trie_map::iterator trie_it = trie.erase(std::next(trie.begin(), k));
            std_map::iterator rb_tree_it = rb_tree.erase(std::next(rb_tree.begin(), k));
            ASSERT_TRUE(iterator_equal(trie_it, rb_tree_it, trie, rb_tree));
            ASSERT_TRUE(container_equal(trie, rb_tree));
        }
    });

    // Erase key
    probs.push_back(1);
    funcs.emplace_back([&] {
        string key = make_key(eng);
        trie_map::size_type trie_erased = trie.erase(key);
        std_map::size_type rb_tree_erased = rb_tree.erase(key);
        ASSERT_EQ(trie_erased, rb_tree_erased);
        ASSERT_TRUE(container_equal(trie, rb_tree));
    });

    std::partial_sum(probs.begin(), probs.end(), probs.begin());
    uniform_int_distribution<int> rand_var(0, probs.back() - 1);

    for (size_t times = 1 << 11; times--; ) {
        int x = rand_var(eng);
        size_t k = std::upper_bound(probs.cbegin(), probs.cend(), x) - probs.cbegin();
        funcs[k]();
    }
}