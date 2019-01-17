#include <cstddef>
#include <cstdint>

#include "common.hpp"

namespace {

    class Counter {
    public:
        Counter() {
            ++count;
        }

        Counter(const Counter &other) {
            ++count;
        }

        ~Counter() {
            check_count();
            --count;
        }

        static std::int64_t get_count() noexcept {
            return count;
        }

    private:
        void check_count() const {
            ASSERT_GE(count, 0);
        }

        static std::int64_t count;
    };

    std::int64_t Counter::count = 0;

    using string_counter_map = radix::radix_map<std::string, Counter,
        std::greater<std::string>, std::equal_to<char>>;
    using value_type = typename string_counter_map::value_type;

    bool tree_equal(const string_counter_map &x, const string_counter_map &y) {
        using const_reference = typename string_counter_map::const_reference;
        return x.size() == y.size() && std::equal(x.cbegin(), x.cend(), y.cbegin(), 
            [](const_reference x, const_reference y) {
            return x.first == y.first;
        });
    }

    bool tree_sorted(const string_counter_map &t) {
        return std::is_sorted(t.cbegin(), t.cend(),
            [](const value_type &x, const value_type &y) {
            return x.first >= y.first;
        });
    }

}

TEST(find, tree_copy) {
    std::vector<std::string> unique_keys = get_unique_keys();
    {
        string_counter_map tree;
        std::random_shuffle(unique_keys.begin(), unique_keys.end());

        for (const std::string &key : unique_keys) {
            typename string_counter_map::iterator it;
            bool inserted;
            std::tie(it, inserted) = tree.emplace(key, Counter());
            ASSERT_TRUE(inserted);
        }
        ASSERT_EQ(Counter::get_count(), tree.size());
        ASSERT_TRUE(tree_sorted(tree));

        auto tree2 = tree;
        ASSERT_TRUE(tree_equal(tree, tree2));
        ASSERT_EQ(Counter::get_count(), tree.size() + tree2.size());

        tree = tree;
        ASSERT_TRUE(tree_equal(tree, tree2));
        ASSERT_EQ(Counter::get_count(), tree.size() + tree2.size());

        tree.clear();
        ASSERT_TRUE(tree.empty());
        ASSERT_EQ(Counter::get_count(), tree.size() + tree2.size());

        tree = tree;
        ASSERT_TRUE(tree.empty());
        ASSERT_EQ(Counter::get_count(), tree.size() + tree2.size());

        tree = tree2;
        ASSERT_TRUE(tree_equal(tree, tree2));
        ASSERT_EQ(Counter::get_count(), tree.size() + tree2.size());
    }
    ASSERT_EQ(Counter::get_count(), 0);
}

TEST(find, tree_move) {
    using std::swap;
    std::vector<std::string> unique_keys = get_unique_keys();
    {
        string_counter_map tree;
        std::random_shuffle(unique_keys.begin(), unique_keys.end());

        for (const std::string &key : unique_keys) {
            typename string_counter_map::iterator it;
            bool inserted;
            std::tie(it, inserted) = tree.emplace(key, Counter());
            ASSERT_TRUE(inserted);
        }
        ASSERT_EQ(Counter::get_count(), tree.size());
        ASSERT_TRUE(tree_sorted(tree));

        auto tree2 = std::move(tree);
        ASSERT_EQ(Counter::get_count(), tree2.size());

        auto tree3 = tree2;
        ASSERT_TRUE(tree_equal(tree2, tree3));
        ASSERT_EQ(Counter::get_count(), tree2.size() + tree3.size());

        tree = std::move(tree3);
        ASSERT_TRUE(tree_equal(tree, tree2));
        ASSERT_EQ(Counter::get_count(), tree.size() + tree2.size());

        tree = std::move(tree);
        ASSERT_TRUE(tree_equal(tree, tree2));
        ASSERT_EQ(Counter::get_count(), tree.size() + tree2.size());

        tree = tree;
        ASSERT_TRUE(tree_equal(tree, tree2));
        ASSERT_EQ(Counter::get_count(), tree.size() + tree2.size());

        swap(tree, tree2);
        ASSERT_TRUE(tree_equal(tree, tree2));
        ASSERT_EQ(Counter::get_count(), tree.size() + tree2.size());
    }
    ASSERT_EQ(Counter::get_count(), 0);
}

TEST(find, empty_base_optimization) {
    ASSERT_EQ(sizeof(string_counter_map), sizeof(void *));
}