

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
}

using string_counter_map = radix::radix_tree<std::string, Counter,
    std::greater<std::string>, std::equal_to<char>>;

TEST(find, mapped_type_count) {
    std::vector<std::string> unique_keys = get_unique_keys();
    string_counter_map tree;
    std::random_shuffle(unique_keys.begin(), unique_keys.end());

    for (const std::string &key : unique_keys) {
        typename string_counter_map::iterator it; 
        bool inserted;
        std::tie(it, inserted) = tree.emplace(key, Counter());
        ASSERT_TRUE(inserted);
    }
    // NOTE: Counter::get_count() >= tree.size()

    tree.clear();
    ASSERT_EQ(Counter::get_count(), 0);
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
        // NOTE: Counter::get_count() >= tree.size()

        auto tree2 = tree;
        ASSERT_EQ(tree.size(), tree2.size()); 

        tree.clear();
        ASSERT_TRUE(tree.empty());

        tree = tree2;
        ASSERT_EQ(tree.size(), tree2.size());
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
        // NOTE: Counter::get_count() >= tree.size()

        auto tree2 = std::move(tree);
        auto tree3 = tree2;
        tree = std::move(tree3);
        swap(tree, tree2);
    }
    ASSERT_EQ(Counter::get_count(), 0);
}