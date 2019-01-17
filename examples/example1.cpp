#include <string>
#include <iostream>
#include <cstdlib>
#include <vector>
#include <list>

#include "../radix_tree.hpp"

template<typename Eng>
std::string make_key(Eng &&eng, std::size_t min_len = 0,
    std::size_t max_len = 5) {
    std::uniform_int_distribution<> rand_elem('a', 'z');
    std::string s;
    size_t len = std::uniform_int_distribution<size_t>{ min_len, max_len }(eng);
    while (len--)
        s.push_back(rand_elem(eng));
    return s;
}

using map_type = radix::radix_map<std::string, int>;
map_type tree;

void insert() {
    tree["apache"]    = 0;
    tree["afford"]    = 1;
    tree["available"] = 2;
    tree["affair"]    = 3;
    tree["avenger"]   = 4;
    tree["binary"]    = 5;
    tree["bind"]      = 6;
    tree["brother"]   = 7;
    tree["brace"]     = 8;
    tree["blind"]     = 9;
    tree["bro"]       = 10;
}

void longest_match(const std::string &key) {
    auto it = tree.longest_match(key);

    std::cout << "longest_match(\"" << key << "\"):" << std::endl;

    if (it != tree.end()) {
        std::cout << "    " << it->first << ", " << it->second << std::endl;
    } else {
        std::cout << "    failed" << std::endl;
    }
}

// prefix_match and greedy_match copy results to a output iterator
// or a container whose value_type is iterator / const_iterator (limited to 
// const_iterator if the tree is const, otherwise both OK)
void prefix_match(const std::string &key) {
    std::list<map_type::iterator> matches;

    // Copy to iterator whose value_type is map_type::iterator 
    tree.prefix_match(key, std::back_inserter(matches));    

    std::cout << "prefix_match(\"" << key << "\"):" << std::endl;

    for (auto it : matches) {
        std::cout << "    " << it->first << ", " << it->second << std::endl;
    }
}

void greedy_match(const std::string &key) {
    std::vector<map_type::const_iterator> matches;

    // Copy to container of map_type::const_iterator
    tree.greedy_match(key, matches);    

    std::cout << "greedy_match(\"" << key << "\"):" << std::endl;

    for (auto it : matches) {
        std::cout << "    " << it->first << ", " << it->second << std::endl;
    }
}

void traverse() {
    std::cout << "traverse:" << std::endl;

    for (const auto &p : tree) {
        std::cout << "    " << p.first << ", " << p.second << std::endl;
    }
}

int main() {
    insert();

    longest_match("binder");
    longest_match("bracelet");
    longest_match("apple");

    prefix_match("aff");
    prefix_match("bi");
    prefix_match("a");

    greedy_match("avoid");
    greedy_match("bring");
    greedy_match("attack");

    traverse();

    tree.erase("bro");
    prefix_match("bro");

    return EXIT_SUCCESS;
}
