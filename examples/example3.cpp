#include <string>
#include <iostream>
#include <cstdlib>
#include <vector>
#include <list>

#include "../radix_tree.hpp"

using std::vector;
using std::list;
using std::string;
using tree_set = radix::radix_tree<string, void>;

namespace {

    tree_set tree;

    void insert() {
        vector<string> strs = {
            "apache", "afford", "available", "affair",
            "avenger", "binary", "bind", "brother",
            "brace", "blind", "bro" };
        for (const string &s : strs)
            tree.emplace(std::move(s));
    }

    void longest_match(std::string key) {
        auto it = tree.longest_match(key);

        std::cout << "longest_match(\"" << key << "\"):" << std::endl;

        if (it != tree.end()) {
            std::cout << "    " << *it << std::endl;
        } else {
            std::cout << "    failed" << std::endl;
        }
    }

    void prefix_match(std::string key) {
        std::list<tree_set::iterator> lst;
        tree.prefix_match(key, std::back_inserter(lst));

        std::cout << "prefix_match(\"" << key << "\"):" << std::endl;

        for (auto it : lst) {
            std::cout << "    " << *it << std::endl;
        }
    }

    void greedy_match(std::string key) {
        std::list<tree_set::iterator> lst;
        tree.prefix_match(key, std::back_inserter(lst));
        tree.greedy_match(key, std::back_inserter(lst));

        std::cout << "greedy_match(\"" << key << "\"):" << std::endl;

        for (auto it : lst) {
            std::cout << "    " << *it << std::endl;
        }
    }

    void traverse() {
        std::cout << "traverse:" << std::endl;
        for (const auto &s : tree)
            std::cout << "    " << s << std::endl;
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
