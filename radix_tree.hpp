#ifndef RADIX_TREE_HPP
#define RADIX_TREE_HPP

#include <cassert>
#include <string>
#include <utility>
#include <functional>
#include <vector>

#include "radix_tree_it.hpp"
#include "radix_tree_node.hpp"
#include "radix_tree_util.hpp"

namespace radix {

    template<typename K, typename T, typename Compare, 
        typename Alloc> class radix_tree;

    namespace detail {

        template<typename K, typename T, typename Compare, typename Alloc>
        struct radix_tree_traits {
            typedef radix_tree<K, T, Compare, Alloc> tree_type;
            typedef K key_type;
            typedef T mapped_type;
            typedef std::pair<const K, T> value_type;
            typedef std::size_t size_type;
            typedef Compare key_compare;
            typedef Alloc allocator_type;
            typedef value_type & reference;
            typedef const value_type & const_reference;
            typedef value_type * pointer;
            typedef const value_type * const_pointer;
        };

    }

    template<typename CharT, typename Traits, typename Alloc>
    inline std::basic_string<CharT, Traits, Alloc> radix_substr(
        const std::basic_string<CharT, Traits, Alloc> &key, 
        int begin, int num) {
        return key.substr(begin, num);
    }

    template<typename CharT, typename Traits, typename Alloc>
    inline std::basic_string<CharT, Traits, Alloc> radix_join(
        const std::basic_string<CharT, Traits, Alloc> &key1,
        const std::basic_string<CharT, Traits, Alloc> &key2) {
        return key1 + key2;
    }

    template<typename CharT, typename Traits, typename Alloc>
    inline int radix_length(const std::basic_string<CharT, Traits, Alloc> &key) {
        return static_cast<int>(key.size()); 
    }

    template<typename K, typename T, typename Compare = std::less<K>, 
        typename Alloc = std::allocator<std::pair<const K, T> > >
    class radix_tree : 
        private Compare, 
        private std::allocator_traits<Alloc>::
        template rebind_alloc<
            detail::radix_tree_node<
                detail::radix_tree_traits<K, T, Compare, Alloc>
            >
        > {
        typedef detail::radix_tree_traits<K, T, Compare, Alloc> traits;
        typedef detail::radix_tree_node<traits> node_type;
        typedef typename std::allocator_traits<Alloc>::
            template rebind_alloc<node_type> node_allocator;

        struct nonconst_tag {};
        struct const_tag {};

    public:
        typedef typename traits::key_type key_type;
        typedef typename traits::mapped_type mapped_type;
        typedef typename traits::value_type value_type;
        typedef typename traits::size_type size_type;
        typedef typename traits::key_compare key_compare;
        typedef typename traits::allocator_type allocator_type;
        typedef detail::radix_tree_const_it<traits> const_iterator;
        typedef detail::radix_tree_it<traits> iterator;

        radix_tree() : radix_tree(key_compare(), allocator_type()) {}
        
        explicit radix_tree(const key_compare &pred) : radix_tree(pred, allocator_type()) {}

        explicit radix_tree(const allocator_type &alloc) : radix_tree(key_compare(), allocator_type()) {}

        radix_tree(const key_compare &pred, const allocator_type &alloc) : 
            key_compare(pred), node_allocator(alloc),
            m_size(0), m_root(NULL) {}

        ~radix_tree() {
            delete_tree(m_root);
        }

        radix_tree(const radix_tree &other) = delete;
        radix_tree &operator=(const radix_tree &other) = delete;

        size_type size() const noexcept { return m_size; }
        
        bool empty() const noexcept { return m_size == 0; }
        
        void clear() {
            delete_tree(m_root);
            m_root = NULL;
            m_size = 0;
        }

        const_iterator find(const key_type &key) const {
            if (m_root == NULL)
                return const_iterator(NULL);

            node_type *node = find_node(key, m_root, 0);
            // if the node is a internal node, return NULL
            if (!node->m_is_leaf)
                return const_iterator(NULL);
            return const_iterator(node);
        }

        iterator find(const key_type &key) {
            return downcast_iterator(const_cast<const radix_tree *>(this)->find(key));
        }

        const_iterator begin() const {
            node_type *node;
            if (m_root == NULL || m_size == 0)
                node = NULL;
            else
                node = begin(m_root);
            return const_iterator(node);
        }

        iterator begin() {
            return downcast_iterator(const_cast<const radix_tree *>(this)->begin());
        }

        const_iterator end() const {
            return const_iterator(NULL);
        }

        iterator end() {
            return iterator(NULL);
        }

        std::pair<iterator, bool> insert(const value_type &val) {
            if (m_root == NULL) {
                key_type nul = radix_substr(val.first, 0, 0);

                m_root = new_node();
                m_root->m_key = nul;
            }

            node_type *node = find_node(val.first, m_root, 0);

            if (node->m_is_leaf) {
                return std::pair<iterator, bool>(iterator(node), false);
            } else if (node == m_root) {
                m_size++;
                return std::pair<iterator, bool>(iterator(append(m_root, val)), true);
            } else {
                m_size++;
                int len = radix_length(node->m_key);
                key_type key_sub = radix_substr(val.first, node->m_depth, len);

                if (key_sub == node->m_key) {
                    return std::pair<iterator, bool>(iterator(append(node, val)), true);
                } else {
                    return std::pair<iterator, bool>(iterator(prepend(node, val)), true);
                }
            }
        }

        std::pair<iterator, bool> insert_or_assign(const value_type &val) {
            std::pair<iterator, bool> ib = insert(val);
            if (!ib.second)
                ib.first->second = val.second;
            return ib;
        }

        size_type erase(const key_type &key) {
            if (m_root == NULL)
                return 0;

            node_type *child;
            node_type *parent;
            node_type *grandparent;
            key_type nul = radix_substr(key, 0, 0);

            child = find_node(key, m_root, 0);

            if (!child->m_is_leaf)
                return 0;

            parent = child->m_parent;
            parent->m_children.erase(nul);

            delete_tree(child);

            m_size--;

            if (parent == m_root)
                return 1;

            if (parent->m_children.size() > 1)
                return 1;

            if (parent->m_children.empty()) {
                grandparent = parent->m_parent;
                grandparent->m_children.erase(parent->m_key);
                delete_tree(parent);
            } else {
                grandparent = parent;
            }

            if (grandparent == m_root) {
                return 1;
            }

            if (grandparent->m_children.size() == 1) {
                // merge grandparent with the uncle
                typename node_type::map_iterator it;
                it = grandparent->m_children.begin();

                node_type *uncle = it->second;

                if (uncle->m_is_leaf)
                    return 1;

                uncle->m_depth = grandparent->m_depth;
                uncle->m_key = radix_join(grandparent->m_key, uncle->m_key);
                uncle->m_parent = grandparent->m_parent;

                grandparent->m_children.erase(it);

                grandparent->m_parent->m_children.erase(grandparent->m_key);
                grandparent->m_parent->m_children[uncle->m_key] = uncle;

                delete_tree(grandparent);
            }

            return 1;
        }

        iterator erase(const_iterator it) {
            iterator next = std::next(downcast_iterator(it));
            return erase(it->first) ? next : end();
        }

        template<typename OutIt>
        typename std::enable_if<detail::is_iterator<OutIt>::value, OutIt>::type
            prefix_match(const key_type &key, OutIt dest) const {
            return prefix_match_dispatch(key, dest, const_tag());
        }

        template<typename OutIt>
        typename std::enable_if<detail::is_iterator<OutIt>::value, OutIt>::type
            prefix_match(const key_type &key, OutIt dest) {
            return prefix_match_dispatch(key, dest, nonconst_tag());
        }

        template<typename OutIt>
        typename std::enable_if<detail::is_iterator<OutIt>::value, OutIt>::type
            greedy_match(const key_type &key, OutIt dest) const {
            return greedy_match_dispatch(key, dest, const_tag());
        }

        template<typename Iter, typename Al>
        void prefix_match(const key_type &key, std::vector<Iter, Al> &vec) const {
            vec.clear();
            prefix_match(key, std::back_inserter(vec));
        }

        template<typename Iter, typename Al>
        void prefix_match(const key_type &key, std::vector<Iter, Al> &vec) {
            vec.clear();
            prefix_match(key, std::back_inserter(vec));
        }

        template<typename OutIt>
        typename std::enable_if<detail::is_iterator<OutIt>::value, OutIt>::type
            greedy_match(const key_type &key, OutIt dest) {
            return greedy_match_dispatch(key, dest, nonconst_tag());
        }

        template<typename Iter, typename Al>
        void greedy_match(const key_type &key, std::vector<Iter, Al> &vec) const {
            vec.clear();
            greedy_match(key, std::back_inserter(vec));
        }

        template<typename Iter, typename Al>
        void greedy_match(const key_type &key, std::vector<Iter, Al> &vec) {
            vec.clear();
            greedy_match(key, std::back_inserter(vec));
        }

        const_iterator longest_match(const key_type &key) const {
            if (m_root == NULL)
                return const_iterator(NULL);

            node_type *node = find_node(key, m_root, 0);

            if (node->m_is_leaf)
                return const_iterator(node);

            key_type key_sub = radix_substr(key, node->m_depth, radix_length(node->m_key));

            if (!(key_sub == node->m_key))
                node = node->m_parent;

            key_type nul = radix_substr(key, 0, 0);

            while (node != NULL) {
                typename node_type::map_iterator it = node->m_children.find(nul);
                if (it != node->m_children.end() && it->second->m_is_leaf)
                    return const_iterator(it->second);

                node = node->m_parent;
            }

            return const_iterator(NULL);
        }

        iterator longest_match(const key_type &key) {
            return downcast_iterator(const_cast<const radix_tree *>(this)->longest_match(key));
        }

        mapped_type &operator[](const key_type &key) {
            iterator it = find(key);
            if (it == end()) {
                std::pair<K, T> val;
                val.first = key;
                std::pair<iterator, bool> ret = insert(val);
                assert(ret.second == true);
                it = ret.first;
            }
            return it->second;
        }

    private:
        node_type *begin(node_type *node) const {
            while (!node->m_is_leaf) {
                assert(!node->m_children.empty());
                node = node->m_children.begin()->second;
            }
            return node;
        }

        node_type *find_node(const key_type &key, node_type *node, int depth) const {
            if (node->m_children.empty())
                return node;

            typename node_type::map_iterator it;
            int len_key = radix_length(key) - depth;

            for (it = node->m_children.begin(); it != node->m_children.end(); ++it) {
                if (len_key == 0) {
                    if (it->second->m_is_leaf)
                        return it->second;
                    else
                        continue;
                }

                if (!it->second->m_is_leaf && key[depth] == it->first[0]) {
                    int len_node = radix_length(it->first);
                    key_type key_sub = radix_substr(key, depth, len_node);

                    if (key_sub == it->first) {
                        return find_node(key, it->second, depth + len_node);
                    } else {
                        return it->second;
                    }
                }
            }

            return node;
        }

        node_type *append(node_type *parent, const value_type &val) {
            int depth;
            int len;
            key_type nul = radix_substr(val.first, 0, 0);
            node_type *node_c, *node_cc;

            depth = parent->m_depth + radix_length(parent->m_key);
            len = radix_length(val.first) - depth;

            if (len == 0) {
                node_c = new_node(val);

                node_c->m_depth = depth;
                node_c->m_parent = parent;
                node_c->m_key = nul;
                node_c->m_is_leaf = true;

                parent->m_children[nul] = node_c;

                return node_c;
            } else {
                node_c = new_node(val);

                key_type key_sub = radix_substr(val.first, depth, len);

                parent->m_children[key_sub] = node_c;

                node_c->m_depth = depth;
                node_c->m_parent = parent;
                node_c->m_key = key_sub;


                node_cc = new_node(val);
                node_c->m_children[nul] = node_cc;

                node_cc->m_depth = depth + len;
                node_cc->m_parent = node_c;
                node_cc->m_key = nul;
                node_cc->m_is_leaf = true;

                return node_cc;
            }
        }

        node_type *prepend(node_type *node, const value_type &val) {
            int count;
            int len1, len2;

            len1 = radix_length(node->m_key);
            len2 = radix_length(val.first) - node->m_depth;

            for (count = 0; count < len1 && count < len2; count++) {
                if (!(node->m_key[count] == val.first[count + node->m_depth]))
                    break;
            }

            assert(count != 0);

            node->m_parent->m_children.erase(node->m_key);

            node_type *node_a = new_node();

            node_a->m_parent = node->m_parent;
            node_a->m_key = radix_substr(node->m_key, 0, count);
            node_a->m_depth = node->m_depth;
            node_a->m_parent->m_children[node_a->m_key] = node_a;


            node->m_depth += count;
            node->m_parent = node_a;
            node->m_key = radix_substr(node->m_key, count, len1 - count);
            node->m_parent->m_children[node->m_key] = node;

            key_type nul = radix_substr(val.first, 0, 0);
            if (count == len2) {
                node_type *node_b;

                node_b = new_node(val);

                node_b->m_parent = node_a;
                node_b->m_key = nul;
                node_b->m_depth = node_a->m_depth + count;
                node_b->m_is_leaf = true;
                node_b->m_parent->m_children[nul] = node_b;

                return node_b;
            } else {
                node_type *node_b, *node_c;

                node_b = new_node();

                node_b->m_parent = node_a;
                node_b->m_depth = node->m_depth;
                node_b->m_key = radix_substr(val.first, node_b->m_depth, len2 - count);
                node_b->m_parent->m_children[node_b->m_key] = node_b;

                node_c = new_node(val);

                node_c->m_parent = node_b;
                node_c->m_depth = radix_length(val.first);
                node_c->m_key = nul;
                node_c->m_is_leaf = true;
                node_c->m_parent->m_children[nul] = node_c;

                return node_c;
            }
        }

        template<typename OutIt, typename Tag>
        OutIt greedy_match(node_type *node, OutIt dest, Tag tag) const {
            if (node->m_is_leaf) {
                *dest++ = make_iterator(node, tag);
                return dest;
            }

            for (typename node_type::map_iterator 
                it = node->m_children.begin(); 
                it != node->m_children.end(); ++it) {
                dest = greedy_match(it->second, dest, tag);
            }
            return dest;
        }

        static const_iterator make_iterator(node_type *node, const_tag) {
            return const_iterator(node);
        }

        static iterator make_iterator(node_type *node, nonconst_tag) {
            return iterator(node);
        }

        node_type *new_node(const value_type &val) {
            node_type *node = node_allocator::allocate(1);
            ::new (node) node_type(val, *this, *this);
            return node;
        }

        node_type *new_node() {
            node_type *node = node_allocator::allocate(1);
            ::new (node) node_type(*this, *this);
            return node;
        }

        void delete_tree(node_type *node) {
            if (node) {
                typename node_type::map_iterator it;
                for (it = node->m_children.begin(); it != node->m_children.end(); ++it) {
                    delete_tree(it->second);
                }
                node->~node_type();
                node_allocator::deallocate(node, 1);
            }
        }

        static node_type *get_pointer(const_iterator it) {
            return const_cast<node_type *>(it.m_pointee);
        }

        static iterator downcast_iterator(const_iterator it) {
            return iterator(get_pointer(it));
        }

        template<typename OutIt, typename Tag>
        OutIt prefix_match_dispatch(const key_type &key, OutIt dest, Tag tag) const {
            if (m_root == NULL)
                return dest;

            node_type *node = find_node(key, m_root, 0);
            if (node->m_is_leaf)
                node = node->m_parent;

            int len = radix_length(key) - node->m_depth;
            key_type key_sub1 = radix_substr(key, node->m_depth, len);
            key_type key_sub2 = radix_substr(node->m_key, 0, len);
            if (key_sub1 != key_sub2)
                return dest;

            return greedy_match(node, dest, tag);
        }

        template<typename OutIt, typename Tag>
        OutIt greedy_match_dispatch(const key_type &key, OutIt dest, Tag tag) const {
            if (m_root == NULL)
                return dest;

            node_type *node = find_node(key, m_root, 0);
            if (node->m_is_leaf)
                node = node->m_parent;
            return greedy_match(node, dest, tag);
        }

        size_type m_size;
        node_type *m_root;
    };

}

/*

(root)
|
|---------------
|       |      |
abcde   bcdef  c
|   |   |      |------
|   |   $3     |  |  |
f   ge         d  e  $6
|   |          |  |
$1  $2         $4 $5

find_node():
  bcdef  -> $3
  bcdefa -> bcdef
  c      -> $6
  cf     -> c
  abch   -> abcde
  abc    -> abcde
  abcde  -> abcde
  abcdef -> $1
  abcdeh -> abcde
  de     -> (root)


(root)
|
abcd
|
$

(root)
|
$

*/

#endif // RADIX_TREE_HPP
