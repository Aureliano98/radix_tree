#ifndef RADIX_TREE_HPP
#define RADIX_TREE_HPP

#include <cassert>
#include <string>
#include <utility>
#include <functional>
#include <vector>
#include <list>

#include "radix_tree_it.hpp"
#include "radix_tree_node.hpp"
#include "radix_tree_util.hpp"

namespace radix {

    template<typename K, typename T, typename Compare, 
        typename Equal, typename Alloc> class radix_tree;

    namespace detail {

        template<typename K, typename T>
        struct radix_tree_traits_base {
            typedef K key_type;
            typedef T mapped_type;
            typedef T & mapped_type_reference;
            typedef const T & mapped_type_const_reference;
            typedef std::pair<const K, T> value_type;
            
            static const key_type &select_key(const value_type &val) noexcept { 
                return val.first;
            }
        };

        template<typename K>
        struct radix_tree_traits_base<K, void> {
            typedef K key_type;
            typedef void mapped_type;
            typedef void mapped_type_reference;
            typedef void mapped_type_const_reference;
            typedef K value_type;

            static const key_type &select_key(const value_type &val) noexcept {
                return val;
            }
        };

        template<typename K, typename T, typename Compare, typename Equal, typename Alloc>
        struct radix_tree_traits : radix_tree_traits_base<K, T> {
        private:
            typedef radix_tree_traits_base<K, T> base;

        public:
            typedef radix_tree<K, T, Compare, Equal, Alloc> tree_type;
            typedef typename base::key_type key_type;
            typedef typename base::mapped_type mapped_type;
            typedef typename base::value_type value_type;
            typedef std::size_t size_type;
            typedef std::ptrdiff_t difference_type;
            typedef Compare key_compare;
            typedef Equal key_element_equal;    // Extension
            typedef Alloc allocator_type;
            typedef value_type & reference;
            typedef const value_type & const_reference;
            typedef typename std::allocator_traits<Alloc>::pointer pointer;
            typedef typename std::allocator_traits<Alloc>::const_pointer const_pointer;
        };

    }

    // NOTE: Users should put user-defined radix_substr, radix_join, radix_length 
    // in the same namespace as Key, not radix namespace.

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

    // Compressed Radix Tree
    // @param K         key type (a sequence that can be accessed by index)
    // @param T         mapped type
    // @param Compare   comparator of key
    // @param Equal     equality predicate of key's element 
    //                  (e.g., K=std::string, Equal=std::equal_to<char>)
    // @param Alloc     allocator
    template<typename K, typename T, typename Compare = std::less<K>, 
        typename Equal = std::equal_to<
            typename std::decay<decltype(std::declval<K>()[0])>::type>,
        typename Alloc = std::allocator<
            typename detail::radix_tree_traits_base<K, T>::value_type> >
    class radix_tree : 
        private Compare, private Equal,
        private std::allocator_traits<Alloc>::
        template rebind_alloc<
            detail::radix_tree_node<
                detail::radix_tree_traits<K, T, Compare, Equal, Alloc>
            >
        > {
        typedef detail::radix_tree_traits<K, T, Compare, Equal, Alloc> traits;
        typedef detail::radix_tree_node<traits> node_type;
        typedef typename std::allocator_traits<Alloc>::
            template rebind_alloc<node_type> node_allocator;
        typedef std::allocator_traits<node_allocator> node_allocator_traits;
        typedef typename traits::mapped_type_reference mapped_type_reference;
        typedef typename traits::mapped_type_const_reference mapped_type_const_reference;

        struct nonconst_tag {};
        struct const_tag {};

    public:
        typedef typename traits::key_type key_type;
        typedef typename traits::mapped_type mapped_type;
        typedef typename traits::value_type value_type;
        typedef typename traits::size_type size_type;
        typedef typename traits::difference_type difference_type;
        typedef typename traits::key_compare key_compare;
        typedef typename traits::key_element_equal key_element_equal;   // Extension
        typedef typename traits::allocator_type allocator_type;
        typedef typename traits::reference reference;
        typedef typename traits::const_reference const_reference;
        typedef typename traits::pointer pointer;
        typedef typename traits::const_pointer const_pointer;
        typedef detail::radix_tree_const_it<traits> const_iterator;
        typedef typename std::conditional<!std::is_void<mapped_type>::value, 
            detail::radix_tree_it<traits>, const_iterator>::type iterator;

        radix_tree() : radix_tree(key_compare(), 
            key_element_equal(), allocator_type()) {}

        explicit radix_tree(const allocator_type &alloc) :
            radix_tree(key_compare(), key_element_equal(), alloc) {}

        radix_tree(const key_compare &pred, const key_element_equal &eq, 
            const allocator_type &alloc = allocator_type()) : 
            key_compare(pred), key_element_equal(eq), node_allocator(alloc),
            m_size(0), m_root(nullptr) {}

        radix_tree(const radix_tree &other) :
            key_compare(other), key_element_equal(other), 
            node_allocator(node_allocator_traits::
                select_on_container_copy_construction(other)) {
            copy_from(other, false);
        }

        radix_tree(const radix_tree &other, const allocator_type &alloc) :
            key_compare(other), key_element_equal(other),
            node_allocator(alloc) {
            copy_from(other, false);
        }

        radix_tree(radix_tree &&other) :
            key_compare(std::move(other)), key_element_equal(std::move(other)),
            node_allocator(std::move(other)) {
            move_from(other); 
        }

        radix_tree(radix_tree &&other, const allocator_type &alloc) :
            key_compare(std::move(other)), key_element_equal(std::move(other)),
            node_allocator(alloc) {
            move_from(other);
        }

        ~radix_tree() {
            delete_tree(m_root);
        }

        radix_tree &operator=(const radix_tree &other) {
            if (node_allocator_traits::propagate_on_container_copy_assignment::value 
                && get_node_allocator() != other.get_node_allocator()) {
                assert(this != std::addressof(other));
                clear();
                get_node_allocator() = other.get_node_allocator();
            } 

            get_key_compare() = other.get_key_compare();
            get_key_element_equal() = other.get_key_element_equal();
            copy_from(other, true);
            return *this;
        }

        radix_tree &operator=(radix_tree &&other) {
            if (this != std::addressof(other)) {
                if (get_node_allocator() == other.get_node_allocator()) {
                    clear();
                    get_key_compare() = std::move(other.get_key_compare());
                    get_key_element_equal() = std::move(other.get_key_element_equal());
                    move_from(other);
                } else if (node_allocator_traits::propagate_on_container_move_assignment::value) {
                    clear();
                    get_key_compare() = std::move(other.get_key_compare());
                    get_key_element_equal() = std::move(other.get_key_element_equal());
                    get_node_allocator() = std::move(other.get_node_allocator());
                    move_from(other);
                } else {
                    get_key_compare() = other.get_key_compare();
                    get_key_element_equal() = other.get_key_element_equal();
                    copy_from(other, true);
                }
            }

            return *this;
        }

        void swap(radix_tree &other) {
            using std::swap;
            if (node_allocator_traits::propagate_on_container_swap::value) {
                swap(get_node_allocator(), other.get_node_allocator());
            } else {
                assert(get_node_allocator() == other.get_node_allocator());
            }
            swap(m_size, other.m_size);
            swap(m_root, other.m_root);
        }

        size_type size() const noexcept { return m_size; }
        
        bool empty() const noexcept { return m_size == 0; }
        
        void clear() {
            delete_tree(m_root);
            m_root = nullptr;
            m_size = 0;
        }

        const_iterator find(const key_type &key) const {
            if (m_root == nullptr)
                return const_iterator(nullptr);

            const node_type *node = find_node(key, m_root, 0);
            // if the node is a internal node, return nullptr
            if (!node->m_is_leaf)
                return const_iterator(nullptr);
            return const_iterator(node);
        }

        iterator find(const key_type &key) {
            return downcast_iterator(const_cast<const radix_tree *>(this)->find(key));
        }

        size_type count(const key_type &key) const {
            return find(key) != end() ? 1 : 0;
        }

        const_iterator cbegin() const {
            return const_iterator(empty() ? nullptr : begin(m_root));
        }

        const_iterator cend() const { return const_iterator(nullptr); }

        const_iterator begin() const { return cbegin(); }

        iterator begin() { return downcast_iterator(cbegin()); }

        const_iterator end() const { return cend(); }

        iterator end() { return downcast_iterator(cend()); }

        std::pair<iterator, bool> insert(const value_type &val) {
            return insert_impl(val);
        }

        template<typename P>
        typename std::enable_if<
            std::is_constructible<value_type, P &&>::value,
            std::pair<iterator, bool>>::type 
            insert(P &&val) {
            return insert_impl(std::forward<P>(val));
        }

        template<typename InIt>
        void insert(InIt first, InIt last) {
            for (; first != last; ++first)
                emplace(*first);
        }

        template<typename... Types>
        std::pair<iterator, bool> emplace(Types &&...args) {
            return insert_impl(value_type(std::forward<Types>(args)...));
        }

        size_type erase(const key_type &key) {
            if (m_root == nullptr)
                return 0;
            node_type *child = find_node(key, m_root, 0);
            return erase(child);
        }

        // Delete given entry. Undefined if it == end().
        // @return      iterator to the next position
        iterator erase(const_iterator it) {
            iterator next_it = std::next(downcast_iterator(it));
            size_type ret = erase(get_pointer(it));
            assert(ret == 1);
            return next_it;
        }

        iterator erase(const_iterator first, const_iterator last) {
            while (first != last) {
                size_type ret = erase(get_pointer(first++));
                assert(ret == 1);
            }
            return downcast_iterator(last);
        }

        // Copy matching const_iterators to dest.
        // @require const_iterator is convertible to OutIt::value_type
        template<typename OutIt>
        typename std::enable_if<detail::is_iterator<OutIt>::value, OutIt>::type
            prefix_match(const key_type &key, OutIt dest) const {
            return prefix_match_dispatch(key, dest, const_tag());
        }

        // Copy matching iterators to dest.
        // @require iterator is convertible to OutIt::value_type
        template<typename OutIt>
        typename std::enable_if<detail::is_iterator<OutIt>::value, OutIt>::type
            prefix_match(const key_type &key, OutIt dest) {
            return prefix_match_dispatch(key, dest, nonconst_tag());
        }

        // Copy matching const_iterators to vec.
        // @require const_iterator is convertible to Iter
        template<typename Iter, typename Al>
        void prefix_match(const key_type &key, std::vector<Iter, Al> &vec) const {
            vec.clear();
            prefix_match(key, std::back_inserter(vec));
        }

        // Copy matching iterators to vec.
        // @require iterator is convertible to Iter
        template<typename Iter, typename Al>
        void prefix_match(const key_type &key, std::vector<Iter, Al> &vec) {
            vec.clear();
            prefix_match(key, std::back_inserter(vec));
        }

        // Copy matching const_iterators to dest.
        // @require const_iterator is convertible to OutIt::value_type
        template<typename OutIt>
        typename std::enable_if<detail::is_iterator<OutIt>::value, OutIt>::type
            greedy_match(const key_type &key, OutIt dest) const {
            return greedy_match_dispatch(key, dest, const_tag());
        }

        // Copy matching iterators to dest.
        // @require iterator is convertible to OutIt::value_type
        template<typename OutIt>
        typename std::enable_if<detail::is_iterator<OutIt>::value, OutIt>::type
            greedy_match(const key_type &key, OutIt dest) {
            return greedy_match_dispatch(key, dest, nonconst_tag());
        }

        // Copy matching const_iterators to vec.
        // @require const_iterator is convertible to Iter
        template<typename Iter, typename Al>
        void greedy_match(const key_type &key, std::vector<Iter, Al> &vec) const {
            vec.clear();
            greedy_match(key, std::back_inserter(vec));
        }

        // Copy matching iterators to vec.
        // @require iterator is convertible to Iter
        template<typename Iter, typename Al>
        void greedy_match(const key_type &key, std::vector<Iter, Al> &vec) {
            vec.clear();
            greedy_match(key, std::back_inserter(vec));
        }

        const_iterator longest_match(const key_type &key) const {
            if (m_root == nullptr)
                return const_iterator(nullptr);

            const node_type *node = find_node(key, m_root, 0);

            if (node->m_is_leaf)
                return const_iterator(node);

            key_type key_sub = radix_substr(key, node->m_depth, radix_length(node->m_key));

            if (!key_equal(key_sub, node->m_key))
                node = node->m_parent;

            key_type nul = radix_substr(key, 0, 0);

            while (node != nullptr) {
                auto it = node->m_children.find(nul);
                if (it != node->m_children.end() && it->second->m_is_leaf)
                    return const_iterator(it->second);

                node = node->m_parent;
            }

            return const_iterator(nullptr);
        }

        iterator longest_match(const key_type &key) {
            return downcast_iterator(const_cast<const radix_tree *>(this)->longest_match(key));
        }

        template<typename Ty = mapped_type,
            typename = typename std::enable_if<!std::is_void<Ty>::value>::type>
            mapped_type_reference operator[](const key_type &key) {
            iterator it = find(key);
            if (it == end()) {
                std::pair<iterator, bool> ret = insert(value_type(key, mapped_type()));
                assert(ret.second);
                it = ret.first;
            }
            return it->second;
        }

        template<typename Ty = mapped_type, 
            typename = typename std::enable_if<!std::is_void<Ty>::value>::type>
            mapped_type_const_reference at(const key_type &key) const {
            const_iterator it = find(key);
            if (it == cend())
                throw std::out_of_range("Radix tree key out of range");
            return it->second;
        }

        template<typename Ty = mapped_type,
            typename = typename std::enable_if<!std::is_void<Ty>::value>::type>
            mapped_type_reference at(const key_type &key) {
            return const_cast<mapped_type &>(
                const_cast<const radix_tree *>(this)->at(key));
        }

        key_compare key_comp() const { return *this; }

        key_element_equal key_elem_eq() const { return *this; }

        allocator_type get_allocator() const { return *this; }

    private:
        static node_type *begin(node_type *node) {
            while (!node->m_is_leaf) {
                assert(!node->m_children.empty());
                node = node->m_children.begin()->second;
            }
            return node;
        }

        const node_type *find_node(const key_type &key, 
            const node_type *node, int depth) const {
            if (node->m_children.empty())
                return node;

            int len_key = radix_length(key) - depth;

            for (auto it = node->m_children.cbegin(); 
                it != node->m_children.cend(); ++it) {
                if (len_key == 0) {
                    if (it->second->m_is_leaf)
                        return it->second;
                    else
                        continue;
                }

                if (!it->second->m_is_leaf 
                    && get_key_element_equal()(key[depth], it->first[0])) {
                    int len_node = radix_length(it->first);
                    key_type key_sub = radix_substr(key, depth, len_node);

                    if (key_equal(key_sub, it->first)) {
                        return find_node(key, it->second, depth + len_node);
                    } else {
                        return it->second;
                    }
                }
            }

            return node;
        }

        node_type *find_node(const key_type &key, node_type *node, int depth) {
            return const_cast<node_type *>(const_cast<const radix_tree *>(this)
                ->find_node(key, node, depth));
        }

        template<typename P>
        node_type *append(node_type *parent, P &&val) {
            int depth;
            int len;
            key_type nul = radix_substr(traits::select_key(val), 0, 0);
            node_type *node_c, *node_cc;

            depth = parent->m_depth + radix_length(parent->m_key);
            len = radix_length(traits::select_key(val)) - depth;

            if (len == 0) {
                node_c = new_node(std::forward<P>(val));

                node_c->m_depth = depth;
                node_c->m_parent = parent;
                node_c->m_key = nul;
                node_c->m_is_leaf = true;

                parent->m_children[nul] = node_c;

                return node_c;
            } else {
                node_c = new_node(val); // Can't forward twice

                key_type key_sub = radix_substr(traits::select_key(val), depth, len);

                parent->m_children[key_sub] = node_c;

                node_c->m_depth = depth;
                node_c->m_parent = parent;
                node_c->m_key = key_sub;


                node_cc = new_node(std::forward<P>(val));
                node_c->m_children[nul] = node_cc;

                node_cc->m_depth = depth + len;
                node_cc->m_parent = node_c;
                node_cc->m_key = nul;
                node_cc->m_is_leaf = true;

                return node_cc;
            }
        }

        template<typename P>
        node_type *prepend(node_type *node, P &&val) {
            int len1 = radix_length(node->m_key);
            int len2 = radix_length(traits::select_key(val)) - node->m_depth;
            
            int count;
            for (count = 0; count < len1 && count < len2; count++) {
                if (!get_key_element_equal()(node->m_key[count], 
                    traits::select_key(val)[count + node->m_depth])) 
                    break;
            }

            assert(count != 0);

            node->m_parent->m_children.erase(node->m_key);

            node_type *node_a = new_empty_node();

            node_a->m_parent = node->m_parent;
            node_a->m_key = radix_substr(node->m_key, 0, count);
            node_a->m_depth = node->m_depth;
            node_a->m_parent->m_children[node_a->m_key] = node_a;

            node->m_depth += count;
            node->m_parent = node_a;
            node->m_key = radix_substr(node->m_key, count, len1 - count);
            node->m_parent->m_children[node->m_key] = node;

            key_type nul = radix_substr(traits::select_key(val), 0, 0);
            if (count == len2) {
                node_type *node_b;

                node_b = new_node(std::forward<P>(val));

                node_b->m_parent = node_a;
                node_b->m_key = nul;
                node_b->m_depth = node_a->m_depth + count;
                node_b->m_is_leaf = true;
                node_b->m_parent->m_children[nul] = node_b;

                return node_b;
            } else {
                node_type *node_b, *node_c;

                node_b = new_empty_node();

                node_b->m_parent = node_a;
                node_b->m_depth = node->m_depth;
                node_b->m_key = radix_substr(traits::select_key(val), 
                    node_b->m_depth, len2 - count);
                node_b->m_parent->m_children[node_b->m_key] = node_b;

                int node_c_depth = radix_length(traits::select_key(val));
                node_c = new_node(std::forward<P>(val));

                node_c->m_parent = node_b;
                node_c->m_depth = node_c_depth;
                node_c->m_key = nul;
                node_c->m_is_leaf = true;
                node_c->m_parent->m_children[nul] = node_c;

                return node_c;
            }
        }

        template<typename OutIt, typename Tag>
        OutIt greedy_match(const node_type *node, OutIt dest, Tag tag) const {
            if (node->m_is_leaf) {
                *dest++ = make_iterator(const_cast<node_type *>(node), tag);
                return dest;
            }

            for (const auto &p : node->m_children)
                dest = greedy_match(p.second, dest, tag);
            return dest;
        }

        static const_iterator make_iterator(const node_type *node, const_tag) noexcept {
            return const_iterator(node);
        }

        static iterator make_iterator(node_type *node, nonconst_tag) noexcept {
            return iterator(node);
        }

        template<typename... Types>
        node_type *new_node(Types &&...args) {
            node_type *node = node_allocator::allocate(1);
            ::new (node) node_type(*this, *this, std::forward<Types>(args)...);
            return node;
        }

        node_type *new_empty_node() {
            node_type *node = node_allocator::allocate(1);
            ::new (node) node_type(node_type::empty_construct, *this, *this);
            return node;
        }

        void delete_tree(node_type *node) {
            if (node) {
                for (const auto &p : node->m_children)
                    delete_tree(p.second);
                node->~node_type();
                node_allocator::deallocate(node, 1);
            }
        }

        void move_from(radix_tree &other) noexcept {
            m_root = other.m_root;
            m_size = other.m_size;
            other.m_root = nullptr;
            other.m_size = 0;
        }

        void copy_from(const radix_tree &other, bool clr) {
            node_type *cpy = other.empty() ? nullptr : copy_tree(other.m_root);
            if (clr)
                delete_tree(m_root);
            m_root = cpy;
            m_size = other.m_size;
        }

        node_type *copy_tree(const node_type *tree) {
            assert(tree);
            node_type *cpy = tree->m_holds_value ? 
                new_node(tree->get_value()) : new_empty_node();
            cpy->m_depth = tree->m_depth;
            cpy->m_is_leaf = tree->m_is_leaf;
            cpy->m_key = tree->m_key;
            for (const auto &p : tree->m_children) {
                assert(p.second);
                node_type *child = copy_tree(p.second);
                cpy->m_children.emplace(p.first, child);
                child->m_parent = cpy;
            }
            return cpy;
        }

        static node_type *get_pointer(const_iterator it) noexcept {
            return const_cast<node_type *>(it.m_pointee);
        }

        static iterator downcast_iterator(const_iterator it) noexcept {
            return iterator(get_pointer(it));
        }

        template<typename OutIt, typename Tag>
        OutIt prefix_match_dispatch(const key_type &key, OutIt dest, Tag tag) const {
            if (m_root == nullptr)
                return dest;

            const node_type *node = find_node(key, m_root, 0);
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
            if (m_root == nullptr)
                return dest;

            const node_type *node = find_node(key, m_root, 0);
            if (node->m_is_leaf)
                node = node->m_parent;
            return greedy_match(node, dest, tag);
        }

        node_allocator &get_node_allocator() noexcept { return *this; }

        const node_allocator &get_node_allocator() const noexcept { return *this; }

        key_compare &get_key_compare() noexcept { return *this; }

        const key_compare &get_key_compare() const noexcept { return *this; }

        key_element_equal &get_key_element_equal() noexcept { return *this; }

        const key_element_equal &get_key_element_equal() const noexcept { return *this; }

        bool key_equal(const key_type &x, const key_type &y) const {
            return !get_key_compare()(x, y) && !get_key_compare()(y, x);
        }

        size_type erase(node_type *child) {
            if (!child || !child->m_is_leaf)
                return 0;

            key_type nul = radix_substr(traits::select_key(child->get_value()), 0, 0);
            node_type *parent = child->m_parent, *grandparent = nullptr;
            parent->m_children.erase(nul);
            delete_tree(child);
            m_size--;

            if (parent == m_root || parent->m_children.size() > 1)
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
                auto it = grandparent->m_children.begin();

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

        template<typename P>
        std::pair<iterator, bool> insert_impl(P &&val) {
            if (m_root == nullptr) {
                key_type nul = radix_substr(traits::select_key(val), 0, 0);

                m_root = new_empty_node();
                m_root->m_key = nul;
            }

            node_type *node = find_node(traits::select_key(val), m_root, 0);

            if (node->m_is_leaf) {
                return { iterator(node), false };
            } else if (node == m_root) {
                m_size++;
                return { iterator(append(m_root, std::forward<P>(val))), true };
            } else {
                m_size++;
                int len = radix_length(node->m_key);
                key_type key_sub = radix_substr(traits::select_key(val), node->m_depth, len);

                if (key_equal(key_sub, node->m_key)) {
                    return { iterator(append(node, std::forward<P>(val))), true };
                } else {
                    return { iterator(prepend(node, std::forward<P>(val))), true };
                }
            }
        }

        size_type m_size;
        node_type *m_root;
    };

    template<typename K, typename T, typename Compare, typename Equal, typename Alloc>
    bool operator==(const radix_tree<K, T, Compare, Equal, Alloc> &x,
        const radix_tree<K, T, Compare, Equal, Alloc> &y) {
        return x.size() == y.size()
            && std::equal(x.cbegin(), x.cend(), y.cbegin());
    }

    template<typename K, typename T, typename Compare, typename Equal, typename Alloc>
    bool operator!=(const radix_tree<K, T, Compare, Equal, Alloc> &x,
        const radix_tree<K, T, Compare, Equal, Alloc> &y) {
        return !(x == y);
    }

    template<typename K, typename T, typename Compare, typename Equal, typename Alloc>
    bool operator<(const radix_tree<K, T, Compare, Equal, Alloc> &x,
        const radix_tree<K, T, Compare, Equal, Alloc> &y) {
        return std::lexicographical_compare(x.cbegin(), x.cend(), y.cbegin(), y.cend());
    }

    template<typename K, typename T, typename Compare, typename Equal, typename Alloc>
    bool operator<=(const radix_tree<K, T, Compare, Equal, Alloc> &x,
        const radix_tree<K, T, Compare, Equal, Alloc> &y) {
        return !(y < x);
    }

    template<typename K, typename T, typename Compare, typename Equal, typename Alloc>
    bool operator>(const radix_tree<K, T, Compare, Equal, Alloc> &x,
        const radix_tree<K, T, Compare, Equal, Alloc> &y) {
        return y < x;
    }

    template<typename K, typename T, typename Compare, typename Equal, typename Alloc>
    bool operator>=(const radix_tree<K, T, Compare, Equal, Alloc> &x,
        const radix_tree<K, T, Compare, Equal, Alloc> &y) {
        return !(x < y);
    }

    template<typename K, typename T, typename Compare, typename Equal, typename Alloc>
    void swap(const radix_tree<K, T, Compare, Equal, Alloc> &x,
        const radix_tree<K, T, Compare, Equal, Alloc> &y) {
        x.swap(y);
    }

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
