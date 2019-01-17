#ifndef RADIX_TREE_HPP
#define RADIX_TREE_HPP

#include <cassert>
#include <string>
#include <utility>
#include <functional>

#include "radix_tree_it.hpp"
#include "radix_tree_node.hpp"
#include "radix_tree_util.hpp"

namespace radix {

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

    namespace detail {

        template<typename K, typename T, typename Compare,
            typename Equal, typename Alloc> class radix_tree;

        template<typename K>
        struct key_element {
            typedef typename std::decay<decltype(std::declval<K>()[0])>::type type;
        };

        template<typename K, typename T>
        struct radix_tree_traits_base {
            typedef K key_type;
            typedef T mapped_type;
            typedef T & mapped_type_reference;
            typedef const T & mapped_type_const_reference;
            typedef std::pair<const K, T> value_type;

            static const key_type &get_key(const value_type &val) noexcept {
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

            static const key_type &get_key(const value_type &val) noexcept {
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

        // Compressed Radix Tree.
        // Users should use radix::radix_map and radix::radix_set aliases instead.
        // 
        // In fact, Compare and Equal can be substitued with one "less" predicate
        // of K (key_type)'s element, but separating them may benefit performance
        // in certain cases.
        //
        // @param K         key type (a sequence that can be accessed by index)
        // @param T         mapped type (can be void, in which case the tree is a set)
        // @param Compare   lexicographical comparison of K
        //                  (e.g., K=std::string, Equal=std::less<std::string>,
        //                  but Equal cannot be std::greater<std::string>)
        // @param Equal     equality predicate of key's element 
        //                  (e.g., K=std::string, Equal=std::equal_to<char>)
        // @param Alloc     allocator
        template<typename K, typename T, typename Compare, typename Equal, typename Alloc>
        class radix_tree : private Compare, private Equal,
            private std::allocator_traits<Alloc>::
                template rebind_alloc<detail::radix_tree_node<
                    detail::radix_tree_traits<K, T, Compare, Equal, Alloc>
                >
            > {
            typedef detail::radix_tree_traits<K, T, Compare, Equal, Alloc> traits;
            typedef detail::radix_tree_node<traits> node_type;
            typedef typename std::allocator_traits<Alloc>::
                template rebind_alloc<node_type> node_allocator;
            typedef typename std::allocator_traits<Alloc>::
                template rebind_traits<node_type> node_allocator_traits;
            typedef std::allocator_traits<Alloc> value_allocator_traits;
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
            typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
            typedef std::reverse_iterator<iterator> reverse_iterator;

            radix_tree() : radix_tree(key_compare(),
                key_element_equal(), allocator_type()) {}

            explicit radix_tree(const allocator_type &alloc) :
                radix_tree(key_compare(), key_element_equal(), alloc) {}

            radix_tree(const key_compare &pred, const key_element_equal &eq,
                const allocator_type &alloc = allocator_type()) :
                key_compare(pred), key_element_equal(eq), node_allocator(alloc) {
                construct_header();
            }

            radix_tree(const radix_tree &other) :
                key_compare(other), key_element_equal(other),
                node_allocator(value_allocator_traits::
                    select_on_container_copy_construction(
                        other.get_allocator())) {
                construct_header();
                copy_from(other, std::false_type());
            }

            radix_tree(const radix_tree &other, const allocator_type &alloc) :
                key_compare(other), key_element_equal(other),
                node_allocator(alloc) {
                construct_header();
                copy_from(other, std::false_type());
            }

            radix_tree(radix_tree &&other) :
                key_compare(std::move(other)), key_element_equal(std::move(other)),
                node_allocator(std::move(other)) {
                construct_header();
                move_from(other, std::false_type());
            }

            radix_tree(radix_tree &&other, const allocator_type &alloc) :
                key_compare(std::move(other)), key_element_equal(std::move(other)),
                node_allocator(alloc) {
                construct_header();
                if (alloc == other.get_allocator()) {
                    // Allocators compare equal, OK to move all except header
                    move_from(other, std::false_type());
                } else {
                    // Move each node
                    piecewise_move_from(other, std::false_type());
                }
            }

            ~radix_tree() {
                clear_all();
            }

            radix_tree &operator=(const radix_tree &other) {
                if (this != std::addressof(other)) {
                    bool reload =
                        value_allocator_traits::propagate_on_container_copy_assignment::value
                        && get_node_allocator() != other.get_node_allocator();
                    if (reload)
                        clear_all();
                    typename value_allocator_traits::propagate_on_container_copy_assignment tag;
                    detail::assign_if(get_node_allocator(), other.get_node_allocator(), tag);
                    if (reload)
                        construct_header();
                    get_key_compare() = other.get_key_compare();
                    get_key_element_equal() = other.get_key_element_equal();
                    copy_from(other, std::true_type());
                }
                return *this;
            }

            radix_tree &operator=(radix_tree &&other) {
                if (this != std::addressof(other)) {
                    bool reload = value_allocator_traits::propagate_on_container_move_assignment::value
                        && get_node_allocator() != other.get_node_allocator();
                    if (reload)
                        clear_all();
                    typename value_allocator_traits::propagate_on_container_move_assignment tag;
                    detail::assign_if(get_node_allocator(), std::move(other.get_node_allocator()), tag);
                    if (reload)
                        construct_header();
                    get_key_compare() = std::move(other.get_key_compare());
                    get_key_element_equal() = std::move(other.get_key_element_equal());
                    if (get_node_allocator() == other.get_node_allocator()) {
                        move_from(other, std::true_type());
                    } else {
                        piecewise_move_from(other, std::true_type());
                    }
                }

                return *this;
            }

            radix_tree &operator=(std::initializer_list<value_type> ilist) {
                clear();
                insert(ilist);
                return *this;
            }

            void swap(radix_tree &other) {
                using std::swap;
                if (value_allocator_traits::propagate_on_container_swap::value) {
                    swap(get_node_allocator(), other.get_node_allocator());
                } else {
                    assert(get_node_allocator() == other.get_node_allocator());
                }
                swap(get_key_compare(), other.get_key_compare());
                swap(get_key_element_equal(), other.get_key_element_equal());
                swap(m_header, other.m_header);
            }

            size_type size() const noexcept { return get_size(); }

            size_type max_size() const noexcept {
                return node_allocator_traits::max_size(get_node_allocator());
            }

            bool empty() const noexcept { return get_size() == 0; }

            const_iterator cbegin() const {
                return const_iterator(empty() ? m_header : get_root()->leftmost());
            }

            const_iterator cend() const noexcept { return const_iterator(m_header); }

            const_iterator begin() const { return cbegin(); }

            iterator begin() { return downcast_iterator(cbegin()); }

            const_iterator end() const noexcept { return cend(); }

            iterator end() noexcept { return downcast_iterator(cend()); }

            const_reverse_iterator crbegin() const noexcept { return const_reverse_iterator(cend()); }

            const_reverse_iterator crend() const { return const_reverse_iterator(cbegin()); }

            const_reverse_iterator rbegin() const noexcept { return crbegin(); }

            reverse_iterator rbegin() noexcept { return reverse_iterator(end()); }

            const_reverse_iterator rend() const { return crend(); }

            reverse_iterator rend() { return reverse_iterator(begin()); }

            void clear() {
                if (m_header) {
                    delete_tree(get_root());
                    set_root(nullptr);
                    get_size() = 0;
                }
            }

            std::pair<iterator, bool> insert(const value_type &val) {
                return insert_nohint(val);
            }

            template<typename P>
            typename std::enable_if<
                std::is_constructible<value_type, P &&>::value,
                std::pair<iterator, bool>>::type
                insert(P &&val) {
                return insert_nohint(std::forward<P>(val));
            }

            template<typename InIt>
            void insert(InIt first, InIt last) {
                for (; first != last; ++first)
                    emplace(*first);
            }

            // Same as emplace_hint. 
            // This also makes the class compatible with std::inserter.
            iterator insert(const_iterator hint, const value_type &val) {
                return emplace_hint(hint, val);
            }

            template<typename P>
            typename std::enable_if<
                std::is_constructible<value_type, P &&>::value,
                std::pair<iterator, bool>>::type
                insert(const_iterator hint, P &&val) {
                return emplace_hint(hint, std::forward<P>(val));
            }

            void insert(std::initializer_list<value_type> ilist) {
                insert(ilist.begin(), ilist.end());
            }

            template<typename... Types>
            std::pair<iterator, bool> emplace(Types &&...args) {
                return insert_nohint(value_type(std::forward<Types>(args)...));
            }

            template<typename... Types>
            iterator emplace_hint(const_iterator hint, Types &&...args) {
                if (hint == cend())
                    return insert_nohint(value_type(std::forward<Types>(args)...)).first;
                assert(!empty());
                return insert_hint(get_pointer(hint),
                    value_type(std::forward<Types>(args)...)).first;
            }

            size_type erase(const key_type &key) {
                if (empty())
                    return 0;
                node_type *child = find_node(key, get_root(), 0);
                return erase_node(child);
            }

            // Delete given entry. Undefined if it == end().
            // @return      iterator to the next position
            iterator erase(const_iterator it) {
                assert(it != end());
                iterator next_it = std::next(downcast_iterator(it));
                size_type ret = erase_node(get_pointer(it));
                assert(ret == 1);
                return next_it;
            }

            iterator erase(const_iterator first, const_iterator last) {
                while (first != last) {
                    size_type ret = erase_node(get_pointer(first++));
                    assert(ret == 1);
                }
                return downcast_iterator(last);
            }

            const_iterator find(const key_type &key) const {
                if (empty())
                    return cend();

                const node_type *node = find_node(key, get_root(), 0);
                // if the node is a internal node, return cend()
                if (!node->is_leaf())
                    return cend();
                return const_iterator(node);
            }

            iterator find(const key_type &key) {
                return downcast_iterator(const_cast<const radix_tree *>(this)->find(key));
            }

            size_type count(const key_type &key) const {
                return find(key) != cend() ? 1 : 0;
            }

            const_iterator lower_bound(const key_type &key) const {
                return const_iterator(lower_bound_impl(key));
            }

            iterator lower_bound(const key_type &key) {
                return iterator(const_cast<node_type *>(lower_bound_impl(key)));
            }

            const_iterator upper_bound(const key_type &key) const {
                return const_iterator(upper_bound_impl(key));
            }

            iterator upper_bound(const key_type &key) {
                return iterator(const_cast<node_type *>(upper_bound_impl(key)));
            }

            std::pair<const_iterator, const_iterator> equal_range(const key_type &key) const {
                auto range = equal_range_impl(key);
                return { const_iterator(range.first), const_iterator(range.second) };
            }

            std::pair<iterator, iterator> equal_range(const key_type &key) {
                auto range = equal_range_impl(key);
                return { iterator(const_cast<node_type *>(range.first)), 
                    iterator(const_cast<node_type *>(range.second)) };
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

            // Copy matching const_iterators to cont.
            // @require const_iterator is convertible to Cont::value_type
            template<typename Cont>
            typename std::enable_if<!detail::is_iterator<Cont>::value, void>::type
                prefix_match(const key_type &key, Cont &cont) const {
                cont.clear();
                prefix_match(key, std::inserter(cont, std::end(cont)));
            }

            // Copy matching iterators to cont.
            // @require iterator is convertible to Cont::value_type
            template<typename Cont>
            typename std::enable_if<!detail::is_iterator<Cont>::value, void>::type
                prefix_match(const key_type &key, Cont &cont) {
                cont.clear();
                prefix_match(key, std::inserter(cont, std::end(cont)));
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

            // Copy matching const_iterators to cont.
            // @require const_iterator is convertible to Cont::value_type
            template<typename Cont>
            typename std::enable_if<!detail::is_iterator<Cont>::value, void>::type
                greedy_match(const key_type &key, Cont &cont) const {
                cont.clear();
                greedy_match(key, std::inserter(cont, std::end(cont)));
            }

            // Copy matching iterators to cont.
            // @require iterator is convertible to Cont::value_type
            template<typename Cont>
            typename std::enable_if<!detail::is_iterator<Cont>::value, void>::type
                greedy_match(const key_type &key, Cont &cont) {
                cont.clear();
                greedy_match(key, std::inserter(cont, std::end(cont)));
            }

            const_iterator longest_match(const key_type &key) const {
                if (empty())
                    return cend();

                const node_type *node = find_node(key, get_root(), 0);

                if (node->is_leaf())
                    return const_iterator(node);

                key_type key_sub = radix_substr(key, node->depth(),
                    radix_length(node->m_key));

                if (!key_equal(key_sub, node->m_key))
                    node = node->m_parent;

                key_type nul = radix_substr(key, 0, 0);

                while (node != m_header) {
                    auto it = node->children().find(nul);
                    if (it != node->children().end() && it->second->is_leaf())
                        return const_iterator(it->second);

                    node = node->m_parent;
                }

                return cend();
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

            key_compare key_comp() const { return get_key_compare(); }

            key_element_equal key_elem_eq() const { return get_key_element_equal(); }

            allocator_type get_allocator() const { return get_node_allocator(); }

        private:
            void construct_header() {
                m_header = node_allocator_traits::allocate(get_node_allocator(), 1);
                m_header->m_parent = nullptr;
                set_root(nullptr);
                get_size() = 0;
            }

            const node_type *find_node(const key_type &key,
                const node_type *node, int depth) const {
                assert(depth == node->depth() + radix_length(node->m_key));
                if (node->is_leaf() || node->children().empty())
                    return node;

                int len_key = radix_length(key) - depth;

                for (auto it = node->children().cbegin();
                    it != node->children().cend(); ++it) {
                    if (len_key == 0) {
                        if (it->second->is_leaf())
                            return it->second;
                        else
                            continue;
                    }

                    if (!it->second->is_leaf()
                        && get_key_element_equal()(key[depth], it->first[0])) { 
                        int len_node = radix_length(it->first);
                        if (key_equal(radix_substr(key, depth, len_node), it->first)) {
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
                key_type nul = radix_substr(traits::get_key(val), 0, 0);
                node_type *node_c, *node_cc;

                depth = parent->depth() + radix_length(parent->m_key);
                len = radix_length(traits::get_key(val)) - depth;

                if (len == 0) {
                    node_c = new_leaf_node(nul, std::forward<P>(val));

                    node_c->set_depth(depth);
                    node_c->m_parent = parent;

                    parent->children()[nul] = node_c;

                    return node_c;
                } else {
                    key_type key_sub = radix_substr(traits::get_key(val), depth, len);
                    // Fixed the bug here: node_c shouldn't hold value
                    node_c = new_internal_node(key_sub);

                    parent->children()[key_sub] = node_c;

                    node_c->set_depth(depth);
                    node_c->m_parent = parent;

                    node_cc = new_leaf_node(nul, std::forward<P>(val));
                    node_c->children()[nul] = node_cc;

                    node_cc->set_depth(depth + len);
                    node_cc->m_parent = node_c;

                    return node_cc;
                }
            }

            template<typename P>
            node_type *prepend(node_type *node, P &&val) {
                int len1 = radix_length(node->m_key);
                int len2 = radix_length(traits::get_key(val)) - node->depth();

                int count;
                int minlen = len1 < len2 ? len1 : len2;
                for (count = 0; count < minlen; count++) {
                    if (!get_key_element_equal()(node->m_key[count],
                        traits::get_key(val)[count + node->depth()]))
                        break;
                }
                assert(count != 0);

                node->m_parent->children().erase(node->m_key);

                node_type *node_a = new_internal_node(radix_substr(node->m_key, 0, count));

                node_a->m_parent = node->m_parent;
                node_a->set_depth(node->depth());
                node_a->m_parent->children()[node_a->m_key] = node_a;

                node->set_depth(node->depth() + count);
                node->m_parent = node_a;
                node->m_key = radix_substr(node->m_key, count, len1 - count);
                node->m_parent->children()[node->m_key] = node;

                key_type nul = radix_substr(traits::get_key(val), 0, 0);
                if (count == len2) {
                    node_type *node_b = new_leaf_node(nul, std::forward<P>(val));

                    node_b->m_parent = node_a;
                    node_b->set_depth(node_a->depth() + count);
                    node_b->m_parent->children()[nul] = node_b;

                    return node_b;
                } else {
                    node_type *node_b, *node_c;

                    node_b = new_internal_node(radix_substr(traits::get_key(val),
                        node->depth(), len2 - count));

                    node_b->m_parent = node_a;
                    node_b->set_depth(node->depth());
                    node_b->m_parent->children()[node_b->m_key] = node_b;

                    int node_c_depth = radix_length(traits::get_key(val));
                    node_c = new_leaf_node(nul, std::forward<P>(val));

                    node_c->m_parent = node_b;
                    node_c->set_depth(node_c_depth);
                    node_c->m_parent->children()[nul] = node_c;

                    return node_c;
                }
            }

            template<typename OutIt, typename Tag>
            OutIt greedy_match_impl(const node_type *node, OutIt dest, Tag tag) const {
                if (node->is_leaf()) {
                    *dest++ = make_iterator(const_cast<node_type *>(node), tag);
                    return dest;
                }

                for (const auto &p : node->children())
                    dest = greedy_match_impl(p.second, dest, tag);
                return dest;
            }

            static const_iterator make_iterator(const node_type *node, const_tag) noexcept {
                return const_iterator(node);
            }

            static iterator make_iterator(node_type *node, nonconst_tag) noexcept {
                return iterator(node);
            }

            template<typename Key, typename... Types>
            node_type *new_leaf_node(Key &&key, Types &&...args) {
                node_type *node = node_allocator_traits::allocate(get_node_allocator(), 1);
                ::new (node) node_type(std::forward<Key>(key));
                node_allocator_traits::construct(get_node_allocator(), &node->value(), 
                    std::forward<Types>(args)...);
                return node;
            }

            template<typename Key>
            node_type *new_internal_node(Key &&key) {
                node_type *node = node_allocator_traits::allocate(get_node_allocator(), 1);
                ::new (node) node_type(std::forward<Key>(key), 
                    get_key_compare(), get_node_allocator());
                return node;
            }

            void delete_tree(node_type *node) {
                if (node) {
                    if (node->is_leaf()) {
                        node_allocator_traits::destroy(get_node_allocator(),
                            std::addressof(node->value()));
                    } else {
                        for (const auto &p : node->children())
                            delete_tree(p.second);
                    }
                    node->~node_type();
                    node_allocator_traits::deallocate(get_node_allocator(), node, 1);
                }
            }

            void clear_if(std::true_type) { clear(); }

            void clear_if(std::false_type) const noexcept {}

            void clear_all() {
                if (m_header) {
                    delete_tree(get_root());
                    node_allocator_traits::deallocate(get_node_allocator(), m_header, 1);
                    m_header = nullptr;
                }
            }

            template<typename Tag>
            void move_from(radix_tree &other, Tag clr) noexcept {
                assert(m_header && other.m_header);
                clear_if(clr);
                set_root(other.get_root());
                get_size() = other.get_size();
                other.set_root(nullptr);
                other.get_size() = 0;
            }

            template<typename Tag>
            void copy_from(const radix_tree &other, Tag clr) {
                assert(m_header && other.m_header);
                node_type *cpy = other.empty() ? nullptr :
                    copy_or_piecewise_move(
                        const_cast<node_type *>(other.get_root()),
                        std::false_type());
                clear_if(clr);
                set_root(cpy);
                get_size() = other.size();
            }

            template<typename Tag>
            void piecewise_move_from(radix_tree &other, Tag clr) {
                assert(m_header && other.m_header);
                node_type *cpy = other.empty() ? nullptr :
                    copy_or_piecewise_move(other.get_root(), std::true_type());
                clear_if(clr);
                set_root(cpy);
                get_size() = other.size();
            }

            template<typename Tag>
            node_type *copy_or_piecewise_move(node_type *tree, Tag piecewise_move) {
                assert(tree);
                node_type *cpy = tree->is_leaf() ?
                    copy_or_move_value(tree, piecewise_move) :
                    new_internal_node(tree->m_key);
                cpy->set_depth(tree->depth());
                if (!tree->is_leaf()) {
                    for (const auto &p : tree->children()) {
                        assert(p.second);
                        node_type *child = copy_or_piecewise_move(p.second, piecewise_move);
                        cpy->children().emplace(p.first, child);
                        child->m_parent = cpy;
                    }
                }
                return cpy;
            }

            // Move value
            node_type *copy_or_move_value(node_type *node, std::true_type) {
                assert(node->is_leaf());
                return new_leaf_node(std::move(node->m_key),
                    std::move(node->value()));
            }

            // Copy value
            node_type *copy_or_move_value(const node_type *node, std::false_type) {
                assert(node->is_leaf());
                return new_leaf_node(node->m_key, node->value());
            }

            static node_type *get_pointer(const_iterator it) noexcept {
                return const_cast<node_type *>(it.m_pointee);
            }

            static iterator downcast_iterator(const_iterator it) noexcept {
                return iterator(get_pointer(it));
            }

            template<typename OutIt, typename Tag>
            OutIt prefix_match_dispatch(const key_type &key, OutIt dest, Tag tag) const {
                if (empty())
                    return dest;

                const node_type *node = find_node(key, get_root(), 0);
                if (node->is_leaf())
                    node = node->m_parent;

                int len = radix_length(key) - node->depth();
                key_type key_sub1 = radix_substr(key, node->depth(), len);
                key_type key_sub2 = radix_substr(node->m_key, 0, len);
                if (key_sub1 != key_sub2)
                    return dest;

                return greedy_match_impl(node, dest, tag);
            }

            template<typename OutIt, typename Tag>
            OutIt greedy_match_dispatch(const key_type &key, OutIt dest, Tag tag) const {
                if (empty())
                    return dest;

                const node_type *node = find_node(key, get_root(), 0);
                if (node->is_leaf())
                    node = node->m_parent;
                return greedy_match_impl(node, dest, tag);
            }

            size_type erase_node(node_type *child) {
                assert(child);
                if (!child->is_leaf())
                    return 0;

                key_type nul = radix_substr(traits::get_key(child->value()), 0, 0);
                node_type *parent = child->m_parent, *grandparent = nullptr;
                parent->children().erase(nul);
                delete_tree(child);
                --get_size();

                if (parent == get_root() || parent->children().size() > 1)
                    return 1;

                if (parent->children().empty()) {
                    grandparent = parent->m_parent;
                    grandparent->children().erase(parent->m_key);
                    delete_tree(parent);
                } else {
                    grandparent = parent;
                }

                if (grandparent == get_root()) {
                    return 1;
                }

                if (grandparent->children().size() == 1) {
                    // merge grandparent with the uncle
                    auto it = grandparent->children().begin();

                    node_type *uncle = it->second;

                    if (uncle->is_leaf())
                        return 1;

                    uncle->set_depth(grandparent->depth());
                    uncle->m_key = radix_join(grandparent->m_key, uncle->m_key);
                    uncle->m_parent = grandparent->m_parent;

                    grandparent->children().erase(it);

                    grandparent->m_parent->children().erase(grandparent->m_key);
                    grandparent->m_parent->children()[uncle->m_key] = uncle;

                    delete_tree(grandparent);
                }

                return 1;
            }

            template<typename P>
            std::pair<iterator, bool> insert_at(node_type *node, P &&val) {
                if (node->is_leaf()) {
                    return { iterator(node), false };
                } else if (node == get_root()) {
                    ++get_size();
                    return { iterator(append(get_root(), std::forward<P>(val))), true };
                } else {
                    ++get_size();
                    int len = radix_length(node->m_key);
                    key_type key_sub = radix_substr(traits::get_key(val), node->depth(), len);

                    if (key_equal(key_sub, node->m_key)) {
                        return { iterator(append(node, std::forward<P>(val))), true };
                    } else {
                        return { iterator(prepend(node, std::forward<P>(val))), true };
                    }
                }
            }

            template<typename P>
            std::pair<iterator, bool> insert_nohint(P &&val) {
                assert(m_header);
                if (get_root() == nullptr) {
                    key_type nul = radix_substr(traits::get_key(val), 0, 0);
                    set_root(new_internal_node(nul));
                }

                node_type *node = find_node(traits::get_key(val), get_root(), 0);
                return insert_at(node, std::forward<P>(val));
            }

            template<typename P>
            std::pair<iterator, bool> insert_hint(node_type *hint, P &&val) {
                assert(hint && hint->is_leaf());

                const key_type &node_key = traits::get_key(hint->value());
                for (;;) {
                    assert(hint != get_root());
                    hint = hint->m_parent;  // Move once at least
                    int len = hint->depth() + radix_length(hint->m_key);
                    if (key_equal(radix_substr(node_key, 0, len),
                        radix_substr(traits::get_key(val), 0, len)))
                        break;
                }

                // Now the same as insert_nohint
                node_type *node = find_node(traits::get_key(val), hint, hint->depth()
                    + radix_length(hint->m_key));
                return insert_at(node, std::forward<P>(val));
            }

            const node_type *lower_bound_impl(const key_type &key,
                const node_type *node, int depth) const {
                assert(depth == node->depth() + radix_length(node->m_key));
                if (node->is_leaf())
                    return depth >= radix_length(key) ? node : m_header;

                int key_len = radix_length(key) - depth;

                for (const auto &p : node->children()) {
                    const key_type &child_key = p.first;
                    const node_type *child = p.second;
                    int child_key_len = radix_length(child_key);
                    if (child_key_len <= key_len) {
                        key_type key_sub = radix_substr(key, depth, child_key_len);
                        if (!get_key_compare()(child_key, key_sub)) {
                            // child_key >= key_sub
                            if (get_key_compare()(key_sub, child_key)) {
                                // child_key > key_sub
                                return child->leftmost();
                            } else {
                                // child_key == key_sub
                                const node_type *lb = lower_bound_impl(key, child, 
                                    depth + child_key_len);
                                if (lb != m_header)
                                    return lb;
                            }
                        }
                    } else {
                        if (get_key_compare()(radix_substr(key, depth, key_len), child_key)) {
                            // child_key > key_sub
                            return child->leftmost();
                        }
                    }
                }

                return m_header;
            }

            const node_type *lower_bound_impl(const key_type &key) const {
                return !empty() ? lower_bound_impl(key, get_root(), 0) : m_header;
            }

            const node_type *upper_bound_impl(const key_type &key) const {
                if (empty())
                    return m_header;
                const node_type *lb = lower_bound_impl(key, get_root(), 0); // lb >= key
                if (lb == m_header || get_key_compare()(key, traits::get_key(lb->value()))) {  
                    return lb;
                } else {    
                    return lb->next();
                }
            }

            const std::pair<const node_type *, const node_type *> 
                equal_range_impl(const key_type &key) const {
                if (empty())
                    return { m_header, m_header };
                const node_type *lb = lower_bound_impl(key, get_root(), 0); // lb >= key
                if (lb == m_header || get_key_compare()(key, traits::get_key(lb->value()))) {  
                    return { lb, lb };
                } else {    
                    return { lb, lb->next() };
                }
            }

            bool key_equal(const key_type &x, const key_type &y) const {
                return !get_key_compare()(x, y) && !get_key_compare()(y, x);
            }

            node_allocator &get_node_allocator() noexcept { return *this; }

            const node_allocator &get_node_allocator() const noexcept { return *this; }

            key_compare &get_key_compare() noexcept { return *this; }

            const key_compare &get_key_compare() const noexcept { return *this; }

            key_element_equal &get_key_element_equal() noexcept { return *this; }

            const key_element_equal &get_key_element_equal() const noexcept { return *this; }

            const node_type *get_root() const noexcept { return m_header->tree_root(); }

            node_type *get_root() noexcept { return m_header->tree_root(); }

            void set_root(node_type *root) noexcept {
                m_header->tree_root() = root;
                if (root)
                    root->m_parent = m_header;
            }

            size_type get_size() const noexcept { return m_header->tree_size(); }

            size_type &get_size() noexcept { return m_header->tree_size(); }

            node_type *m_header;
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

    template<typename K, typename T, typename Compare = std::less<K>,
        typename Equal = std::equal_to<typename detail::key_element<K>::type>,
        typename Alloc = std::allocator<std::pair<const K, T> > >
        using radix_map = detail::radix_tree<K, T, Compare, Equal, Alloc>;

    template<typename K, typename Compare = std::less<K>,
        typename Equal = std::equal_to<typename detail::key_element<K>::type>,
        typename Alloc = std::allocator<K> >
        using radix_set = detail::radix_tree<K, void, Compare, Equal, Alloc>;

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
