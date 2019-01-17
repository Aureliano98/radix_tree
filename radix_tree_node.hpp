#ifndef RADIX_TREE_NODE_HPP
#define RADIX_TREE_NODE_HPP

#include <cassert>
#include <map>
#include <functional>

namespace radix {
    namespace detail {

        template<typename K, typename T, typename Compare,
            typename Equal, typename Alloc> class radix_tree;

        template<typename Traits>
        class radix_tree_node {
            friend typename Traits::tree_type;
            friend class radix_tree_const_it<Traits>;

            typedef radix_tree_node self;

            typedef typename Traits::value_type value_type;
            typedef typename Traits::key_type key_type;
            typedef typename Traits::key_compare key_compare;
            typedef typename Traits::allocator_type allocator_type;
            typedef typename Traits::size_type size_type;

            typedef typename std::allocator_traits<allocator_type>::
                template rebind_alloc<std::pair<const key_type, self *>> map_allocator;
            typedef std::map<key_type, self *, key_compare, map_allocator> map_type; 
            typedef typename map_type::iterator map_iterator;
            typedef typename map_type::const_iterator map_const_iterator;

            static_assert(sizeof(map_type) >= sizeof(self *),
                "header cannot hold a pointer");

            // Construct as an internal node
            template<typename Key>
            radix_tree_node(Key &&key, const key_compare &pred, 
                const map_allocator &alloc) :
                m_parent(nullptr), 
                m_depth(0), 
                m_key(std::forward<Key>(key)) { 
                ::new (std::addressof(children())) map_type(pred, alloc);
            }

            // Construct as a leaf node
            template<typename Key>
            radix_tree_node(Key &&key) : 
                m_parent(nullptr),
                m_depth(-1),
                m_key(std::forward<Key>(key)) {
            }

            ~radix_tree_node() {
                if (!is_leaf())
                    std::addressof(children())->~map_type();
            }
            
            radix_tree_node(const radix_tree_node &) = delete;
            radix_tree_node &operator=(const radix_tree_node &) = delete;

            value_type &value() NOEXCEPT_IF_NDEBUG {
                assert(is_leaf());
                return *reinterpret_cast<value_type *>(m_buf); 
            }
            
            const value_type &value() const NOEXCEPT_IF_NDEBUG {
                assert(is_leaf()); 
                return *reinterpret_cast<const value_type *>(m_buf); 
            }

            map_type &children() NOEXCEPT_IF_NDEBUG {
                assert(!is_leaf());
                return *reinterpret_cast<map_type *>(m_buf);
            }

            const map_type &children() const NOEXCEPT_IF_NDEBUG {
                assert(!is_leaf());
                return *reinterpret_cast<const map_type *>(m_buf);
            }

            const self *leftmost() const {
                const self *p = this;
                while (!p->is_leaf()) {
                    assert(!p->children().empty());
                    p = p->children().cbegin()->second;
                }
                return p;
            }

            const self *rightmost() const {
                const self *p = this;
                while (!p->is_leaf()) {
                    assert(!p->children().empty());
                    p = p->children().crbegin()->second;
                }
                return p;
            }

            const self *next() const {
                const self *node = this, *parent;
                for (;;) {
                    parent = node->m_parent;
                    assert(parent);
                    if (!parent->m_parent)  // header
                        return parent;
                    auto it = parent->children().find(node->m_key);
                    assert(it != parent->children().cend());
                    ++it;
                    if (it == parent->children().cend())
                        node = parent;
                    else
                        return it->second->leftmost();
                }
            }

            const self *prev() const {
                const self *node = this;
                if (node->m_parent == nullptr)
                    return node->tree_root()->rightmost();

                const self *parent;
                for (;;) {
                    parent = node->m_parent;
                    assert(parent != nullptr);
                    auto it = parent->children().find(node->m_key);
                    assert(it != parent->children().cend());
                    if (it != parent->children().cbegin())
                        return std::prev(it)->second->rightmost();
                    node = parent;
                }
            }

            self *&tree_root() noexcept {
                return reinterpret_cast<self *&>(m_buf);
            }

            const self *tree_root() const noexcept {
                using const_ptr = const self *;
                return reinterpret_cast<const const_ptr &>(m_buf);
            }

            size_type &tree_size() noexcept {
                return reinterpret_cast<size_type &>(m_depth);
            }

            size_type tree_size() const noexcept {
                return reinterpret_cast<const size_type &>(m_depth);
            }

            // Is leaf <=> has value
            // NOTE: the root is never considered a laef.
            bool is_leaf() const noexcept { return m_depth < 0; }

            int depth() const noexcept { return m_depth < 0 ? ~m_depth : m_depth; }

            void set_depth(int depth) noexcept { m_depth = is_leaf() ? ~depth : depth; }
            
            self *m_parent;     // Pointer to parent, nullptr for header
            int m_depth;        // Depth and leaf tag
            key_type m_key;     // Key substr
            char m_buf[sizeof(value_type) > sizeof(map_type) ? 
                sizeof(value_type) : sizeof(map_type)]; // Value (leaf) or children (internal)
        };

    }
}

#endif // RADIX_TREE_NODE_HPP
