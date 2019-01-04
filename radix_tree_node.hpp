#ifndef RADIX_TREE_NODE_HPP
#define RADIX_TREE_NODE_HPP

#include <cassert>
#include <map>
#include <functional>

namespace radix {

    template<typename K, typename T, typename Compare, typename Alloc>
    class radix_tree;

    namespace detail {

        template<typename Traits>
        class radix_tree_node {
            friend typename Traits::tree_type;
            friend class radix_tree_const_it<Traits>;

            typedef radix_tree_node self;

            typedef typename Traits::value_type value_type;
            typedef typename Traits::key_type key_type;
            typedef typename Traits::key_compare key_compare;
            typedef typename Traits::allocator_type allocator_type;

            typedef typename std::allocator_traits<allocator_type>::
                template rebind_alloc<std::pair<const key_type, self *>> map_allocator;
            typedef std::map<key_type, self *, key_compare, map_allocator> map_type; 
            typedef typename map_type::iterator map_iterator;
            typedef typename map_type::const_iterator map_const_iterator;

            radix_tree_node(const key_compare &pred, const map_allocator &alloc) :
                m_children(pred, alloc),
                m_parent(NULL), m_is_leaf(false), m_holds_value(false), m_depth(0), m_key() { }

            radix_tree_node(const value_type &val, const key_compare &pred, 
                const map_allocator &alloc) : 
                m_children(pred, alloc),
                m_parent(NULL),
                m_is_leaf(false),
                m_holds_value(true),
                m_depth(0),
                m_key() {
                ::new (reinterpret_cast<value_type *>(m_value)) value_type(val);
            }

            ~radix_tree_node() {
                if (m_holds_value) 
                    reinterpret_cast<value_type *>(m_value)->~value_type();
            }
            
            radix_tree_node(const radix_tree_node &) = delete;
            radix_tree_node &operator=(const radix_tree_node &) = delete;

            value_type &get_value() { 
                assert(m_holds_value);
                return *reinterpret_cast<value_type *>(m_value); 
            }
            
            const value_type &get_value() const { 
                assert(m_holds_value); 
                return *reinterpret_cast<const value_type *>(m_value); 
            }

            map_type m_children;
            self *m_parent;
            char m_value[sizeof(value_type)];
            bool m_is_leaf;
            bool m_holds_value;
            int m_depth;
            key_type m_key;
        };

    }
}

#endif // RADIX_TREE_NODE_HPP
