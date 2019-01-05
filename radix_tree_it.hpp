#ifndef RADIX_TREE_IT
#define RADIX_TREE_IT

#include <iterator>
#include <functional>

namespace radix {

    // forward declaration
    template <typename K, typename T, typename Compare, 
        typename Equal, typename Alloc> class radix_tree;

    namespace detail {
        
        template<typename Traits> class radix_tree_node;

        template<typename Traits>
        class radix_tree_const_it : public std::iterator<
            std::forward_iterator_tag, typename Traits::value_type, 
            std::ptrdiff_t, typename Traits::const_pointer, 
            typename Traits::const_reference> {
            typedef std::iterator<
                std::forward_iterator_tag, typename Traits::value_type,
                std::ptrdiff_t, typename Traits::const_pointer,
                typename Traits::const_reference> base;

        protected:
            typedef radix_tree_node<Traits> node_type;

        public:
            friend typename Traits::tree_type;
            
            typedef typename base::iterator_category iterator_category;
            typedef typename base::value_type value_type;
            typedef typename base::reference reference;
            typedef typename base::pointer pointer;
            typedef typename base::difference_type difference_type;

            radix_tree_const_it() noexcept : m_pointee(0) { }

            radix_tree_const_it(const radix_tree_const_it &r) noexcept : 
                m_pointee(r.m_pointee) { }
            
            radix_tree_const_it &operator=(const radix_tree_const_it &r) noexcept { 
                m_pointee = r.m_pointee; 
                return *this; 
            }
            
            ~radix_tree_const_it() { }

            reference operator*() const { return m_pointee->get_value(); }
            
            pointer operator->() const { return &m_pointee->get_value(); }

            radix_tree_const_it &operator++() {
                // it is undefined behaviour to dereference iterator 
                // that is out of bounds...
                if (m_pointee != nullptr) 
                    m_pointee = increment(m_pointee);
                return *this;
            }

            radix_tree_const_it operator++(int) {
                radix_tree_const_it copy(*this);
                ++(*this);
                return copy;
            }

            bool operator!=(const radix_tree_const_it &rhs) const noexcept {
                return m_pointee != rhs.m_pointee;
            }

            bool operator==(const radix_tree_const_it &rhs) const noexcept {
                return m_pointee == rhs.m_pointee;
            }

        protected:
            radix_tree_const_it(const node_type *p) noexcept : m_pointee(p) { }

            const node_type *increment(const node_type *node) const {
                const node_type *parent = node->m_parent;
                if (parent == nullptr)
                    return nullptr;

                typename node_type::map_const_iterator it
                    = parent->m_children.find(node->m_key);
                assert(it != parent->m_children.end());
                ++it;
                if (it == parent->m_children.end())
                    return increment(parent);
                else
                    return descend(it->second);
            }

            const node_type *descend(const node_type *node) const {
                while (!node->m_is_leaf) {
                    typename node_type::map_const_iterator it
                        = node->m_children.cbegin();
                    assert(it != node->m_children.cend());
                    node = it->second;
                }
                return node;
            }

            const node_type *m_pointee;
        };

        template<typename Traits>
        class radix_tree_it : public radix_tree_const_it<Traits> {
            typedef radix_tree_const_it<Traits> base;
            typedef typename base::node_type node_type;

        public:
            friend typename Traits::tree_type;

            typedef typename base::iterator_category iterator_category;
            typedef typename Traits::value_type value_type;
            typedef typename Traits::reference reference;
            typedef typename Traits::pointer pointer;
            typedef typename base::difference_type difference_type;

            radix_tree_it() noexcept : base() {}

            radix_tree_it(const radix_tree_it &it) noexcept : base(it) {}

            radix_tree_it &operator++() {
                base::operator++();
                return *this;
            }

            radix_tree_it operator++(int) {
                radix_tree_it copy = *this;
                ++(*this);
                return copy;
            }

            reference operator*() const {
                return const_cast<reference>(base::operator*());
            }

            pointer operator->() const {
                return const_cast<pointer>(base::operator->());
            }

        private:
            radix_tree_it(node_type *node) noexcept : base(node) {}
        };

    }
}

#endif // RADIX_TREE_IT
