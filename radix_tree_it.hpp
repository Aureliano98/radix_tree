#ifndef RADIX_TREE_IT
#define RADIX_TREE_IT

#include <iterator>
#include <functional>

#include "radix_tree_util.hpp"

namespace radix {
    namespace detail {
        // forward declaration
        template <typename K, typename T, typename Compare,
            typename Equal, typename Alloc> class radix_tree;

        template<typename Traits> class radix_tree_node;

        template<typename Traits>
        class radix_tree_const_it : public std::iterator<
            std::bidirectional_iterator_tag, typename Traits::value_type, 
            typename Traits::difference_type, typename Traits::const_pointer, 
            typename Traits::const_reference> {
            typedef std::iterator<
                std::bidirectional_iterator_tag, typename Traits::value_type,
                typename Traits::difference_type, typename Traits::const_pointer,
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

            radix_tree_const_it() noexcept : m_pointee(nullptr) { }

            radix_tree_const_it(const radix_tree_const_it &r) noexcept : 
                m_pointee(r.m_pointee) { }
            
            radix_tree_const_it &operator=(const radix_tree_const_it &r) noexcept { 
                m_pointee = r.m_pointee; 
                return *this; 
            }
            
            reference operator*() const NOEXCEPT_IF_NDEBUG { return m_pointee->value(); }
            
            pointer operator->() const NOEXCEPT_IF_NDEBUG { return &m_pointee->value(); }

            radix_tree_const_it &operator++() {
                // it is undefined behaviour to dereference iterator 
                // that is out of bounds...
                assert(m_pointee && m_pointee->m_parent);
                m_pointee = m_pointee->next();
                return *this;
            }

            radix_tree_const_it operator++(int) {
                radix_tree_const_it copy(*this);
                ++(*this);
                return copy;
            }

            radix_tree_const_it &operator--() {
                // it is undefined behaviour to dereference iterator 
                // that is out of bounds...
                assert(m_pointee);
                m_pointee = m_pointee->prev();
                return *this;
            }

            radix_tree_const_it operator--(int) {
                radix_tree_const_it copy(*this);
                --(*this);
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

            radix_tree_it &operator--() {
                base::operator--();
                return *this;
            }

            radix_tree_it operator--(int) {
                radix_tree_it copy = *this;
                --(*this);
                return copy;
            }

            reference operator*() const NOEXCEPT_IF_NDEBUG {
                return const_cast<reference>(base::operator*());
            }

            pointer operator->() const NOEXCEPT_IF_NDEBUG {
                return const_cast<pointer>(base::operator->());
            }

        private:
            radix_tree_it(node_type *node) noexcept : base(node) {}
        };

    }
}

#endif // RADIX_TREE_IT
