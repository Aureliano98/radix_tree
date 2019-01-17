radix_tree
=====

STL like container of radix tree in C++

Usage
=====
It's a header-only library. Just include it. See [examples](examples/).

Develop
=====
Requirements: c++11 compiler (`g++` or `clang++`), `cmake`

	~/radix_tree $ mkdir build && cd build
	~/radix_tree/build $ cmake -DBUILD_TESTS=On ../
	~/radix_tree/build $ make check

Copyright
=====
See [COPYING](COPYING).

Notes
=====
The repository is forked from [ytakano/radix_tree](https://github.com/ytakano/radix_tree).
The version mainly differs in the following aspects: 

* Custom allocator
* Const iterator and const qualifiers
* Set interface (radix::radix\_set, in addition to radix::radix\_map)
* Originally users need to provide < (or a predicate) and == for key type, == for element of key type; now users only need to provide < (or a predicate) for key type, == (or a predicate) for element of key type. Therefore an additional template parameter is added. 
* Generalized prefix\_match and greedy\_match singatures
* Copy/move constructor/assignment and swap
* Empty base optimization (EBO) and other space optimization
* lower\_bound, upper\_bound, equal\_range
* The code is put into radix namespace
* This version requires c++11 support