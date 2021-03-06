How do we implement references and pointers in languages that do not provide them?

In order to implement a machinery for dynamically allocating references, we notice that on can represent a collection of similar values (e.g., of type `int` or `string`) by packaging them into arrays, so such arrays will play the role of random-access memory. For instance, two consecutive nodes with the payloads `(15, "a")` and `(42, "b")` of a double-linked list containing pairs of integers can be encoded by sub-segments of following three arrays: one for pointer “addresses”, one for integers, and one for strings.

A list “node” (`dll_node`) is simply a segment of four consecutive entries in a pointer array, with the corresponding links to an integer and a string part of the payload. Therefore, in order to work with a double-linked list represented via three arrays, one should manipulate with the encoding of references in by means of changing the contents of those arrays.
