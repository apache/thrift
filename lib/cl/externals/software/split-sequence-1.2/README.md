SPLIT-SEQUENCE
==============

[SPLIT-SEQUENCE](http://cliki.net/split-sequence) is a member of the
[Common Lisp Utilities](http://cliki.net/Common%20Lisp%20Utilities)
family of programs, designed by community consensus.


_Function_ __SPLIT-SEQUENCE, SPLIT-SEQUENCE-IF, SPLIT-SEQUENCE-IF-NOT__


__Syntax:__

__split-sequence__ _delimiter sequence `&key` count
remove-empty-subseqs from-end start end test test-not key ⇒ list,
index_

__split-sequence-if__ _predicate sequence `&key` count
remove-empty-subseqs from-end start end key ⇒ list, index_

__split-sequence-if-not__ _predicate sequence `&key` count
remove-empty-subseqs from-end start end key ⇒ list, index_


__Arguments and Values:__

_delimiter_&mdash;an _object_.

_predicate_&mdash;a designator for a _function_ of one _argument_ that
returns a _generalized boolean_.

_sequence_&mdash;a _proper sequence_.

_count_&mdash;an _integer_ or __nil__. The default is __nil__.

_remove-empty-subseqs_&mdash;a _generalized boolean_. The default is
_false_.

_from-end_&mdash;a _generalized boolean_. The default is _false_.

_start, end_&mdash;_bounding index designators_ of _sequence_. The
defaults for _start_ and _end_ are __0__ and __nil__, respectively.

_test_&mdash;a _designator_ for a _function_ of two _arguments_ that
returns a _generalized boolean_.

_test-not_&mdash;a _designator_ for a _function_ of two _arguments_
that returns a _generalized boolean_.

_key_&mdash;a _designator_ for a _function_ of one _argument_, or
__nil__.

_list_&mdash;a _proper sequence_.

_index_&mdash;an _integer_ greater than or equal to zero, and less
than or equal to the _length_ of the _sequence_.


__Description:__

Splits _sequence_ into a list of subsequences delimited by objects
_satisfying the test_.

_List_ is a list of sequences of the same kind as _sequence_ that has
elements consisting of subsequences of _sequence_ that were delimited
in the argument by elements _satisfying the test_. Index is an index
into _sequence_ indicating the unprocessed region, suitable as an
argument to
[subseq](http://www.lispworks.com/documentation/HyperSpec/Body/f_subseq.htm)
to continue processing in the same manner if desired.

The _count_ argument, if supplied, limits the number of subsequences
in the first return value; if more than _count_ delimited subsequences
exist in _sequence_, the _count_ leftmost delimited subsequences will
be in order in the first return value, and the second return value
will be the index into _sequence_ at which processing stopped.

If _from-end_ is non-null, _sequence_ is conceptually processed from
right to left, accumulating the subsequences in reverse order;
_from-end_ only makes a difference in the case of a non-null _count_
argument. In the presence of _from-end_, the _count_ rightmost
delimited subsequences will be in the order that they are in
_sequence_ in the first return value, and the second is the index
indicating the end of the unprocessed region.

The _start_ and _end_ keyword arguments permit a certain subsequence
of the _sequence_ to be processed without the need for a copying
stage; their use is conceptually equivalent to partitioning the
subsequence delimited by _start_ and _end_, only without the need for
copying.

If _remove-empty-subseqs_ is null (the default), then empty
subsequences will be included in the result.

In all cases, the subsequences in the first return value will be in
the order that they appeared in _sequence_.


__Examples:__

<pre>
SPLIT-SEQUENCE> (split-sequence #\Space "A stitch in time saves nine.")
⇒ ("A" "stitch" "in" "time" "saves" "nine.")
⇒ 28

SPLIT-SEQUENCE> (split-sequence #\, "foo,bar ,baz, foobar , barbaz,")
⇒ ("foo" "bar " "baz" " foobar " " barbaz" "")
⇒ 30
</pre>
