# hpio

`hpio` provides support for writing GPIO programs in Haskell. It
includes an embedded DSL for writing platform-independent programs,
along with low-level monads and IO functions which provide direct
access to each supported platform's native GPIO API.

Currently only the Linux `sysfs` GPIO filesystem is supported, but
support for other Unix GPIO platforms is planned.

For details on usage, see the included tutorial module, or the
`examples` directory in the source distribution.

[![Travis CI build status](https://travis-ci.org/dhess/hpio.svg?branch=master)](https://travis-ci.org/dhess/hpio)
