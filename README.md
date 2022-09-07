# Tutorial for haskells servant

For reference, here’s a list of some combinators from servant:

- Delete, Get, Patch, Post, Put: these do not become arguments. They provide the return type of handlers, which usually is Handler <something>.
- Capture "something" a becomes an argument of type a.
- QueryParam "something" a, Header "something" a all become arguments of type Maybe a, because there might be no value at all specified by the client for these.
- QueryFlag "something" gets turned into an argument of type Bool.
- QueryParams "something" a gets turned into an argument of type [a].
- ReqBody contentTypes a gets turned into an argument of type a.

