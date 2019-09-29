Implementation of loeb & moeb combinators in Scala as described in:

http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html

https://github.com/quchen/articles/blob/master/loeb-moeb.md

https://colourcoding.net/2018/03/29/marvellous-moeb/

`NaiveLoeb` is a straightforward translation from Haskell, hence nothing works (calls fail with stack overflow) since these combinators rely on runtime laziness.
`LoebIO` is rather ugly but it works thanks to cats' `IO`'s lazyness & stack-safety.

Both files also contain loeb's implementation in terms of moeb (in addition to the self-contained one).
