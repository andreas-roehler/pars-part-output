# pars-part-output
List the result of calling ‘parse-partial-sexp’ resp. ‘syntax-ppss’
alongside with the explanation from its docu.

First column displays the position in resulting list.
For example

0  depth in parens.

	 ====> 1 <====

might relate to a result of
`(nth 0 (parse-partial-sexp (point-min) (point)))`



