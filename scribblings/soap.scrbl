#lang scribble/manual
@require[@for-label[soap]]

@title{soap}
@author{"Jörgen Brandt"}

@defmodule[soap]

@section{Auxiliaries}

@subsection{Qualified Name}

@defstruct[qname ([prefix Symbol] [name Symbol]) #:omit-constructor]{}

@defproc[(qname->string (qn qname)) String]{}

@defproc[(qname->symbol (qn qname)) Symbol]{}

@subsection{X-Expression Constructor}

@defproc[(make-xml-element (name qname) (att-list (Listof (Pairof Symbol String))) (body (Listof XExpr))) XExpr]{}

@defform[(with-prefix ([x s] ...) r ...)
         #:grammar
         [(x id)]
         #:contracts ([s String]
                      [r Any])]{}

@defform[(clear-prefix r ...)
         #:contracts ([r Any])]{}

@defform[(set-prefix prefix-context r ...)
         #:contracts ([prefix-context (Listof (Pairof Symbol String))])]{}