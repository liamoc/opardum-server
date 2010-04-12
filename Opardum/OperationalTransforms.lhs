% Opardum
% A Collaborative Code Editor
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\ignore{

> {-# LANGUAGE PatternGuards #-}

}
\title{Opardum: Operational Transforms}
\maketitle

\section{Introduction}

Opardum makes use of a type of patch calculus that is also used in other collaborative editors
such as Google Wave and Etherpad. The system is known as Operational Transforms or OT. The basic
idea of OT is to transform (or adjust) the parameters of an editing operation according to the
effects of previously executed concurrent operations so that the transformed operation can achieve
the correct effect and maintain document consistency.

This transformation is performed by both the client and server. Client and server states are allowed
to diverge, but they will always become unified with transformation once operations have been
transmitted across the wire. Details of this transformation are provided in section 4.

The Operational Transform semantics are pairwise, so each transformation is only concerned with
unifying only one client and the server.

> module Opardum.OperationalTransforms where
>
> import Data.List(isPrefixOf)
> import Control.Applicative((<$>))

\section{Operations}

We have defined an operation to be a list of components that, taken together, specify a map of
actions to perform on some document.

> type Op = [OpComponent]

The location in the document to perform an action is taken in the context of the whole op. Hence
the |Retain| component that specifies that the next $n$ characters remain unchanged.

> data OpComponent = Insert String
>                  | Delete String
>                  | Retain Int
>                  deriving (Show, Eq)


\section {Composition}

Next we have the composition relation, |+>|, which is an associative but not commutative relation.
The relation is defined such that for some operation $a$ and another $b$, applying $a$|+>|$b$ to
a document should be equivalent to applying $a$ to the document and then applying $b$ separately.
Similarly, applying $a$|<+|$b$ should be the same as applying $b$ then $a$.

This relation is used on the server for storage. Rather than store the document in the |Storage|
as a sequence of |Op|s (which is rather inefficient in terms of space), we compose all |Op|s together
into a running `snapshot' as they arrive. More information is in the @ConcurrencyControl@ module.

We include in the contract for the relation the idea that an invalid operation will not compose
with a valid operation, and hence this running snapshot is also used for error checking.

\subsection {Implementation}

First we define one operation in terms of its reverse, to save typing.

> (+>) :: Op -> Op -> Maybe Op
> (+>) = flip (<+)

Now for the actual meat of the algorithm. The algorithm functions somewhat like the well known
functional list operation |zip|, except that, seeing as each component of the first op does
not necessarily line up with the components of the second |Op|, components must be split into
several components in order for the |Op|s to zip cleanly together.

\begin{center}
\includegraphics[width=105mm]{resources/zip.pdf}
\end{center}

First we shall define some useful helper functions used here and in the operational transform
function itself.

\subsubsection{Helper Functions}

First we define functions for use in Pattern Guards to identify the type of a component.

> delete :: OpComponent -> Bool
> delete (Delete _) = True
> delete _          = False
> insert (Insert _) = True
> insert _          = False
> retain (Retain _) = True
> retain _          = False

Next, we define a function |oplength| to determine the amount of characters affected by a
component. This is used to determine how the components should be split.

> oplength :: OpComponent -> Int
> oplength (Insert v) = length v
> oplength (Delete v) = length v
> oplength (Retain n) = n

Next, we define a |sub| function that produces the second half of the op, split at $n$
characters.

> sub :: Int -> OpComponent -> OpComponent
> sub n (Delete str) = Delete $ drop n str
> sub n (Retain n')  = Retain $ n' - n
> sub n (Insert str) = Insert $ drop n str

And of course, the opposite |first| function that produces the first half, split at $n$
characters.

> first :: Int -> OpComponent -> OpComponent
> first n (Insert str) = Insert $ take n str
> first n (Delete str) = Delete $ take n str
> first n (Retain num) = Retain $ n

The |normalize| function is used to reconnect contiguous regions of operation components,
as the zip operations performed by composition and transformation can cause unnecessary
breaks.

> normalize :: Op -> Op
> normalize ((Insert str):(Insert str'):rest) = normalize ((Insert $ str ++ str'):rest)
> normalize ((Delete str):(Delete str'):rest) = normalize ((Delete $ str ++ str'):rest)
> normalize ((Retain num):(Retain num'):rest) = normalize ((Retain $ num +  num'):rest)
> normalize ((Retain 0):rest)                 = normalize rest
> normalize ((Delete ""):rest)                = normalize rest
> normalize ((Insert ""):rest)                = normalize rest
> normalize (op1:rest)                        = op1:(normalize rest)
> normalize []                                = []

\subsubsection{Composition Relation}

> (<+) :: Op -> Op -> Maybe Op
> (<+) = compose

The obvious definitions come first. Any operation $o$ applied to the no-op operation |[]| is just
$o$. However, |[]| applied to any operation $o'$ is not just $o'$, as the operation we are applying
is required to specify |Retain| actions up until the end of the operation it is meant to apply
to. This is enforced in composition for error checking.

> compose o  [] = Just o

For the rest of this function, we will consider the relation to be applying the operation $s_1$ to
the operation $s_2$. The operation components closest to the zip are called $o_1$ and $o_2$ and
the rest of the operation components in the ops are called $o_1s$ and $o_2s$ respectively.


Next, we define that if $o_1$ is an |Insert|, we simply emit $o_1$ through the |zip|, because $o_2$
cannot be affected in any way by an insertion later in history.

> compose (op1:op1s) op2s | insert op1 = (op1:) <$> (compose op1s op2s)

Similarly, a |Delete| in $o_2$ is passed straight through, as the $o_1$ operations cannot be
affected by a deletion earlier in history.

> compose op1s (op2:op2s) | delete op2 = (op2:) <$> (compose op1s op2s)

Any combination of |Delete|s or |Retain|s in $s_1$ and |Insert|s or |Retain|s in $s_2$ does not
result in a simple-pass through result, as these operations have some precedence (Future |Delete|s
alter past |Insert|s, and all actions override |Retain|s). Therefore a `split' may be required in
the stream to ensure that they zip cleanly. Therefore, we compare the length of each op,
and split either op depending on which one is larger. We also check that the text in |Insert| and
|Delete| components match, to prevent invalid ops from corrupting the composition.

> compose (op1:op1s) (op2:op2s)
>         | delete op1, retain op2 = overrideRetain
>         | retain op1, retain op2 = overrideRetain
>         | delete op1, insert op2, textOk = case compare op1Length op2Length of
>                                               EQ -> eq
>                                               LT -> lt
>                                               GT -> gt
>         | retain op1, insert op2 = case compare op1Length op2Length of
>                                       EQ -> (op2:) <$> eq
>                                       LT -> (first op1Length op2:) <$> lt
>                                       GT -> (op2:) <$> gt
>         where overrideRetain = case compare op1Length op2Length of
>                                   EQ -> (op1:) <$> eq
>                                   LT -> (op1:) <$> lt
>                                   GT -> (first op2Length op1:) <$>gt
>               eq = compose op1s op2s
>               lt = compose op1s (sub (oplength op1) op2:op2s)
>               gt = compose (sub (oplength op2) op1:op1s) op2s
>               op1Length = oplength op1
>               op2Length = oplength op2
>               textOk = text op1 `isPrefixOf` text op2 ||
>                        text op2 `isPrefixOf` text op1
>               text (Insert t) = t
>               text (Delete t) = t
> compose _ _ = Nothing

\section{Transform Function}

\emph{Note:} Code marked with a |%| is not included in the actual code of this module.
\medskip

When an operation hits the server from a client, the server may have |Op|s added to the |Document|
in state that the client was not aware of when it sent its |Op|s. If we simply applied the
operations as-is to the document, the operations would not do what we expect.

For example, for this document:

\begin{code}%
D1 = [[Insert "Foo Bar Baz"]]
\end{code}

If the client wanted to insert the word ``Bop'' after the world ``Bar'', it would make the
following op:

\begin{code}%
C1 = [Retain 8, Insert "Bop", Retain 3]
\end{code}

But, suppose that the server had already received the following op, that inserted the word ``Quo'':

\begin{code}%
S1 = [Insert "Quo", Retain 11]
\end{code}

Then, when our first client op $C_1$ reaches the server, the (composed) document looks different
to what the client expected:

\begin{code}%
D2 = [[Insert "Quo Foo Bar Baz"]]
\end{code}

And so, if we attempt to apply the client op to the server's document, the word ``Bop'' is inserted
after ``Foo'', not after ``Bar'' as expected.

So, we must \emph{transform} $C_1$ against the op $S_1$, producing two ops, $C_1'$, and $S_1'$. $C_1'$
is an op that achieves the desired effect of $C_1$ when applied to the document that already has $S_1$
--- i.e the server document.
$S_1'$ is an op that, when applied to the document that already has $C_1$ (i.e the client document)
will achieve the desired effect of $S_1$.

In this way, transformation causes two diverging document states to converge again.

\begin{center}
\includegraphics[width=90mm]{resources/transform.pdf}
\end{center}

Once again, like composition, the transformation function is implemented as a type of |zip| that
`splits' operations that don't |zip| cleanly. It calls |normalize| and the splitting helper
functions defined in section 3.

> t :: (Op, Op) -> (Op, Op)
> t (s, c) = let (a,b) = unzip $ t' s c
>             in (normalize a, normalize b)
>   where
>     t' (op:ops) (ag:ags)

Two retains will split the zip if they are not of equal size.

>        | all retain [op,ag] = let adv = min (oplength op) (oplength ag)
>                                in (Retain adv, Retain adv):subtraction

An insertion in one op produces a retain in the other op, so that the insertion is preserved.

>        | insert op = (op, Retain $ oplength op):(t' ops $ ag:ags)
>        | insert ag = (Retain $ oplength ag, ag):(t' (op:ops) ags)

A deletion in one op causes the other action (which cannot be an insert due to the previous case)
to be shortened or removed. This causes a split in the zip unless the two ops are of equal size.

>        | delete op = (op, Retain 0):subtraction
>        | delete ag = (Retain 0, ag):subtraction
>        where
>          subtraction = let [l_op, l_ag] = map oplength [op, ag]
>                         in case compare l_op l_ag of
>                              EQ -> t' ops ags
>                              LT -> t' ops $ sub l_op ag:ags
>                              GT -> t' (sub l_ag op:ops) ags

|Retain| actions are inserted to cover any ops that are remaining. Note that this will produce
|Retain| actions for |Delete| actions in the other op, which is an error -- but validation via
composition can find this error, seeing as the |Delete| action is not valid because there is
no operation to subtract from, hence the entire op is invalid and would be rejected by the
composition.

>     t' (op:ops) [] = (op, Retain $ oplength op):(t' ops [])
>     t' [] (ag:ags) = (Retain $ oplength ag, ag):(t' [] ags)
>     t' [] [] = []

\end{document}
