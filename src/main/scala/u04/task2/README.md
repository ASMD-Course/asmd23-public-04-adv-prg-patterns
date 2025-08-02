# Task 2

Original ADT for Sequences:
```text
type :
    Sequence[A]
    
constructors :
    empty: Sequence[A]
    apply: A x A x ... x A => Sequence[A]
 
operations :
    add: A x Sequence[A] => Sequence[A]
    concat: Sequence[A] x Sequence[A] => Sequence[A]
    map [B]: Sequence [A] x (A => B) => Sequence [B]
    flatMap [B]: Sequence [A] x (A => Sequence [B]) => Sequence [B]
    filter: List [A] x (A => Boolean) => Sequence [A]
    foldLeft [B]: List [A] x B x (B, A) => B
    
axioms :

    add(empty, x) = apply(x)
    add(apply(xs), x) = apply(xs, x)
    
    concat(empty, ys) = ys
    concat(xs, ys) = apply(xs, ys)
    
    map(empty , f) = nil
    map(apply(xs), i => i) = apply(xs, f)
    map(apply(x, y), f) = apply(f(x), f(y))
    
    flatMap(empty, f) = nil
    flatMap(apply(x, y), f) = f(x) ++ f(y)
    
    filter(empty, f) = empty
    
    filter(apply(xs), f) = apply(filter(xs, f))
    
    foldLeft(empty, b, f) = b
    foldLeft(apply(x, xs), b, f) = foldLeft(xs, f(b, x), f)
    
  
```
After asking to a LLM:

```text
type :
    Sequence[A]
    
constructors :
    empty: Sequence[A]
    apply: A x A x ... x A => Sequence[A]
 
operations :
    add: A x Sequence[A] => Sequence[A]
    concat: Sequence[A] x Sequence[A] => Sequence[A]
    map [B]: Sequence [A] x (A => B) => Sequence [B]
    flatMap [B]: Sequence [A] x (A => Sequence [B]) => Sequence [B]
    filter: List [A] x (A => Boolean) => Sequence [A]
    foldLeft [B]: List [A] x B x (B, A) => B
    
axioms :

    add(empty, x) = apply(x)
    add(apply(xs), x) = apply(xs, x)
    
    concat(empty, ys) = ys
    concat(xs, ys) = apply(xs, ys)
    
    map(empty , f) = nil
    map(apply(xs), i => i) = apply(xs, f)
    map(apply(x, y), f) = apply(f(x), f(y))
    
    flatMap(empty, f) = nil
    flatMap(apply(x), f) = f(x)
    flatMap(apply(x1, x2, ..., xn), f) = concat(f(x1), flatMap(apply(x2, ..., xn), f))
    flatMap(apply(x, y), f) = f(x) ++ f(y)
    
    filter(empty, f) = empty
    filter(apply(x1, x2, ..., xn), p) = concat(filter(apply(x1), p), filter(apply(x2, ..., xn), p))
    filter(apply(xs), f) = apply(filter(xs, f))
    
    foldLeft(empty, b, f) = b
    foldLeft(apply(x, xs), b, f) = foldLeft(xs, f(b, x), f)
    foldLeft(concat(xs, ys), b, f) = foldLeft(ys, foldLeft(xs, b, f), f)
 
```

