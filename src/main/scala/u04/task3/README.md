# Task3

```text
type: 
    Monad[M[_]]

constructors: 
        unit: A => M[A]
    
operations: 
    flatMap: M[A] x (A => M[B]) => M[B]
    map: M[A] x (A => B) => M[B]
    map2: M[A] x M[B] x (A, B) => M[C]
    seq: M[A] x M[B] => M[B]
    seqN: Stream[M[A]] => M[[A]]
    
axioms:
    flatMap(unit(a), f) = f(a)
    flatMap(m, a => unit(a)) = m
    flatMap(flatMap(m, f), g) = flatMap(m, a => flatMap(f(a), g))
    
    map(unit(a), f) = unit(f(a))
    map(m, x => x) = m   
    map(m, f) = flatMap(m, a => unit(f(a)))
    
    map2(unit(a), unit(b), f) = unit(f(a, b)) 
    map2(m1, m2, (a, b) => b) = seq(m1, m2)   
    
    seqN(Stream(m)) = m
    seqN(Stream(m, ms)) = seq(m, seqN(Stream(ms)))
```
