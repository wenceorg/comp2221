-- This code is not Haskell, but it might look reasonably similar to
-- you. This uses the Lean theorem prover. In particular, the
-- community-maintained version 3
-- https://leanprover-community.github.io

-- I don't expect you to know any lean really after this. But
-- hopefully it was interesting in terms of seeing what can be done on
-- computers in a programming language.
-- If you're interested, Kevin Buzzard writes a nice blog talking
-- about formalising the Imperial undergrad maths curriculum in Lean:
-- https://xenaproject.wordpress.com

-- Some type variables
variables {α : Type*} {β : Type*} {γ : Type*}

-- Let the simplifier know about this definition
-- Heres the standard definition for map on lists that we've seen
-- previously in Haskell.
-- We're going to prove that this definition obeys the two Functor laws.
@[simp]
def map (f : α -> β) : list α -> list β
  | [] := []
  | (h::t) := f h :: map t

-- Functor laws

-- In general these kinds of propositions can't be automatically
-- proven (due to Rice's theorem on undecidability).
-- So we must manually construct a proof as a witness.

-- It is helpful to do so in a system with dependent types (a more
-- powerful type system than Haskell has)
-- A downside is that we have to do more manual type annotation (since
-- types cannot be inferred as easily).
-- 
-- Notice how we no longer have a distinction between functions and
-- data type constructors (naming-wise). This is not just syntax,
-- there's really no difference in lean between a function between
-- values and a function between types.


-- 1st law. map id xs == id xs for all xs
-- The type of this lemma is the statement
-- forall xs, map id xs = id xs
-- Notice how we can refer to values in the type
-- Haskell doesn't give us a way of stating this lemma at the type
-- level, so it can't be type checked.
lemma map_id : ∀ (xs : list α), map id xs = id xs :=
begin
  -- The body of the function is a proof of the type
  -- Give myself a list.
  intro xs,
  -- We'll do induction
  induction xs with head tail hypothesis,
  -- Proving the base case is simple enough that the reflexivity
  -- "tactic" will do (https://en.wikipedia.org/wiki/Reflexive_relation)
  { reflexivity, },
  -- For the inductive case, we rewrite the map (to apply it once)
  -- Then we can use our induction hypothesis on the tail
  -- And reflexivity will do the trick again.
  { rw map,
    rw hypothesis,
    reflexivity, },
end

-- 2nd law. map (f . g) xs == map f (map g xs) for all f, g, xs
lemma map_composition : ∀ (xs : list α) (f : β-> γ) (g : α -> β),
  map (f ∘ g) xs = map f (map g xs) :=
begin
  -- Now we have a bunch of foralls, so get handles on all of them.
  intros,
  -- Again by induction
  induction xs with head tail hypothesis,
  -- Base case is easy in the same way
  { reflexivity, },
  -- Inductive case we just need to rewrite the definition of map a
  -- few times before being in a form where the induction hypothesis
  -- can be applied.
  { repeat {rw map, },
    rw hypothesis, },
end

-- Fancier, lean has a "simplification" tactic. 
lemma map_id' (xs : list α) : map id xs = id xs :=
by induction xs; simp *

lemma map_composition' (xs : list α) (f : β -> γ) (g : α -> β) :
  map (f ∘ g) xs = map f (map g xs) :=
by induction xs; simp *

-- So now the idea is that we make type-level statements about our
-- program. Then the computer can help us prove the statement, and
-- verify the proof. We've pushed the difficulty (in some senses) onto
-- coming up with the correct statement to prove.
