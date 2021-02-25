variables {α : Type*} {β : Type*} {γ : Type*}

inductive tree (α : Type*)
| leaf : tree
| node : tree -> α -> tree -> tree

@[simp]
def tree_map (f : α -> β) : tree α -> tree β
| tree.leaf := tree.leaf
| (tree.node l x r) := tree.node (tree_map l) (f x) (tree_map r)

lemma tree_map_id : ∀ (xs : tree α), tree_map id xs = id xs :=
begin
  intro xs,
  induction xs with l x r hypl hypr,
  { refl, },
  { rw tree_map,
    rw hypl,
    rw hypr,
    refl, },
end

lemma tree_map_composition : ∀ (xs : tree α) (f : β -> γ) (g : α -> β),
  tree_map (f ∘ g) xs = tree_map f (tree_map g xs) :=
begin
  intros,
  induction xs with l x r hypl hypr,
  { refl, },
  { repeat {rw tree_map},
  rw hypl,
  rw hypr, },
end
