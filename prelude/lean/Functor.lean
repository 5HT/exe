/- Functor -/

import Setoid
import Cat
import Mor

/-
 - The category of functors
 -/

namespace Functor
    -- axioms of functor
    section withOnHom
        variables (C D : CatType) (onOb : C → D)
        variables (onHom : Π{X Y : C}, (X ⇒C⇒ Y)⥤(onOb X ⇒D⇒ onOb Y))
        definition OnIdProp : Prop :=
            ∀{X : C}, (onHom ①) ≡(onOb X ⇒D⇒ onOb X)≡ ①
        definition OnIdInvProp : Prop :=
            ∀{X : C}, ① ≡(onOb X ⇒D⇒ onOb X)≡ (onHom ①)
        definition OnMulProp : Prop :=
            ∀{X Y Z : C}, ∀(g : Y ⇒C⇒ Z), ∀(f : X ⇒C⇒ Y),
                (onHom (g ⊙C⊙ f)) ≡(onOb X ⇒D⇒ onOb Z)≡ ((onHom g) ⊙D⊙ (onHom f))
        definition OnMulInvProp : Prop :=
            ∀{X Y Z : C}, ∀(g : Y ⇒C⇒ Z), ∀(f : X ⇒C⇒ Y),
                ((onHom g) ⊙D⊙ (onHom f)) ≡(onOb X ⇒D⇒ onOb Z)≡ (onHom (g ⊙C⊙ f))
    end withOnHom
end Functor

-- functor as an object of the category `Functor`
record FunctorType (C D : CatType) : Type :=
    (onOb : C → D)
    (onHom : Π{X Y : C}, (X ⇒C⇒ Y)⥤(onOb X ⇒D⇒ onOb Y))
    (onId : Functor.OnIdProp C D onOb @onHom)
    (onMul : Functor.OnMulProp C D onOb @onHom)
abbreviation Functor.MkOb {C D : CatType} := @FunctorType.mk C D

abbreviation FunctorType.onIdInv {C D : CatType} (F : FunctorType C D)
    : Functor.OnIdInvProp C D (@FunctorType.onOb _ _ F) (@FunctorType.onHom _ _ F) :=
    λ X, SetoidType.Sym _ (@FunctorType.onId _ _ F X)

abbreviation FunctorType.onMulInv {C D : CatType} (F : FunctorType C D)
    : Functor.OnMulInvProp C D (@FunctorType.onOb _ _ F) (@FunctorType.onHom _ _ F) :=
    λ X Y Z, λ f g, SetoidType.Sym _ (@FunctorType.onMul _ _ F X Y Z f g)

-- action on carrier
attribute FunctorType.onOb [coercion]
infixl `$$`:100 := FunctorType.onOb
definition cat_hom.onHom {C D : CatType} (F : FunctorType C D)
    {X Y : C} (m : X ⇒C⇒ Y) := (FunctorType.onHom F) $ m
attribute cat_hom.onHom [coercion]
infixl `$$/`:100 := cat_hom.onHom
definition cat_hom.onEqu {C D : CatType} (F : FunctorType C D)
    {X Y : C} {m1 m2 : X ⇒C⇒ Y} (e : m1 ≡_≡ m2) := (FunctorType.onHom F) $/ e
attribute cat_hom.onEqu [coercion]
infixl `$$//`:100 := cat_hom.onEqu
definition cat_hom.onSquare {C D : CatType} (F : FunctorType C D)
        {X11 X12 X21 X22 : C}
        {m1x : X11 ⇒C⇒ X12} {m2x : X21 ⇒C⇒ X22}
        {mx1 : X11 ⇒C⇒ X21} {mx2 : X12 ⇒C⇒ X22}
    (sq : Mor.SquareProp C m1x m2x mx1 mx2)
    : Mor.SquareProp D (F m1x) (F m2x) (F mx1) (F mx2) :=
    (FunctorType.onMulInv F mx2 m1x)
        ⊡((F X11) ⇒D⇒ (F X22))⊡
            (F $$// sq)
        ⊡((F X11) ⇒D⇒ (F X22))⊡
    (FunctorType.onMul F m2x mx1)
attribute cat_hom.onSquare [coercion]
infixl `$$///`:100 := cat_hom.onSquare

namespace Functor
    -- morphisms between functors are natural transformations
    record HomType {C D : CatType} (F G : FunctorType C D) : Type :=
        (onOb : Π(X : C), F X ⇒D⇒ G X)
        (onHom : ∀{X Y : C}, ∀(m : X ⇒C⇒ Y), Mor.SquareProp D (F m) (G m) (onOb X) (onOb Y))
    abbreviation MkHom {C D : CatType} {F G : FunctorType C D} := @HomType.mk C D F G
    abbreviation HomType.onHomInv {C D : CatType} {F G : FunctorType C D}
        (nat : HomType F G) {X Y : C} (m : X ⇒C⇒ Y)
        := SetoidType.Sym (F X ⇒D⇒ G Y) (HomType.onHom nat m)
end Functor

-- action
attribute Functor.HomType.onOb [coercion]
infixl `/$$`:100 := Functor.HomType.onOb
infixl `/$$/`:100 := Functor.HomType.onHom
infixl `/$/$/`:100 := Functor.HomType.onHomInv

namespace Functor
    -- setoid of natural transformations
    definition HomSet {C D : CatType} : Cat.HomType (FunctorType C D) :=
        λ(F G : FunctorType C D), Setoid.MkOb
            /- El -/ (HomType F G)
            /- Equ -/ (λ(f g : HomType F G), ∀ X, f X ≡(F X ⇒D⇒ G X)≡ g X)
            /- Refl-/ (λ f, λ X, ⊜)
            /- Trans -/ (λ f g h, λ fg gh, λ X, (fg X) ⊡(F X ⇒D⇒ G X)⊡ (gh X))
            /- Sym -/ (λ f g, λ fg, λ X, SetoidType.Sym (F X ⇒D⇒ G X) (fg X))
end Functor

-- the dedicated arrow for morphisms of functors (nat.tr.)
infixr `⟹`:10 := Functor.HomSet

namespace Functor
  section withCD
    variables {C D : CatType}

    -- identity in functor category (identity natural transformation)
    definition Id : Cat.IdType (@Functor.HomSet C D) :=
      λ(F : FunctorType C D), Functor.MkHom
        /- onOb -/ (λ X, ①)
        /- onHom -/ (λ X Y, λ m, Mor.SquareId1 D (F m))

    -- multiplication in functor category (composition of natural transformations)
    definition Mul : Cat.MulType (@Functor.HomSet C D) :=
        λ(F G H : FunctorType C D), Setoid.MkHom -- :[Hom G H ⥤ Hom F G ⥤ Hom F H]
            /- onEl -/ (λ(a : G⟹H), Setoid.MkHom -- :[Hom F G ⥤ Hom F H]
                /- onEl -/ (λ(b : F⟹G), Functor.MkHom -- :[Hom F H]
                    /- onOb -/ (λ(o : C), a o ⊙D⊙ b o)
                    /- onHom -/ (λ(o1 o2 : C), λ(m12 : o1 ⇒C⇒ o2),
                        Mor.SquareMul1 D (b /$$/ m12) (a /$$/ m12) ))
                /- onEqu -/ (λ(b1 b2 : F⟹G), λ(b12 : b1 ≡(F⟹G)≡ b2),
                    λ(o : C), ((CatType.Mul D) (a o)) $/ (b12 o) /- Equ in Hom F H -/ ))
            /- onEqu -/ (λ(a1 a2 : G⟹H), λ(a12 : a1 ≡(G⟹H)≡ a2), λ(b : F⟹G),
                    λ(o : C), ((CatType.Mul D) $/ (a12 o)) (b o) /-Equ in Hom F H -/ )

    definition UnitL : Cat.UnitLProp (@Id C D) (@Mul C D) :=
        λ(A B : FunctorType C D), λ(f : A⟹B),
            λ(o : C), CatType.UnitL D (f o)

    definition UnitR : Cat.UnitRProp (@Id C D) (@Mul C D) :=
        λ(A B : FunctorType C D), λ(f : A⟹B),
            λ(o : C), CatType.UnitR D (f o)

    definition Assoc : Cat.AssocProp (@Mul C D) :=
        λ(F G H I: FunctorType C D), λ(f : H⟹I), λ(g : G⟹H), λ(h : F⟹G),
            λ(o : C), CatType.Assoc D (f o) (g o) (h o)

  end withCD
end Functor

-- the category of functors (between categories C and D)
definition FunctorCat (C D : CatType) : CatType :=
    Cat.MkOb
        (FunctorType C D) (@Functor.HomSet C D)
        (@Functor.Id C D) (@Functor.Mul C D)
        (@Functor.UnitL C D) (@Functor.UnitR C D) (@Functor.Assoc C D)

-- the dedicated arrow for morphisms of categories (functors)
infixr `⟶`:100 := FunctorCat

-- constant functor
namespace Cat
namespace Delta
  section withCD
    variables (C D : CatType)

    definition onOb  (d : D)
        : C ⟶ D :=
        Functor.MkOb
            /- onOb -/ ( λ(c : C), d)
            /- onHom -/ ( λ(c1 c2 : C), Setoid.Const (c1 ⇒C⇒ c2) (d ⇒D⇒ d) ① )
            /- onId -/ ( λ(c : C), ⊜)
            /- onMul -/ ( λ(c1 c2 c3 : C), λ(g : c2 ⇒C⇒ c3), λ(f : c1 ⇒C⇒ c2),
                CatType.UnitCInv D)

    definition onHom.onEl {d1 d2 : D} (f : d1 ⇒D⇒ d2)
        : (onOb C D d1) ⟹ (onOb C D d2) :=
        Functor.MkHom
            ( λ(c : C), f)
            ( λ(c1 c2 : C), λ(c12 : c1 ⇒C⇒ c2), Mor.SquareId2 D f)

    definition onHom.onEqu{d1 d2 : D} {f1 f2 : d1 ⇒D⇒ d2} (e : f1 ≡(d1 ⇒D⇒ d2)≡ f2 )
        :   (onHom.onEl C D f1)
                ≡((onOb C D d1) ⟹ (onOb C D d2))≡
            (onHom.onEl C D f2) :=
        λ(c : C), e

    definition onHom {d1 d2 : D} : (d1 ⇒D⇒ d2)⥤(onOb C D d1 ⟹ onOb C D d2) :=
        Setoid.MkHom (@onHom.onEl C D d1 d2) (@onHom.onEqu C D d1 d2)

    definition onId : Functor.OnIdProp D (C ⟶ D) (@onOb C D) (@onHom C D) :=
        λ (d : D), λ (c : C), ⊜

    definition onMul : Functor.OnMulProp D (C ⟶ D) (@onOb C D) (@onHom C D) :=
        λ(d1 d2 d3 : D), λ(g : d2 ⇒D⇒ d3), λ(f : d1 ⇒D⇒ d2), λ (c : C), ⊜

  end withCD
end Delta

    definition Delta (C D : CatType) : D ⟶ (C ⟶ D) :=
        Functor.MkOb
            (@Delta.onOb C D)
            (@Delta.onHom C D)
            (@Delta.onId C D)
            (@Delta.onMul C D)

end Cat

namespace Functor

    abbreviation ConeType {C D : CatType} (X : D) (F : C ⟶ D) : Type
    := Π(Y : C), (X ⇒D⇒ F Y)

    abbreviation CoconeType {C D : CatType} (F : C ⟶ D) (X : D) : Type
    := Π(Y : C), (F Y ⇒D⇒ X)

    abbreviation IsConeProp {C D : CatType} (X : D) (F : C ⟶ D)
        (arrow : ConeType X F) : Prop
    := ∀{A B : C}, ∀(m : A ⇒C⇒ B),
            (arrow B) ≡(X ⇒D⇒ F B)≡ ((F m) ⊙D⊙ (arrow A))

    abbreviation IsCoconeProp {C D : CatType} (F : C ⟶ D) (X : D)
        (arrow : CoconeType F X) : Prop
    := ∀{A B : C}, ∀(m : A ⇒C⇒ B),
            ((arrow B) ⊙D⊙ (F m)) ≡(F A ⇒D⇒ X)≡ (arrow A)

    definition FromCone {C D : CatType} {X : D} {F : C ⟶ D}
        (arrow : ConeType X F)
        (cone : IsConeProp X F arrow)
        : (Cat.Delta C D X) ⟹ F
    := MkHom
        /- onOb -/ arrow
        /- onHom -/ ( λ(A B: C), λ(m : A ⇒C⇒ B),
            (CatType.UnitR D (arrow B))⊡(X ⇒D⇒ F B)⊡(cone m))

    definition FromCocone {C D : CatType} {F : C ⟶ D} {X : D}
        (arrow : CoconeType F X)
        (cocone : IsCoconeProp F X arrow)
        : F ⟹ (Cat.Delta C D X)
    := MkHom
        /- onOb -/ arrow
        /- onHom -/ ( λ(A B: C), λ(m : A ⇒C⇒ B),
            (cocone m)⊡(F A ⇒D⇒ X)⊡(CatType.UnitLInv D (arrow A))
            )

end Functor

-- the identity functor (1 in Cat)
definition Cat.Id {C : CatType} : C ⟶ C := Functor.MkOb
    /- onOb -/ ( λ(X : C), X)
    /- onHom -/ ( λ(X Y : C), Setoid.Id)
    /- onId -/ ( λ(X : C), ⊜)
    /- onMul -/ ( λ(X Y Z : C), λ(g : Y ⇒C⇒ Z), λ(f : X ⇒C⇒ Y), ⊜)

notation `𝟙` := Cat.Id

-- multiplication of functors
definition Cat.MulFF {C D E : CatType} (F : D ⟶ E) (G : C ⟶ D)
    : C ⟶ E := Functor.MkOb
        /- onOb -/ ( λ(X : C), (F (G X)))
        /- onHom -/ ( λ(X Y : C),
            (@FunctorType.onHom D E F (G X) (G Y))
                ⊙SetoidCat⊙
            (@FunctorType.onHom C D G X Y))
        /- onId -/ ( λ(X : C),
            (F (@FunctorType.onId C D G X))
                ⊡((F (G X)) ⇒E⇒ (F (G X)))⊡
            (@FunctorType.onId D E F (G X)))
        /- onMul -/ ( λ(X Y Z : C), λ(g : Y ⇒C⇒ Z), λ(f : X ⇒C⇒ Y),
            (F (@FunctorType.onMul C D G X Y Z g f))
                ⊡((F (G X)) ⇒E⇒ (F (G Z)))⊡
            (@FunctorType.onMul D E F (G X) (G Y) (G Z) (G g) (G f)))

notation F `⊗` G : 100 := Cat.MulFF F G

definition Cat.MulFN {C D E : CatType} (F : D ⟶ E) {G1 G2 : C ⟶ D} (g : G1 ⟹ G2)
    : (F ⊗ G1) ⟹ (F ⊗ G2) := Functor.MkHom
        /- onOb -/ ( λ(X : C), (F $$/ (g /$$ X)))
        /- onHom -/ ( λ(X Y : C), λ(m : X ⇒C⇒ Y), (F $$/// (g /$$/ m)) )

notation F `⊗/` g : 100 := Cat.MulFN F g

definition Cat.MulNF {C D E : CatType} {F1 F2 : D ⟶ E} (f : F1 ⟹ F2) (G : C ⟶ D)
    : (F1 ⊗ G) ⟹ (F2 ⊗ G) := Functor.MkHom
        /- onOb -/ ( λ(X : C), (f /$$ (G $$ X)))
        /- onHom -/ ( λ(X Y : C), λ(m : X ⇒C⇒ Y), (f /$$/ (G $$/ m)) )

notation f `/⊗` G : 100 := Cat.MulNF f G

definition Cat.Assoc {C1 C2 C3 C4 : CatType}
    (F34 : C3 ⟶ C4) (F23 : C2 ⟶ C3) (F12 : C1 ⟶ C2)
    : ((F34 ⊗ F23) ⊗ F12) ⟹ (F34 ⊗ (F23 ⊗ F12))
    := Functor.MkHom
        /- onOb -/ (λ X, ①)
        /- onHom -/ (λ X Y, λ m, Mor.SquareId1 C4 (F34 $$/ (F23 $$/ (F12 $$/ m))))
definition Cat.AssocInv {C1 C2 C3 C4 : CatType}
    (F34 : C3 ⟶ C4) (F23 : C2 ⟶ C3) (F12 : C1 ⟶ C2)
    : (F34 ⊗ (F23 ⊗ F12)) ⟹ ((F34 ⊗ F23) ⊗ F12)
    := Functor.MkHom
        /- onOb -/ (λ X, ①)
        /- onHom -/ (λ X Y, λ m, Mor.SquareId1 C4 (F34 $$/ (F23 $$/ (F12 $$/ m))))

definition Cat.UnitL {C1 C2 : CatType} (F : C1 ⟶ C2) : (𝟙 ⊗ F) ⟹ F
    := Functor.MkHom ( λ X, ①) (λ X Y, λ m, Mor.SquareId1 C2 (F $$/ m))
definition Cat.UnitLInv {C1 C2 : CatType} (F : C1 ⟶ C2) : F ⟹ (𝟙 ⊗ F)
    := Functor.MkHom ( λ X, ①) (λ X Y, λ m, Mor.SquareId1 C2 (F $$/ m))
definition Cat.UnitR {C1 C2 : CatType} (F : C1 ⟶ C2) : (F ⊗ 𝟙) ⟹ F
    := Functor.MkHom ( λ X, ①) (λ X Y, λ m, Mor.SquareId1 C2 (F $$/ m))
definition Cat.UnitRInv {C1 C2 : CatType} (F : C1 ⟶ C2) : F ⟹ (F ⊗ 𝟙)
    := Functor.MkHom ( λ X, ①) (λ X Y, λ m, Mor.SquareId1 C2 (F $$/ m))

abbreviation Cat.FullEqu {C D : CatType} {F G : C ⟶ D} : EquType [F ⟹ G]
    := SetoidType.Equ (F ⟹ G)

infix `≣` : 10 := Cat.FullEqu

abbreviation Cat.Wiskering {C D : CatType} {F G H : C ⟶ D}
    (b : G ⟹ H) (a : F ⟹ G) : (F ⟹ H)
    := b ⊙(C ⟶ D)⊙ a

infixl `○` : 100 := Cat.Wiskering

-- TODO: Cat.Mul, Cat,UnitorLR, Cat.Associator, Cat.TriangleLCREqu, Cat.PentagonEqu
