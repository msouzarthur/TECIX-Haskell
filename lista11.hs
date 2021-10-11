--Primeira
data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
 deriving (Eq,Show)

--Segunda
finalDeSemana :: Dia -> Bool
finalDeSemana Sabado = True
finalDeSemana Domingo = True
finalDeSemana _ = False

--Terceira
data TalvezFloat = Valor Float | Erro String
 deriving (Eq,Show)

--Quarta
divisao :: Float -> Float -> TalvezFloat
divisao _ 0 = Erro "Erro de divisao por zero"
divisao n1 n2 = Valor (n1/n2)

--Quinta
data Nat = Zero | Suc Nat
 deriving (Eq,Show)

natToInt :: Nat -> Int
natToInt Zero    = 0
natToInt (Suc n) = 1 + natToInt n

--Sexta
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n =  Suc(intToNat (n-1))
