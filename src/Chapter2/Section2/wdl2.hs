module Lab2 where
  -- import Data.Array
  -- import Data.List.Split

------------------------------------------------------------------------------------------------------------------------------
-- Lab 2: Validating Credit Card Numbers
------------------------------------------------------------------------------------------------------------------------------

{- Example:
     Ex 0 - Given: 
              4012888888881881
     Ex 1 - Reverse the list of digits to start with the rightmost digit:
              [1,8,8,1,8,8,8,8,8,8,8,8,2,1,0,4]
     Ex 2 - Double every second digit:
              [1,16,8,2,8,16,8,16,8,16,8,16,2,2,0,8]
     Ex 3 - Split each element into individual digits (i.e. 16 becomes [1,6]), then sum all the digits:
              [1,[1,6],8,2,8,[1,6],8,[1,6],8,[1,6],8,[1,6],2,2,0,8]
              90
     Ex 4 - Calculate the modulus of Ex 3 result (i.e. modulus of 90 over 10)
              90 % 10
              0
     Ex 5 - Report whether valid credt card number. Valid is result of 0.
              True
-}

-- ===================================
-- Ex. 0
-- ===================================

-- PSEUDO CODE
-- Find the digits of a number by defining a Function
-- that takes n :: Integer where n >= 0
-- returns list of digits of n
-- should satisfy properties:
--   eval (toDigits n) == n
--   all (\d -> d >= 0 && d < 10) (toDigits n)
-- where:
--   eval xs = foldl (\x y -> y + (10 * x)) 0 xs
-- where:
--   toDigits n results in error for n < 0

  toDigits   :: Integer -> [Integer]
  -- toDigits = undefined

-- ANSWER ATTEMPT #1
{-
toDigits n = if n >= 0 && n < 10
             then [0..n] -- similar to zeroto example
             else [] -- how to hard code error?
-}
 
-- ANSWER ATTEMPT #2 onwards
-- toDigits n = [x | x <- [1..n], all (\n -> n >= 0 && n < 10) (toDigits n)] -- similar to factors example 
-- toDigits n = [x | x <- [1..n], n >= 0 && n < 10]
-- let toDigits n = listArray (0, length n - 1) n
-- toDigits [3,6]
-- toDigits n = listArray (0, length n - 1) n -- import Data.Array
  toDigits n = [x | x <- [1..n], n >= 0 && n < 10]
-- toDigits n | n >= 0 && n < 10 = [0..n]
-- toDigits n = [x | x <- splitOn ",", n >= 0 && n < 10]
-- toDigits n = [x | x <- [n], n >= 0 && n < 10]
-- toDigits n = [x | x <- n mod 10, n mod 10 > 0]
--  toDigits n = if n < 0 || n > 100000000000000
--               then []
--               else toDigits (n `mod` 10)


-- ===================================
-- Ex. 1
-- ===================================

-- PSEUDO CODE
-- Reverse the digits of a number by defining a Function
-- that takes n :: Integer where n >= 0 and
-- returns a List of the digits of n in reverse order
-- where satisfies properties:
--   evalRev(toDigitsRev n) == n
--   all (\d -> d >= 0 && d < 10) (toDigitsRev n) 
-- where:
--   evalRev xs = foldr (\x y -> x + (10 * y)) 0 xs
-- where:
--   toDigitsRev n results in error for n < 0

  toDigitsRev :: Integer -> [Integer]
  toDigitsRev = undefined

-- ===================================
-- Ex. 2
-- ===================================

-- Algorithm should double the value of every second digit beginning with the rightmost

-- PSEUDO CODE
-- defining a Function doubleSecond :: [Integer] -> [Integer]
-- that doubles every second number in the input List
-- (i.e. [8, 7, 6, 5] becomes [8, 14, 6, 10])

  doubleSecond :: [Integer] -> [Integer]
  doubleSecond = undefined

-- ===================================
-- Ex. 3
-- ===================================

-- Algorithm should add the digits of the doubled values and the undoubled digits from the original number

-- PSEUDO CODE
-- define a Function that takes the mixture of elements with both one and multiple-digit numbers,
-- and breaks them into separate digits to calculate the sum of all individual digits 
-- (i.e. sumDigits [8,14,6,10] = 8 + (1 + 4) + 6 + (1 + 0) = 20)

  sumDigits :: [Integer] -> Integer
  sumDigits = undefined


-- ===================================
-- Ex. 4
-- ===================================

-- Algorithm should calculate the modulus of the sum divided by 10 and report valid 
-- number if result equals 0

-- PSEUDO CODE
-- define a Function that tells whether any input n :: Integer where n >= 0 could be a
-- a valid credit card number using Functions defined in previous exercises Ex 1 to 3 

  isValid :: Integer -> Bool
  isValid = undefined


-- ===================================
-- Ex. 5
-- ===================================
    
  numValid :: [Integer] -> Integer
  numValid xs = sum . map (\_ -> 1) $ filter isValid xs


  creditcards :: [Integer]
  creditcards = [ 4716347184862961,
                4532899082537349,
                4485429517622493,
                4320635998241421,
                4929778869082405,
                5256283618614517,
                5507514403575522,
                5191806267524120,
                5396452857080331,
                5567798501168013,
                6011798764103720,
                6011970953092861,
                6011486447384806,
                6011337752144550,
                6011442159205994,
                4916188093226163,
                4916699537435624,
                4024607115319476,
                4556945538735693,
                4532818294886666,
                5349308918130507,
                5156469512589415,
                5210896944802939,
                5442782486960998,
                5385907818416901,
                6011920409800508,
                6011978316213975,
                6011221666280064,
                6011285399268094,
                6011111757787451,
                4024007106747875,
                4916148692391990,
                4916918116659358,
                4024007109091313,
                4716815014741522,
                5370975221279675,
                5586822747605880,
                5446122675080587,
                5361718970369004,
                5543878863367027,
                6011996932510178,
                6011475323876084,
                6011358905586117,
                6011672107152563,
                6011660634944997,
                4532917110736356,
                4485548499291791,
                4532098581822262,
                4018626753711468,
                4454290525773941,
                5593710059099297,
                5275213041261476,
                5244162726358685,
                5583726743957726,
                5108718020905086,
                6011887079002610,
                6011119104045333,
                6011296087222376,
                6011183539053619,
                6011067418196187,
                4532462702719400,
                4420029044272063,
                4716494048062261,
                4916853817750471,
                4327554795485824,
                5138477489321723,
                5452898762612993,
                5246310677063212,
                5211257116158320,
                5230793016257272,
                6011265295282522,
                6011034443437754,
                6011582769987164,
                6011821695998586,
                6011420220198992,
                4716625186530516,
                4485290399115271,
                4556449305907296,
                4532036228186543,
                4916950537496300,
                5188481717181072,
                5535021441100707,
                5331217916806887,
                5212754109160056,
                5580039541241472,
                6011450326200252,
                6011141461689343,
                6011886911067144,
                6011835735645726,
                6011063209139742,
                379517444387209,
                377250784667541,
                347171902952673,
                379852678889749,
                345449316207827,
                349968440887576,
                347727987370269,
                370147776002793,
                374465794689268,
                340860752032008,
                349569393937707,
                379610201376008,
                346590844560212,
                376638943222680,
                378753384029375,
                348159548355291,
                345714137642682,
                347556554119626,
                370919740116903,
                375059255910682,
                373129538038460,
                346734548488728,
                370697814213115,
                377968192654740,
                379127496780069,
                375213257576161,
                379055805946370,
                345835454524671,
                377851536227201,
                345763240913232
                ]

