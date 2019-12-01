main :: IO ()
main = do
  f <- readFile "input"
  let masses = map read . lines $ f
      result = sum . map fuelForMass $ masses
  print result
  let result2 = sum . map fuelForMassAndFuel $ masses
  print result2

fuelForMass :: Int -> Int
fuelForMass mass = max (mass `div` 3 - 2) 0

fuelForMassAndFuel :: Int -> Int
fuelForMassAndFuel mass =
  let fuel = fuelForMass mass
      fuelForFuel = fuelForMass fuel
  in fuel + if fuelForFuel > 0 then fuelForMassAndFuel fuel else 0
