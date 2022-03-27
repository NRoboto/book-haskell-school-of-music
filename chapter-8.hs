data MyData = Data1
  { myFloat :: Float,
    myInteger :: Integer,
    myBool :: Bool
  }

dataInst =
  Data1
    { myFloat = 3.2,
      myInteger = 5,
      myBool = True
    }

outputMyFloat = print $ myFloat dataInst