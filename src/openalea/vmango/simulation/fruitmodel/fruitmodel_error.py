class FruitModelValueError(ValueError):
 def __init__(self, arg):
  self.strerror = arg
  self.args = {arg}

class FruitModelInputError(ValueError):
 def __init__(self, arg):
  self.strerror = arg
  self.args = {arg}
