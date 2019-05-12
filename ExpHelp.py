from abc import ABCMeta, abstractmethod
import numbers
import math

# base class for all expressions
class Expression (object):
    __metaclass__ = ABCMeta
    
    #return: another instance of Expression
    @abstractmethod
    def derivative(self):
        pass
    
    #param: x = a number
    #return: a number
    @abstractmethod
    def compute(self,x):
        pass
    
    #return: a string
    @abstractmethod
    def __str__(self):
        pass
    
    #param: other = an Expression
    @abstractmethod
    def __eq__(self,other):
        pass
    
    def __ne__(self,other):
        return not self.__eq__(other)

def simplify(expr):
    assert isinstance(expr,Expression)
    
    if isinstance(expr,Constant):
        return expr
    elif isinstance(expr,Variable):
        return expr
    elif isinstance(expr,Plus):
        
        left = simplify(expr.left)
        right = simplify(expr.right)
        if left == Constant(0) and right == Constant(0):
            return Constant(0)
        elif left == Constant(0):
            return right
        elif right == Constant(0):
            return left
        else:
            return Plus(left, right)
    
    elif isinstance(expr,Minus):
        
        left = simplify(expr.left)
        right = simplify(expr.right)
        if left == Constant(0) and right ==Constant(0):
            return Constant(0)
        elif right == Constant(0):
            return left
        elif left == Constant(0):
            return Multiply(Constant(-1), right)
        else:
            return Minus(left,right)
        
    elif isinstance(expr,Multiply):
        
        left = simplify(expr.left)
        right = simplify(expr.right)
        if left == Constant(0) or right == Constant(0):
            return Constant(0)
        elif left == Constant(1):
            return right
        elif right == Constant(1):
            return left
        else:
            return Multiply(left, right)
        
    elif isinstance(expr,Divide):
        
        left = simplify(expr.left)
        right = simplify(expr.right)        
        if left == Constant(0):
            return Constant(0)
        elif left == right:
            return Constant(1)
        else:
            return Divide(left, right)
        
    elif isinstance(expr,Power):
        
        base = simplify(expr.base)
        exponent = simplify(expr.exponent)
        if base == Constant(0):
            return Constant(0)
        elif base == Constant(1):
            return Constant(1)
        elif exponent == Constant(0):
            return Constant(1)
        elif exponent == Constant(1):
            return base
        else:
            return Power(base, exponent)
        
    elif isinstance(expr,E):
        exponent = simplify(expr.exponent)
        if exponent == Constant(0):
            return Constant(1)
        elif isinstance(exponent,Ln):
            return exponent.argument
        else:
            return E(exponent)
    elif isinstance(expr,Ln):
        argument = simplify(expr.argument)
        if argument == Constant(1):
            return Constant(0)
        elif isinstance(argument,E):
            return argument.exponent
        else:
            return Ln(argument)
    else:
        #Trig functions
        expression = simplify(expr.expression)
        if isinstance(expr,Sin):
            return Sin(expression)
        elif isinstance(expr,Cos):
            return Cos(expression)
        elif isinstance(expr,Tan):
            return Tan(expression)
        elif isinstance(expr,Cot):
            return Cot(expression)
        elif isinstance(expr,Sec):
            return Sec(expression)
        elif isinstance(expr,Csc):
            return Csc(expression)
        
class Constant(Expression):
    
    #param: n = a number
    def __init__(self,n):
        assert isinstance(n, numbers.Number)
        self.value = n
        
    def derivative(self):
        return Constant(0)
    
    def compute(self, x):
        return self.value
    
    def __str__(self):
        return str(self.value)
    
    def __eq__(self, other):
        return isinstance(other,Constant) and self.value == other.value
        
        
class Variable(Expression):
    
    #param: v = a character
    def __init__(self,v):
        assert isinstance(v,str)
        assert len(v) == 1
        self.value = v
        
    def derivative(self):
        return Constant(1)
    
    def compute(self, x):
        return x
    
    def __str__(self):
        return self.value
    
    def __eq__(self, other):
        return isinstance(other,Variable) and self.value == other.value
        
class Plus(Expression):
    
    #param: left = an Expression, right = an Expression
    def __init__(self,left,right):
        assert isinstance(left,Expression)
        assert isinstance(right,Expression)
        self.left = left
        self.right = right
        
    def derivative(self):
        left = self.left.derivative()
        right = self.right.derivative()
        return simplify(Plus(left, right))
    
    def compute(self, x):
        return self.left.compute(x) + self.right.compute(x)
    
    def __str__(self):
        return "(" + self.left.__str__() + "+" + self.right.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Plus) and self.left == other.left and self.right == other.right   

class Minus(Expression):
    
    #param: left = an Expression, right = an Expression
    def __init__(self,left,right):
        assert isinstance(left,Expression)
        assert isinstance(right,Expression)        
        self.left = left
        self.right = right
        
    def derivative(self):
        left = self.left.derivative()
        right = self.right.derivative()
        return simplify(Minus(left, right))
    
    def compute(self, x):
        return self.left.compute(x) - self.right.compute(x)
    
    def __str__(self):
        return "(" + self.left.__str__() + "-" + self.right.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Minus) and self.left == other.left and self.right == other.right      
    
class Multiply(Expression):
    
    #param: left = an Expression, right = an Expression
    def __init__(self,left,right):
        assert isinstance(left,Expression)
        assert isinstance(right,Expression)        
        self.left = left
        self.right = right
        
    def derivative(self):
        left = Multiply(self.left.derivative() , self.right)
        right =  Multiply(self.left , self.right.derivative())
        return simplify(Plus(left,right))
    
    def compute(self, x):
        return self.left.compute(x) * self.right.compute(x)
    
    def __str__(self):
        return "(" + self.left.__str__() + "*" + self.right.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Multiply) and self.left == other.left and self.right == other.right      

class Divide(Expression):
    
    #param: left = an Expression, right = an Expression
    def __init__(self,left,right):
        assert isinstance(left,Expression)
        assert isinstance(right,Expression)        
        self.left = left
        self.right = right
        
    def derivative(self):
        #general case
        left = Multiply(self.left.derivative() , self.right)
        right =  Multiply(self.left , self.right.derivative())
        up = Minus(left,right)
        down = Multiply(self.right, self.right)
        return simplify(Divide(up,down))
    
    def compute(self, x):
        return self.left.compute(x) / float(self.right.compute(x))
    
    def __str__(self):
        return "(" + self.left.__str__() + "/" + self.right.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Divide) and self.left == other.left and self.right == other.right      
    
class E(Expression):
    
    #param: exponent = an Expression
    def __init__(self,exponent):
        assert isinstance(exponent,Expression)
        self.exponent = exponent
    
    def derivative(self):
        if isinstance(self.exponent,Ln):
            return simplify(self.exponent.argument.derivative())
        else:
            return simplify(Multiply(self.exponent.derivative(),self))
    
    def compute(self, x):
        return math.pow(math.e,self.exponent.compute(x))
    
    def __str__(self):
        return "(e^" + self.exponent.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,E) and self.exponent == other.exponent
    
class Ln(Expression):
    
    #param: argument = an Expression
    def __init__(self,argument):
        assert isinstance(argument,Expression)
        self.argument = argument
    
    def derivative(self):
        if isinstance(self.argument,E):
            return simplify(self.argument.exponent.derivative())
        else:
            return simplify(Multiply(self.argument.derivative(),Power(self.argument, Constant(-1))))
    
    def compute(self, x):
        return math.log(self.argument.compute(x))
    
    def __str__(self):
        return "(ln " + self.argument.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Ln) and self.argument == other.argument    
    
class Power(Expression):
    
    #param: base = an Expression, Exponent = an Expression
    def __init__(self,base,exponent):
        assert isinstance(base,Expression)
        assert isinstance(exponent,Expression)        
        self.base = base
        self.exponent = exponent
        
    def derivative(self):
        #Power rule
        if isinstance(self.base,Variable) and isinstance(self.exponent,Constant):
            return simplify(Multiply(self.exponent, Power(self.base, Constant(self.exponent.value-1))))
        #this method covers everything else
        return simplify(E(Multiply(self.exponent, Ln(self.base))).derivative())
    
    def compute(self, x):
        return math.pow(self.base.compute(x), self.exponent.compute(x))
    
    def __str__(self):
        return "(" + self.base.__str__() + "^" + self.exponent.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Power) and self.base == other.base and self.exponent == other.exponent    

class Sin(Expression):
    
    #param: expression = an Expression
    def __init__(self,expression):
        assert isinstance(expression,Expression)
        self.expression = expression
    
    def derivative(self):
        return simplify(Multiply(self.expression.derivative(), Cos(self.expression)))
    
    def compute(self, x):
        return math.sin(self.expression.compute(x))
    
    def __str__(self):
        return "(sin " + self.expression.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Sin) and self.expression == other.expression
    
class Cos(Expression):
    
    #param: expression = an Expression
    def __init__(self,expression):
        assert isinstance(expression,Expression)
        self.expression = expression
    
    def derivative(self):
        return simplify(Multiply(self.expression.derivative(), Multiply(Constant(-1),Sin(self.expression))))
    
    def compute(self, x):
        return math.cos(self.expression.compute(x))
    
    def __str__(self):
        return "(cos " + self.expression.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Cos) and self.expression == other.expression    
    
class Tan(Expression):
    
    #param: expression = an Expression
    def __init__(self,expression):
        assert isinstance(expression,Expression)
        self.expression = expression
    
    def derivative(self):
        return simplify(Multiply(self.expression.derivative(), Power(Sec(self.expression),Constant(2))))
    
    def compute(self, x):
        return math.tan(self.expression.compute(x))
    
    def __str__(self):
        return "(tan " + self.expression.__str__() + ")" 
    
    def __eq__(self, other):
        return isinstance(other,Tan) and self.expression == other.expression    

class Cot(Expression):
    
    #param: expression = an Expression
    def __init__(self,expression):
        assert isinstance(expression,Expression)
        self.expression = expression
    
    def derivative(self):
        return simplify(Multiply(self.expression.derivative(), Multiply(Constant(-1),Power(Csc(self.expression),Constant(2)))))
    
    def compute(self, x):
        return 1 / math.tan(self.expression.compute(x))
    
    def __str__(self):
        return "(cot " + self.expression.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Cot) and self.expression == other.expression    
    
class Sec(Expression):
    
    #param: expression = an Expression
    def __init__(self,expression):
        assert isinstance(expression,Expression)
        self.expression = expression
    
    def derivative(self):
        return simplify(Multiply(self.expression.derivative(), Multiply(self,Tan(self.expression))))
    
    def compute(self, x):
        return 1 / math.cos(self.expression.compute(x))
    
    def __str__(self):
        return "(sec " + self.expression.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Sec) and self.expression == other.expression    
    
class Csc(Expression):
    
    #param: expression = an Expression
    def __init__(self,expression):
        assert isinstance(expression,Expression)
        self.expression = expression
    
    def derivative(self):
        return simplify(Multiply(self.expression.derivative(), Multiply(Constant(-1),Multiply(self,Cot(self.expression)))))
    
    def compute(self, x):
        return 1 / math.sin(self.expression.compute(x))
    
    def __str__(self):
        return "(csc " + self.expression.__str__() + ")"
    
    def __eq__(self, other):
        return isinstance(other,Csc) and self.expression == other.expression    
    
