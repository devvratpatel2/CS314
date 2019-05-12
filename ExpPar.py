import re
from Exphelp import *
class TokenStream:
    
    def __init__(self,source,v):
        self.stream = re.sub(re.compile(r'\s+'), '',source)     
        self.variable = v
        
    def __str__(self):
        return self.stream
    
    def __getChar(self,i):
        if len(self.stream) == 0:
            return False
        return self.stream[i]
    
    #get the first word in the stream
    #return False if output doesn't match
    def getWord(self,word):
        if len(word) > len(self.stream):
            return False
        result = []
        for i in xrange(0,len(word)):
            result.append(self.__getChar(i))
        if all(result):
            result = "".join(result)
            if result == word:
                self.stream = self.stream[len(result):]
                return result
            else:
                return False
        else:
            return False
            
        
    #get the first number in the stream
    #return False if number cannot be parsed correctly    
    def getNumber(self):
        temp = ""
        result = ""
        for i in xrange(0,len(self.stream)):
            c = self.stream[i]
            if i == 0 and c == '-':
                temp += c
            elif c.isdigit() or c == '.':
                temp += c
            else:
                break
        try:
            if temp.find(".") != -1:
                result = float(temp)
            else:
                result = int(temp)
        except ValueError:
            return False
        self.stream = self.stream[len(temp):]
        return result
    
    def getOperator(self):
        operators = "+-*/^"
        for o in operators:
            c = self.getWord(o)
            if c :
                return c
        return False
            
    def getLeftParen(self):
        return self.getWord("(")
                 
    def getRightParen(self):
        return self.getWord(")")
    
    def getFunction(self):
        functions = ["e^","ln","sin","cos","tan","sec","cot","csc"]
        for f in functions:
            result = self.getWord(f)
            if result:
                return result
        return False        
    
    def getVariable(self):
        return self.getWord(self.variable)
    
    def hasStream(self):
        return len(self.stream) != 0

class ParsingError(Exception):
    pass
class ParserError(Exception):
    pass
class Parser:
    
    def __init__(self,source,v):
        if not (isinstance(v,str) or len(v) == 1):
            raise ParserError
        pattern = '\d' + v +'|' + v +'\(' + '|\)\(|' + '\d\(' +'|\)' + v
        pattern += ("|\de^" + "|\dln" + "|\dsin" + "|\dcos" + "|\dtan" + "|\dsec" + "|\dcot" + "|\dcsc")   
        pattern += ("|de^" + "|dln" + "|dsin" + "|dcos" + "|dtan" + "|dsec" + "|dcot" + "|dcsc").replace('d',v)
        while (True):
            result = re.search(pattern,source)
            if result:
                index = re.search(pattern,source).start() + 1
                source = source[:index] + '*' + source[index:]  
            else:
                break
        self.ts = TokenStream(source, v)
    
    def __str__(self):
        return str(self.ts)
    def parse(self):
        result = self.startParse()
        if self.ts.hasStream():
            raise ParsingError
        else:
            return result
        
    def startParse(self):
        return self.parsePlusMinus()
        
    def parsePlusMinus(self):
        return self.parsePlusMinusSeq(self.parseMultiplyDivide())
    
    def parsePlusMinusSeq(self,left):
        if self.ts.getWord("+"):
            right = self.parseMultiplyDivide()
            return self.parsePlusMinusSeq(Plus(left,right))
        elif self.ts.getWord("-"):
            right = self.parseMultiplyDivide()
            return self.parsePlusMinusSeq(Minus(left,right))
        else:
            return left
        
    def parseMultiplyDivide(self):
        return self.parseMultiplyDivideSeq(self.parsePower()) 
    
    def parseMultiplyDivideSeq(self,left):
        if self.ts.getWord("*"):
            right = self.parsePower()
            return self.parseMultiplyDivideSeq(Multiply(left,right))
        elif self.ts.getWord("/"):
            right = self.parsePower()
            return self.parseMultiplyDivideSeq(Divide(left,right))
        else:
            return left
        
    def parsePower(self):
        return self.parsePowerSeq(self.parseFunctions())
    
    def parsePowerSeq(self,left):
        if self.ts.getWord("^"):
            right = self.parseFunctions()
            return self.parsePowerSeq(Power(left,right))
        else:
            return left
    def parseFunctions(self):
        f = self.ts.getFunction()
        if f == "e^":
            argument = self.parseVariable()
            return E(argument)
        elif f == "ln":
            return Ln(self.parseParenthesis())

        elif f == "sin":
            return Sin(self.parseParenthesis())
            
        elif f == "cos":
            return Cos(self.parseParenthesis())

        elif f == "tan":
            return Tan(self.parseParenthesis())

        elif f == "sec":
            return Sec(self.parseParenthesis())

        elif f == "cot":
            return Cot(self.parseParenthesis())
            
        elif f == "csc":
            return Csc(self.parseParenthesis())
        else:
            return self.parseVariable()
        
    def parseVariable(self):
        v = self.ts.getVariable()
        if v:
            return Variable(v)
        else:
            return self.parseConstant()
        
    def parseConstant(self):
        n = self.ts.getNumber()
        if n:
            return Constant(n)
        else:
            return self.parseParenthesis()
        
    def parseParenthesis(self):
        leftParen = self.ts.getLeftParen()
        if leftParen:
            v = self.startParse()
            rightParen = self.ts.getRightParen()                
            if rightParen:
                return v
            else:
                raise ParsingError
        else:
            raise ParsingError

def main():
    try:
        while (True):
            input = raw_input("f(x)=")
            p = Parser(input,'x')
            result = p.parse().derivative()
            print ("f' (x)=") , result
    except (ParsingError,ParserError):
        print ("Parsing Error")

if __name__ == "__main__":
    main()

    