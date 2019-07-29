
class BST:
    def __init__(self, n=None):
        self.data = n
        self.left = None
        self.right = None

    def insert(self, n):
        if self.data:
            if self.data > n:
                # greater than
                if self.left:
                    self.left.insert(n)
                    return n
                else:
                    self.left = BST(n)
                    return n
            elif self.data < n:
                # less than
                if self.right:
                    self.right.insert(n)
                    return n
                else:
                    self.right = BST(n)
                    return n
            else:
                return None
        else:
            self.data = n

    def query(self, n, trace=""):
        if self.data:
            if n == self.data:
                if trace:
                    print(trace)
                else:
                    print("found: root")
            elif n < self.data:
                if self.left:

                    self.left.query(n, trace + " l" if trace else "l")
                else:
                    print("not found")
            else:
                if self.right:

                    self.right.query(n, trace + " r" if trace else "r")
                else:
                    print("not found")
        else:
            print("not found")


class RPNStack:
    def __init__(self, data=[]):
        self.data = data

    def operate(self, op):
        b = self.data.pop()

        if op == "~":
            self.data.append(-1 * b)
            return -1 * b

        a = self.data.pop()

        if op == "+":
            self.data.append(a + b)
            return a + b
        elif op == "-":
            self.data.append(a - b)
            return a - b
        elif op == "*":
            self.data.append(a * b)
            return a * b
        else:
            self.data.append(a / b)
            return a / b

    def push(self, n):
        self.data.append(n)
        return n


class DFA:
    def __init__(self, init_state, symbols, final_states, transitions):
        self.init_state = init_state
        self.current_state = self.init_state
        self.symbols = symbols
        self.final_states = final_states
        self.transitions = transitions

    def transform(self, in_str):
        self.current_state = self.transitions[self.current_state][in_str]

    def evaluate(self, in_str):
        for i in in_str.strip():
            self.transform(i)
        end_state = self.current_state
        self.state = self.init_state
        return end_state in self.final_states