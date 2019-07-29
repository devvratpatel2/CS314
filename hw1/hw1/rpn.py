from helper import RPNStack
import re

def main():
    stack = RPNStack()

    while True:
        try:
            in_str = input()

            if re.match(r"[0-9]+", in_str):
                value = stack.push(float(in_str))
                print(int(value) if value.is_integer() else value)
            elif re.match(r"[\+\-\*\/]", in_str):
                value = stack.operate(in_str)
                print(int(value) if value.is_integer() else value)
            elif not in_str:
                break
            else:
                print("Invalid input")
        except EOFError:
            exit()


if __name__=="__main__":
    main()