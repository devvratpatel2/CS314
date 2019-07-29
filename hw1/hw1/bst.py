from helper import BST
from enum import Enum, unique
import re


@unique
class CommandType(Enum):
    INSERT = 1
    QUERY = 2

str = "found: "
def parse(line):
    rx_dict = {
        CommandType.INSERT: re.compile(r"i(\s+)(\d+)"),
        CommandType.QUERY: re.compile(r"q(\s+)(\d+)")
    }

    for key, rx in rx_dict.items():
        match = rx.search(line)
        if match:
            return key, match
    return None, None


def main():
    tree = BST()
    while True:
        try:
            in_str = input()
            command, match = parse(in_str)
            if command and match:
                if command == CommandType.INSERT:
                    if not tree.insert(float(match.group(2))):
                        continue
                else:
                    tree.query(float(match.group(2)))
            else:
                print("Invalid input")
        except EOFError:
            exit()


if __name__ == "__main__":
    main()