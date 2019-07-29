import re
from helper import DFA
from sys import stdin


def get_states():
    #  print("Waiting for input")
    states = stdin.readline()
    dic = dict.fromkeys(filter(lambda state: "states" not in state, states.split()), dict())
    if not any(dic):
        print("Error")
        quit()

    #  print("got states")
    return dic


def get_symbols():
    syms = stdin.readline()
    symbols = set(filter(lambda symbol: "symbols" not in symbol, syms.split()))

    if not symbols:
        print("Error ")
        quit()

    #  print("got symbols")
    return symbols


def get_rules(transitions, symbols):
    line = stdin.readline()
    if line.strip() == "begin_rules":
        line = stdin.readline()
        while line.strip() != "end_rules":
            match = re.match(pattern=r"(.+)[\s*]->[\s*](.+)[\s*]on[\s*](.+)$", string=line)

            if match and \
                    match.group(1) in transitions and \
                    match.group(2) in transitions and \
                    match.group(3) in symbols:
                transitions[match.group(1)][match.group(3)] = match.group(2)
            else:
                print("not a valid input line ")
            line = stdin.readline()
    else:
        print("Improper formatting ")
        quit()

    #  print("got rules")
    return transitions


def get_start():
    start = stdin.readline()
    #  print("got start")
    return start.split()[1]


def get_final():
    final = stdin.readline()
    #  print("got final")
    return final.split()[1:]


def get_dfa():
    states = get_states()
    symbols = get_symbols()
    rules = get_rules(states, symbols)
    start = get_start()
    final = get_final()

    return DFA(
        init_state=start,
        final_states=final,
        transitions=rules,
        symbols=symbols
    )


def main():
    dfa = get_dfa()
    for line in stdin:
        print("accepted" if dfa.evaluate(line) else "rejected")


if __name__ == "__main__":
    main()