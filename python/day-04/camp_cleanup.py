import re


def ex1_and_ex2(text):
    items = re.compile(',|-|\n').split(text)

    ex1, ex2 = 0, 0

    for idx in range(0, len(items), 4):
        x1, x2 = int(items[idx]), int(items[idx + 1]) + 1
        y1, y2 = int(items[idx + 2]), int(items[idx + 3]) + 1
        range1, range2 = set(range(x1, x2)), set(range(y1, y2))

        if range1.issubset(range2) or range2.issubset(range1):
            ex1 += 1

        if set() != range1.intersection(range2):
            ex2 += 1

    return ex1, ex2


if __name__ == '__main__':
    ex_inp = open('./lib/day-04/ex-inp.txt').read()
    inp = open('./../inputs/day-04/inp.txt').read()

    print(ex1_and_ex2(ex_inp))  # (2, 4)
    print(ex1_and_ex2(inp))  # (595, 952)
