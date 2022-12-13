priorities = {}

for i in range(0, 26):
    priorities[chr(ord('a') + i)] = i + 1
    priorities[chr(ord('A') + i)] = i + 27


def split(ls, chunk_size):
    for i in range(0, len(ls), chunk_size):
        yield ls[i:i + chunk_size]


def elf_rucksack_priority(elf):
    item1 = set(list(elf[:len(elf)//2]))
    item2 = set(list(elf[len(elf)//2:]))
    misplaced = item1.intersection(item2).pop()

    return priorities[misplaced]


def badge_priority(elves):
    [elf1, elf2, elf3] = elves
    elf1 = set(list(elf1))
    elf2 = set(list(elf2))
    elf3 = set(list(elf3))
    badge = elf1.intersection(elf2).intersection(elf3).pop()

    return priorities[badge]


def ex1_and_ex2(inp):
    lines = inp.split("\n")

    rucksack_priority = 0
    elf_squad_priority = 0

    for elves in split(lines, 3):
        for elf in elves:
            rucksack_priority += elf_rucksack_priority(elf)

        elf_squad_priority += badge_priority(elves)

    return [rucksack_priority, elf_squad_priority]


if __name__ == '__main__':
    ex_inp = open('./lib/day-03/ex-inp.txt').read()
    inp = open('./lib/day-03/inp.txt').read()

    print(ex1_and_ex2(ex_inp))
    print(ex1_and_ex2(inp))
