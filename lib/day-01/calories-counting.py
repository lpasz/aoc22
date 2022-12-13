inp = open('./lib/day-01/inp.txt').read()

elves = inp.split("\n\n")

max_calories = []

for elf in elves:
    snacks = elf.split('\n')
    total_calories = 0
    for snack in snacks:
        total_calories = total_calories + int(snack)
    max_calories.append(total_calories)

max_calories.sort(reverse=True)

print(max_calories[0])
print(max_calories[0] + max_calories[1] + max_calories[2])
