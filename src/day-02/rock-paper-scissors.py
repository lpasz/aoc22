inp = open('./src/day-02/inp.txt').read()
ex1_decrypt = {'A': 'rock', 'B': 'paper', 'C': 'scissor',
               'X': 'rock', 'Y': 'paper', 'Z': 'scissor'}
ex2_decrypt = {'X': 'lose', 'Y': 'tie', 'Z': 'win'}
win = {'rock': 'scissor', 'paper': 'rock', 'scissor': 'paper'}
points = {'rock': 1, 'paper': 2, 'scissor': 3, 'win': 6, 'lose': 0, 'tie': 3}
lines = inp.split("\n")
exp1 = 0
exp2 = 0

for line in lines:
    [adv, me] = line.split(' ')
    adv = ex1_decrypt[adv]
    me_1 = ex1_decrypt[me]
    me_2 = ''
    result_1 = ''
    result_2 = ex2_decrypt[me]

    if adv == win[me_1]:
        result_1 = 'win'
    elif adv == me_1:
        result_1 = 'tie'
    else:
        result_1 = 'lose'

    if result_2 == 'lose':
        me_2 = win[adv]
    elif result_2 == 'tie':
        me_2 = adv
    else:
        me_2 = win[win[adv]]

    exp1 += points[me_1] + points[result_1]
    exp2 += points[me_2] + points[result_2]

print(exp1)
print(exp2)
