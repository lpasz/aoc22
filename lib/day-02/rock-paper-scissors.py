EX1_DECRYPT = {'A': 'rock', 'B': 'paper', 'C': 'scissor',
               'X': 'rock', 'Y': 'paper', 'Z': 'scissor'}
EX2_DECRYPT = {'X': 'lose', 'Y': 'tie', 'Z': 'win'}
WIN = {'rock': 'scissor', 'paper': 'rock', 'scissor': 'paper'}
POINTS = {'rock': 1, 'paper': 2, 'scissor': 3, 'win': 6, 'lose': 0, 'tie': 3}


def round_result(your_play, adv_play):
    if adv_play == WIN[your_play]:
        return 'win'
    elif adv_play == your_play:
        return 'tie'
    else:
        return 'lose'


def play_to_achive_result(expected_result, adv_play):
    if result_2 == 'lose':
        return WIN[adv]
    elif result_2 == 'tie':
        return adv
    else:
        return WIN[WIN[adv]]


if __name__ == '__main__':
    inp = open('./lib/day-02/inp.txt').read()
    lines = inp.split("\n")
    exp1 = 0
    exp2 = 0

    for line in lines:
        [adv, me] = line.split(' ')
        adv = EX1_DECRYPT[adv]
        me_1 = EX1_DECRYPT[me]
        result_1 = round_result(me_1, adv)
        result_2 = EX2_DECRYPT[me]
        me_2 = play_to_achive_result(result_2, adv)

        exp1 += POINTS[me_1] + POINTS[result_1]
        exp2 += POINTS[me_2] + POINTS[result_2]

    print(exp1)  # 14531
    print(exp2)  # 11258
