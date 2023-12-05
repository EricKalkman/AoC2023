with open('inputs/day04.inp') as f:
    puzzle_input = f.read().strip()

def part2():
    card_map = {}
    for line in puzzle_input.split('\n'):
        card_no_temp = line.split(':')[0].split(" ")[1:]
        card_no = int([x for x in card_no_temp if x != ""][0])
        winning_numbers = line.split(': ')[1].split(' |')[0].split(" ")
        [winning_numbers.remove(x) for x in winning_numbers if x == ""]  # Remove spurious spaces
        my_numbers = line.split(" | ")[1].split(" ")
        [my_numbers.remove(x) for x in my_numbers if x == ""]  # Remove spurious spaces

        win_count = 0
        for num in my_numbers:
            if num in winning_numbers:
                win_count += 1

        if card_no in card_map:
            card_map[card_no] += 1
        else:
            card_map[card_no] = 1

        for i in range(card_no+1, card_no+win_count+1):
            if i in card_map:
                card_map[i] += card_map[card_no]
            else:
                card_map[i] = card_map[card_no]

    print(sum(list(card_map.values())))
