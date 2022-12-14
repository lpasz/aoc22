defmodule MonkeyInTheMiddle do
  def parse_input(text) do
    text
    |> String.split("\n\n")
    |> Enum.map(&parse_block/1)
    |> Enum.into(%{})
  end

  defp parse_block(block) do
    block
    |> String.split("\n")
    |> Enum.reduce({nil, %{throws: 0}}, &parse_block_line/2)
  end

  defp parse_block_line(line, {idx, acc}) do
    case line do
      "Monkey " <> rest ->
        {idx, _} = rest |> Integer.parse()
        {idx, acc}

      "  Starting items: " <> rest ->
        starting_items = rest |> String.split(", ") |> Enum.map(&String.to_integer/1)
        {idx, Map.put(acc, :items, starting_items)}

      "  Operation: new =" <> rest ->
        throw_to = Code.eval_string("fn old -> " <> rest <> " end") |> elem(0)
        {idx, Map.put(acc, :operation, throw_to)}

      "  Test: divisible by " <> rest ->
        {idx, Map.put(acc, :div, String.to_integer(rest))}

      "    If true: throw to monkey " <> rest ->
        {idx, Map.put(acc, true, String.to_integer(rest))}

      "    If false: throw to monkey " <> rest ->
        {idx, Map.put(acc, false, String.to_integer(rest))}

      _ ->
        {idx, acc}
    end
  end

  def monkeys_turn(monkeys, rounds, calm_with) do
    Enum.reduce(1..rounds, monkeys, fn _round, monkeys ->
      Enum.reduce(monkeys, monkeys, fn {trowing_monkey, _old_monkey}, monkeys ->
        monkey = monkeys[trowing_monkey]

        monkey.items
        |> Enum.reduce(monkeys, fn item, monkeys ->
          worry_lvl = monkey.operation.(item)
          worry_lvl_calmed = calm_with.(worry_lvl)
          divisible? = rem(worry_lvl_calmed, monkey.div) == 0
          throw_to = monkey[divisible?]

          update_in(monkeys, [throw_to, :items], &[worry_lvl_calmed | &1])
        end)
        |> update_in([trowing_monkey, :throws], &(&1 + length(monkey.items)))
        |> put_in([trowing_monkey, :items], [])
      end)
    end)
    |> Enum.map(fn {_k, v} -> v.throws end)
    |> Enum.sort(&>/2)
    |> Enum.take(2)
    |> Enum.reduce(1, &*/2)
  end

  def ex1(monkeys) do
    monkeys_turn(monkeys, 20, &div(&1,3))
  end

  def ex2(monkeys) do
    scd =
      monkeys
      |> Enum.map(fn {_, %{div: div}} -> div end)
      |> Enum.reduce(1, &*/2)

    monkeys_turn(monkeys, 10000, &rem(&1,scd))
  end
end

ex_inp = File.read!("./lib/day-11/ex-inp.txt")
inp = File.read!("./lib/day-11/inp.txt")



# 10605
ex_inp
|> MonkeyInTheMiddle.parse_input()
|> MonkeyInTheMiddle.ex1()
|> IO.inspect()

# 55458
inp
|> MonkeyInTheMiddle.parse_input()
|> MonkeyInTheMiddle.ex1()
|> IO.inspect()

# 2713310158
ex_inp
|> MonkeyInTheMiddle.parse_input()
|> MonkeyInTheMiddle.ex2()
|> IO.inspect()

# 14508081294
inp
|> MonkeyInTheMiddle.parse_input()
|> MonkeyInTheMiddle.ex2()
|> IO.inspect()
