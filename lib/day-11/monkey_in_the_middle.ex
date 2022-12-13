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
    |> Enum.reduce({nil, %{throws: 1}}, &parse_block_line/2)
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

  def ex1(monkeys) do
    scd =
      monkeys
      |> Enum.map(fn {_, %{div: div}} -> div end)
      |> Enum.reduce(1, &*/2)

    Enum.reduce(0..20, monkeys, fn _, monkeys ->
      Enum.reduce(monkeys, monkeys, fn {trowing_monkey, monkey}, monkeys ->
        monkey.items
        |> Enum.reduce(monkeys, fn item, monkeys ->
          worry_lvl = monkey.operation.(item)
          worry_lvl_calmed = div(worry_lvl, 3)
          # worry_lvl_after = rem(worry_lvl_calmed, scd)
          divisible? = rem(worry_lvl_calmed, monkey.div) == 0
          trow_to_monkey = monkey[divisible?]

          monkeys
          |> update_in([trow_to_monkey, :items], &[worry_lvl | &1])
          |> update_in([trowing_monkey, :items], &tl/1)
          |> update_in([trowing_monkey, :throws], & &1 + 1)
        end)
      end)
    end)
  end
end

ex_inp = File.read!("./lib/day-11/ex-inp.txt")
inp = File.read!("./lib/day-11/inp.txt")

ex_inp
|> MonkeyInTheMiddle.parse_input()
|> MonkeyInTheMiddle.ex1()
|> IO.inspect()

# inp |> MonkeyInTheMiddle.parse_input() |> IO.inspect()
