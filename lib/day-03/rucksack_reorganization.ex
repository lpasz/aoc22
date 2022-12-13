defmodule RucksackReorganizations do
  @priorities 0..25
              |> Enum.flat_map(fn i ->
                char_to_string = &List.to_string([&1])
                [{char_to_string.(?a + i), i + 1}, {char_to_string.(?A + i), i + 27}]
              end)
              |> Enum.into(%{})

  def ex1(text) do
    text
    |> String.split("\n")
    |> Enum.reduce(0, fn line, acc ->
      half = line |> String.length() |> div(2)
      {item1, item2} = String.split_at(line, half)
      priority = intersection_priority([item1, item2])

      @priorities[priority] + acc
    end)
  end

  def ex2(text) do
    text
    |> String.split("\n")
    |> Enum.chunk_every(3)
    |> Enum.reduce(0, &(@priorities[intersection_priority(&1)] + &2))
  end

  defp intersection_priority(list) do
    list
    |> Enum.map(&MapSet.new(String.codepoints(&1)))
    |> Enum.reduce(&MapSet.intersection/2)
    |> MapSet.to_list()
    |> List.first()
  end
end

ex_inp = File.read!("./lib/day-03/ex-inp.txt")
inp = File.read!("./lib/day-03/inp.txt")

ex_inp |> RucksackReorganizations.ex1() |> IO.puts()
inp |> RucksackReorganizations.ex1() |> IO.puts()
ex_inp |> RucksackReorganizations.ex2() |> IO.puts()
inp |> RucksackReorganizations.ex2() |> IO.puts()
