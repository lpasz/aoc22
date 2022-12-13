defmodule CampCleanup do
  def ex1(text) do
    count_overlaps(text, fn range1, range2 ->
      MapSet.subset?(range1, range2) or MapSet.subset?(range2, range1)
    end)
  end

  def ex2(text) do
    count_overlaps(text, fn range1, range2 ->
      not Enum.empty?(MapSet.intersection(range1, range2))
    end)
  end

  defp count_overlaps(text, overlapped?) do
    text
    |> String.split(~r/,|-|\n/)
    |> Enum.map(&String.to_integer/1)
    |> Enum.chunk_every(4)
    |> Enum.count(fn [x1, x2, y1, y2] -> overlapped?.(MapSet.new(x1..x2), MapSet.new(y1..y2)) end)
  end
end

ex_inp = File.read!("./lib/day-04/ex-inp.txt")
inp = File.read!("./lib/day-04/inp.txt")

# 2
ex_inp |> CampCleanup.ex1() |> IO.puts()
# 595
inp |> CampCleanup.ex1() |> IO.puts()
# 4
ex_inp |> CampCleanup.ex2() |> IO.puts()
# 952
inp |> CampCleanup.ex2() |> IO.puts()
