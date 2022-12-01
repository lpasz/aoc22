defmodule CaloriesCounting do
  def inp, do: File.read!("./src/day-01/inp.txt")

  defp elf_calories(text) do
    text
    |> String.split("\n")
    |> Enum.chunk_by(&(&1 == ""))
    |> Enum.reject(&(&1 == [""]))
    |> Enum.map(fn elfs ->
      elfs
      |> Enum.map(&String.to_integer/1)
      |> Enum.sum()
    end)
    |> Enum.sort(:desc)
  end

  def max_elf_cal() do
    inp()
    |> elf_calories()
    |> List.first()
  end

  def top_3_elf_cal() do
    inp()
    |> elf_calories()
    |> Enum.take(3)
    |> Enum.sum()
  end
end


CaloriesCounting.max_elf_cal() # 69528
CaloriesCounting.top_3_elf_cal() # 206152
