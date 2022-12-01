defmodule CaloriesCounting do
  @inp File.read!("./src/day-01/inp.txt")

  def max_elf_cal(), do: @inp |> elfs_calories() |> Enum.max()

  def top_3_elf_cal() do
    @inp |> elfs_calories() |> Enum.sort(:desc) |> Enum.take(3) |> Enum.sum()
  end

  defp elfs_calories(text), do: text |> String.split("\n\n") |> Enum.map(&sum_elf_calories/1)

  defp sum_elf_calories(elf) do
    elf |> String.split("\n") |> Enum.map(&String.to_integer/1) |> Enum.sum()
  end
end

69528 = CaloriesCounting.max_elf_cal()
206_152 = CaloriesCounting.top_3_elf_cal()
