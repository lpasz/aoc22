defmodule RockPaperScissor do
  @inp File.read!("./src/day-02/inp.txt")

  @ex1_encrypt %{
    "A" => :rock,
    "B" => :paper,
    "C" => :scissor,
    "X" => :rock,
    "Y" => :paper,
    "Z" => :scissor
  }

  @ex2_encrypt %{"X" => :lose, "Y" => :tie, "Z" => :win}
  @win %{rock: :scissor, paper: :rock, scissor: :paper}
  @points %{rock: 1, paper: 2, scissor: 3, win: 6, lose: 0, tie: 3}

  def exp1 do
    @inp
    |> String.split("\n")
    |> Enum.reduce(0, fn line, acc ->
      [adv, you] = String.split(line, " ")
      acc + round_points(@ex1_encrypt[adv], @ex1_encrypt[you])
    end)
  end

  def exp2 do
    @inp
    |> String.split("\n")
    |> Enum.reduce(0, fn line, acc ->
      [adv, expected_result] = String.split(line, " ")
      adv = @ex1_encrypt[adv]
      you = my_play_to_get_result(adv, @ex2_encrypt[expected_result])
      acc + round_points(adv, you)
    end)
  end

  defp my_play_to_get_result(adv, result) do
    case result do
      :tie -> adv
      :win -> @win[@win[adv]]
      :lose -> @win[adv]
    end
  end

  defp round_points(adv, you) do
    @points[you] + @points[game_result(adv, you)]
  end

  defp game_result(adv, you) do
    cond do
      you == adv -> :tie
      @win[you] == adv -> :win
      :else -> :lose
    end
  end
end
