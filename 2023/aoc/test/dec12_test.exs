defmodule Dec12Test do
  use ExUnit.Case
  doctest Dec12

  @tag timeout: :infinity
  test "Dec 12" do
    input = File.read!("../../inputs/2023/12.txt")

    assert Dec12.a(input) == 7350
    assert Dec12.b(input) == 0
  end
end
