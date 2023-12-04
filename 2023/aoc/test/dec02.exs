defmodule Dec02Test do
  use ExUnit.Case
  doctest Dec02

  test "dec 02" do
    input = File.read!("../../inputs/2023/02.txt")
    assert Dec02.a(input) == 2278
    assert Dec02.b(input) == 67953
  end
end
