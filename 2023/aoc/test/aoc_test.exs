defmodule AoCTest do
  use ExUnit.Case
  doctest Dec01
  doctest Dec02
  doctest Dec03

  test "dec 01" do
    input = File.read!("../../inputs/2023/01.txt")
    assert Dec01.a(input) == 55386
    assert Dec01.b(input) == 54824
  end

  test "dec 02" do
    input = File.read!("../../inputs/2023/02.txt")
    assert Dec02.a(input) == 2278
    assert Dec02.b(input) == 67953
  end

  test "dec 03" do
    input = File.read!("../../inputs/2023/03.txt")
    assert Dec03.a(input) == 536_576
    assert Dec03.b(input) == 75_741_499
  end
end
