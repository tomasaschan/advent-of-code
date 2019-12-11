use super::IntcodeComputer;
use std::fs::File;
use std::io::prelude::*;

const RELATIVE_BASE_ADDR: i128 = -1;

impl IntcodeComputer {
  pub fn step(&mut self) {
    let op = self.memory.get(&self.instruction_pointer).unwrap_or(&0) % 100;
    let mode = self.memory.get(&self.instruction_pointer).unwrap_or(&0) / 100;
    match op {
      1 => self.binary_op(|a, b| a + b, mode),
      2 => self.binary_op(|a, b| a * b, mode),
      3 => self.read(mode),
      4 => self.write(mode),
      5 => self.jmp_if(true, mode),
      6 => self.jmp_if(false, mode),
      7 => self.cmp(|a, b| a < b, mode),
      8 => self.cmp(|a, b| a == b, mode),
      9 => self.set_rel_base(mode),
      _ => {
        let mut buffer =
          File::create("core.dump").expect("core dump failed; could not create file core.dump.");
        write!(buffer, "{:?}", self.memory).expect("core dump failed; could not write to file.");
        panic!(
          "invalid opcode {} at position {}. memory dumped to core.tmp",
          op, self.instruction_pointer
        )
      }
    };
  }

  fn binary_op<F: Fn(i128, i128) -> i128>(&mut self, op: F, mode: i128) {
    let a = self.value_at_offset(1, param_mode(mode, 0));
    let b = self.value_at_offset(2, param_mode(mode, 1));
    self.set_at_offset(3, op(a, b), param_mode(mode, 2));
    self.step_pointer(4);
  }
  fn jmp_if(&mut self, t: bool, mode: i128) {
    let a = self.value_at_offset(1, param_mode(mode, 0));

    if t ^ (a == 0) {
      let p = self.value_at_offset(2, param_mode(mode, 1));
      self.instruction_pointer = p;
    } else {
      self.step_pointer(3);
    }
  }

  fn cmp<F: Fn(i128, i128) -> bool>(&mut self, comparer: F, mode: i128) {
    let a = self.value_at_offset(1, param_mode(mode, 0));
    let b = self.value_at_offset(2, param_mode(mode, 1));

    self.set_at_offset(3, if comparer(a, b) { 1 } else { 0 }, param_mode(mode, 2));
    self.step_pointer(4);
  }

  fn set_rel_base(&mut self, mode: i128) {
    let prev = *self.memory.get(&RELATIVE_BASE_ADDR).unwrap_or(&0);
    let diff = self.value_at_offset(1, param_mode(mode, 0));
    self.memory.insert(RELATIVE_BASE_ADDR, prev + diff);
    self.step_pointer(2);
  }

  fn read(&mut self, mode: i128) {
    let value = self.input.recv().expect("Failure when reading from input");
    self.set_at_offset(1, value, param_mode(mode, 0));
    self.step_pointer(2);
  }

  fn write(&mut self, mode: i128) {
    let v = self.value_at_offset(1, param_mode(mode, 0));
    self.output.send(v).expect("Failure when writing to output");
    self.step_pointer(2);
  }
  fn value_at_offset(&mut self, offset: i128, mode: ParamMode) -> i128 {
    let loc = match mode {
      ParamMode::Position => *self
        .memory
        .get(&(self.instruction_pointer + offset))
        .unwrap_or(&0),
      ParamMode::Immediate => self.instruction_pointer + offset,
      ParamMode::Relative => {
        self.memory.get(&(RELATIVE_BASE_ADDR)).unwrap_or(&0)
          + self
            .memory
            .get(&(self.instruction_pointer + offset))
            .unwrap_or(&0)
      }
    };
    *self.memory.get(&loc).unwrap_or(&0)
  }
  fn set_at_offset(&mut self, offset: i128, value: i128, mode: ParamMode) {
    let loc = match mode {
      ParamMode::Position => *self
        .memory
        .get(&(self.instruction_pointer + offset))
        .unwrap_or(&0),
      ParamMode::Relative => {
        *self.memory.get(&(RELATIVE_BASE_ADDR)).unwrap_or(&0)
          + *self
            .memory
            .get(&(self.instruction_pointer + offset))
            .unwrap_or(&0)
      }
      ParamMode::Immediate => panic!(
        "Tried to write in immediate mode! IP: {}",
        self.instruction_pointer
      ),
    };
    self.memory.insert(loc, value);
  }

  fn step_pointer(&mut self, steps: i128) {
    self.instruction_pointer = self.instruction_pointer + steps;
  }
  pub fn set_pointer(&mut self, loc: i128) {
    self.instruction_pointer = loc;
  }
}

#[derive(Debug, PartialEq)]
enum ParamMode {
  Immediate,
  Position,
  Relative,
}

fn param_mode(mut mode: i128, mut n: i128) -> ParamMode {
  loop {
    if n == 0 {
      break match mode % 10 {
        0 => ParamMode::Position,
        1 => ParamMode::Immediate,
        2 => ParamMode::Relative,
        _ => panic!("invalid parameter mode {}", mode),
      };
    }
    mode /= 10;
    n -= 1;
  }
}

#[test]
fn param_mode_test() {
  assert_eq!(param_mode(0, 0), ParamMode::Position);
  assert_eq!(param_mode(0, 1), ParamMode::Position);
  assert_eq!(param_mode(0, 2), ParamMode::Position);
  assert_eq!(param_mode(1, 0), ParamMode::Immediate);
  assert_eq!(param_mode(1, 1), ParamMode::Position);
  assert_eq!(param_mode(1, 2), ParamMode::Position);
  assert_eq!(param_mode(10, 0), ParamMode::Position);
  assert_eq!(param_mode(10, 1), ParamMode::Immediate);
  assert_eq!(param_mode(10, 2), ParamMode::Position);
  assert_eq!(param_mode(11, 0), ParamMode::Immediate);
  assert_eq!(param_mode(11, 1), ParamMode::Immediate);
  assert_eq!(param_mode(11, 2), ParamMode::Position);
  assert_eq!(param_mode(110, 0), ParamMode::Position);
  assert_eq!(param_mode(110, 1), ParamMode::Immediate);
  assert_eq!(param_mode(110, 2), ParamMode::Immediate);
  assert_eq!(param_mode(111, 0), ParamMode::Immediate);
  assert_eq!(param_mode(111, 1), ParamMode::Immediate);
  assert_eq!(param_mode(111, 2), ParamMode::Immediate);
}
