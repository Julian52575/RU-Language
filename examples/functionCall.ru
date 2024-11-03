printLn("Pov: Inoxtag et l'everest");
fn step2half() -> void {
  print("Hold on...")
  ;
}
fn step2() -> void {
  printLn("Step2");
  step2half();
  printLn("Ok good");
  step3();
}
fn step1() -> void {
  printLn("Step1");
  step2();
  return("Done");
}
step1();
fn step3() -> void {
  printLn("Step 3");
}
