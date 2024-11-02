let x: int = (10 + 10) * ((2 / 5) % 12);
print(x);
pritnLn(x);

let result: int = add(1, 1);

if ( result == x ) {
  print("Equal!");
} else {
  print("Unequal!");
}

for i in 1..=10 {
  print(i);
}

let n: int = 0;
while n < 500 {
  printLn(n);
  n = add(n, 1);
}

let str: string
  ="Bonjour";
match str {
  "Hello" => printLn("English."),
  "Bonjour" => printLn("French."),
  _         => printLn("Unknow.")
}


fn add(a: int, b: int) -> int {
    return a + b;
}
