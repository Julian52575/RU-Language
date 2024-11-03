# Ru-Compiler

Compiles ru code (.ru) into a ru executable.

## Installation:
Download the ru_compiler binary from the latest release.

## Usage:
```
  ./ru_compiler ru code
```

## Syntax
Find below the syntax for the ru language.

# 

---

- **Function Calls**
    
    ```jsx
    start_service("nginx");
    replace("foo", "bar", "data.txt");
    ```
    

---

- Function Definition
    
    ### Basic Typed Function
    
    In your language, functions can have explicit types for their parameters and return values. This makes the function signature clearer and helps catch errors early.
    
    Syntax:
    
    ```jsx
    fn add(x: int, y: int) -> int {
        return x + y;
    }
    ```
    
    Example:
    
    ```jsx
    fn add(x: int, y: int) -> int {
        return x + y;
    }
    
    let result = add(5, 3);
    print(result); // Output: 8
    ```
    
    ---
    
    ### Typed Functions with Default Parameters
    
    You can define default values for parameters, and you still specify the types.
    
    Syntax:
    
    ```jsx
    fn function_name(param1: type1 = default_value) -> return_type {
        // code
    }
    ```
    
    Exemple:
    
    ```jsx
    fn greet(name: string = "Guest") -> void {
        print("Hello, " + name);
    }
    
    greet();         // Output: Hello, Guest
    greet("Bob");    // Output: Hello, Bob
    ```
    
    ---
    
    ### Typed Inline or Anonymous Functions (Lambdas)
    
    Anonymous functions can also have types for their parameters and return values. These are useful for concise function definitions or passing as arguments.
    
    Syntax:
    
    ```jsx
    let my_function: (type1, type2) -> return_type = (param1, param2) => {
        return value;
    };
    ```
    
    Example:
    
    ```jsx
    let add: (int, int) -> int = (x, y) => x + y;
    print(add(3, 4)); // Output: 7
    ```
    
    ---
    
    ### Typed Recursive Functions
    
    Recursive functions can also be typed. You need to specify the parameter and return types.
    
    Syntax:
    
    ```jsx
    fn function_name(param: type) -> return_type {
        if base_case {
            return value;
        } else {
            return function_name(modified_param);
        }
    }
    ```
    
    Exemple:
    
    ```jsx
    fn factorial(n: int) -> int {
        if n == 1 {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
    }
    
    print(factorial(5)); // Output: 120
    ```
    
    ---
    
    ### Typed Higher-Order Functions
    
    Higher-order functions that take other functions as arguments or return functions can also have types specified.
    
    Syntax:
    
    ```jsx
    fn high_order_function(callback: (param_type) -> return_type) -> return_type {
        return callback(param);
    }
    ```
    
    Exemple:
    
    ```jsx
    fn apply_twice(func: (int) -> int, value: int) -> int {
        return func(func(value));
    }
    
    let add_one: (int) -> int = (x: int) => x + 1;
    print(apply_twice(add_one, 5)); // Output: 7
    ```
    
    ---
    
    ### Typed Variadic Functions
    
    You can also type variadic functions. The type of the arguments is specified as an array or list of a given type.
    
    Syntax:
    
    ```jsx
    fn function_name(...params: [type]) -> return_type {
        // code to handle multiple params
    }
    ```
    
    Exemple:
    
    ```jsx
    fn print_all(...args: [string]) -> void {
        for arg in args {
            print(arg);
        }
    }
    
    print_all("Hello", "World", "Foo", "Bar");
    // Output:
    // Hello
    // World
    // Foo
    // Bar
    ```
    
    ---
    
    ### Typed Function Overloading
    
    Overloading functions allows you to define multiple functions with the same name but different types. The correct function is chosen based on the types of the arguments.
    
    Syntax:
    
    ```jsx
    fn add(x: int, y: int) -> int {
        return x + y;
    }
    
    fn add(x: float, y: float) -> float {
        return x + y;
    }
    ```
    
    Exemple:
    
    ```jsx
    print(add(1, 2));         // Calls the int version, Output: 3
    print(add(1.5, 2.5));     // Calls the float version, Output: 4.0
    ```
    
    ---
    
    ### Nested Typed Functions
    
    You can also define typed functions within other functions, allowing for local logic while maintaining type safety.
    
    Syntax:
    
    ```jsx
    fn outer_function(param: type) -> return_type {
        fn inner_function(inner_param: type) -> return_type {
            // code inside inner function
        }
        return inner_function(param);
    }
    ```
    
    Exemple:
    
    ```jsx
    fn greet(name: string) -> void {
        fn say_hello() -> void {
            print("Hello, " + name);
        }
        say_hello();
    }
    
    greet("Alice"); // Output: Hello, Alice
    ```
    
    ---
    
    ### Example of Complex Typed Function
    
    Here’s a more complex example combining multiple concepts, such as a higher-order function, recursion, and default parameters:
    
    Syntax:
    
    ```jsx
    fn repeat(func: (int) -> int, value: int, times: int = 2) -> int {
        if times == 0 {
            return value;
        } else {
            return repeat(func, func(value), times - 1);
        }
    }
    
    let add_one: (int) -> int = (x: int) => x + 1;
    print(repeat(add_one, 5));  // Output: 7 (add_one applied twice)
    ```
    
    ---
    
    ### Typed Lambdas (Anonymous Functions)
    
    **Lambdas**, or anonymous functions, are small unnamed functions that can be defined in-line. In your language, lambdas are explicitly typed for both their parameters and return values. They are useful when you need a quick function, such as passing it as an argument to higher-order functions or returning it from a function.
    
    Syntax:
    
    ```jsx
    let function_name: (type1, type2) -> return_type = (param1: type1, param2: type2) => {
        // code
        return expression;
    };
    ```
    
    Example:
    
    ```jsx
    let add: (int, int) -> int = (x: int, y: int) => {
        return x + y;
    };
    
    print(add(3, 4)); // Output: 7
    ```
    
    In this example:
    
    - `add` is a lambda that takes two `int` parameters and returns an `int` value. The types are explicitly declared in the lambda's signature.
    
    ---
    
    ### Passing Typed Lambdas as Arguments
    
    Typed lambdas can be passed as arguments to higher-order functions. This is useful when applying a function to a collection or when a function needs behavior that can vary depending on a provided lambda.
    
    Exemple:
    
    ```jsx
    fn apply_twice(func: (int) -> int, value: int) -> int {
        return func(func(value));
    }
    
    let increment: (int) -> int = (x: int) => {
        return x + 1;
    };
    
    print(apply_twice(increment, 5)); // Output: 7
    ```
    
    In this example:
    
    - The lambda `increment` is passed to `apply_twice`, and it adds 1 to the value twice.
    
    ---
    
    ### Returning Typed Lambdas from Functions
    
    Typed lambdas can also be returned from functions, allowing dynamic or customized behavior.
    
    Example:
    
    ```jsx
    fn create_multiplier(factor: int) -> (int) -> int {
        return (x: int) => {
            return x * factor;
        };
    }
    
    let double: (int) -> int = create_multiplier(2);
    print(double(5)); // Output: 10
    ```
    
    In this case:
    
    - `create_multiplier` returns a lambda that multiplies its input by a predefined factor, which can be customized by the caller.

---

- Loops
    
    ### Basic `for` Loop
    
    The `for` loop is used to iterate over a range of values or perform a block of code a specific number of times.
    
    Syntax:
    
    ```jsx
    for i in start..end {
        // code to execute for each value of i
    }
    ```
    
    Exemple:
    
    ```jsx
    for i in 0..10 {
        print(i); // prints values from 0 to 9
    }
    ```
    
    ---
    
    ### C-style `for` Loop
    
    This is a classic `for` loop where you have full control over the initialization, condition, and increment.
    
    Syntax:
    
    ```jsx
    for (let i = start; i < end; i = i + step) {
        // code to execute for each value of i
    }
    ```
    
    Exemple:
    
    ```jsx
    for (let i = 0; i < 10; i = i + 1) {
        print(i); // prints values from 0 to 9, increments by 1
    }
    ```
    
    ---
    
    ### Range-based `for` with Step
    
    You can customize the step value to control the iteration increment or decrement.
    
    Syntax:
    
    ```jsx
    for i in start..end step value {
        // code to execute for each value of i
    }
    ```
    
    Exemple:
    
    ```jsx
    for i in 0..10 step 2 {
        print(i); // prints values 0, 2, 4, 6, 8
    }
    ```
    
    ---
    
    ### `while` Loop
    
    The `while` loop repeats a block of code as long as a condition is true.
    
    Syntax:
    
    ```jsx
    while condition {
        // code to execute while the condition is true
    }
    ```
    
    Exemple:
    
    ```jsx
    let i = 0;
    while i < 10 {
        print(i);
        i = i + 1;
    }
    ```
    
    ---
    
    ### `do-while` Loop
    
    The `do-while` loop is similar to the `while` loop, but it guarantees that the code inside the loop is executed at least once.
    
    Syntax:
    
    ```jsx
    do {
        // code to execute at least once
    } while condition;
    ```
    
    Exemple:
    
    ```jsx
    let i = 0;
    do {
        print(i);
        i = i + 1;
    } while i < 10;
    ```
    
    ---
    
    ### Infinite `while true` Loop
    
    An infinite loop can be created using a `while true` structure. You can use `break` to exit the loop manually.
    
    Syntax:
    
    ```jsx
    while true {
        // infinite loop, must be manually stopped
    }
    ```
    
    Exemple:
    
    ```jsx
    while true {
        print("This will run forever");
        if condition {
            break; // breaks the infinite loop
        }
    }
    ```
    
    ---
    
    ### `for` with Inclusive Range
    
    Use an inclusive range to iterate over values where the final value is included in the iteration.
    
    Syntax:
    
    ```jsx
    for i in start..=end {
        // code to execute for each value of i, including the end value
    }
    ```
    
    Exemple:
    
    ```jsx
    for i in 0..=10 {
        print(i); // prints values from 0 to 10, including 10
    }
    ```
    
    ---
    
    ### `until` Loop
    
    The `until` loop is the inverse of a `while` loop, where the code runs until a certain condition becomes true.
    
    Syntax:
    
    ```jsx
    until condition {
        // code to execute until the condition is true
    }
    ```
    
    Exemple:
    
    ```jsx
    let i = 0;
    until i == 10 {
        print(i);
        i = i + 1;
    }
    ```
    
    ---
    
    ### Using `break` and `continue`
    
    `break` and `continue` provide additional control within loops. `break` exits the loop, while `continue` skips the current iteration and moves to the next one.
    
    Syntax:
    
    ```jsx
    while condition {
        if some_condition {
            break; // exit the loop
        }
    
        if another_condition {
            continue; // skip to the next iteration
        }
    
        // code here is executed if neither condition is met
    }
    ```
    
    Exemple:
    
    ```jsx
    let i = 0;
    while i < 10 {
        i = i + 1;
    
        if i == 5 {
            continue; // skips printing 5
        }
    
        if i == 8 {
            break; // stops the loop when i is 8
        }
    
        print(i);
    }
    ```
    
    ---
    
    ### `while let` Loop
    
    A `while let` loop is useful for iterating while dynamically handling values, such as processing data streams or options.
    
    Syntax:
    
    ```jsx
    while let Some(value) = optional_value {
        // code to execute while there's a value
    }
    ```
    
    Exemple:
    
    ```jsx
    while let Some(x) = get_next_value() {
        print(x); // prints values as long as get_next_value returns Some(x)
    }
    ```
    

---

- Conditions
    
    ### Basic `if-else`
    
    The `if-else` statement checks a condition and executes a block of code if the condition is true. If the condition is false, the `else` block will be executed.
    
    Syntax:
    
    ```jsx
    if condition {
        // code if condition is true
    } else {
        // code if condition is false
    }
    ```
    
    Exemple:
    
    ```jsx
    let age = 25;
    
    if age > 18 {
        print("Adult");
    } else if age == 18 {
        print("Exactly 18");
    } else {
        print("Underage");
    }
    ```
    
    ---
    
    ### Ternary Expression
    
    A **ternary expression** is a concise way to return a value based on a condition, similar to Rust or C's ternary operator. It is useful for simple conditions where you don't want to write a full `if-else` block.
    
    Syntax
    
    ```jsx
    let result = (condition) ? true_value : false_value;
    ```
    
    Example
    
    ```jsx
    let max_value = (x > y) ? x : y;
    print(max_value); // prints the greater value
    ```
    
    ---
    
    ### Logical Operators (`and`, `or`, `not`)
    
    Logical operators allow you to combine multiple conditions. They include `and`, `or`, and `not` (or you can use `&&`, `||`, `!` like in C).
    
    Syntax
    
    ```jsx
    if condition1 and condition2 {
        // code if both conditions are true
    }
    
    if not condition {
        // code if the condition is false
    }
    
    ```
    
    Example
    
    ```jsx
    if x > 10 and y < 5 {
        print("x is greater than 10 and y is less than 5");
    }
    
    if not (x == y) {
        print("x is not equal to y");
    }
    ```
    
    ---
    
    ### Pattern Matching (`match`)
    
    **Pattern matching** is a powerful tool that allows you to match values against multiple patterns. It’s like a `switch` statement but more expressive, especially when working with enums or specific data structures.
    
    Syntax
    
    ```jsx
    match value {
        pattern1 => action1,
        pattern2 => action2,
        _ => default_action, // _ is the default case
    }
    ```
    
    Example
    
    ```elixir
    match x {
        0 => print("x is zero"),
        1 | 2 => print("x is either 1 or 2"), // matches 1 or 2
        _ => print("x is something else"),    // default case
    }
    ```
    
    Example with Guard
    
    ```jsx
    match x {
        0 if y > 10 => print("x is zero and y is greater than 10"),
        1 | 2 => print("x is either 1 or 2"),
        _ => print("x is something else"),
    }
    
    ```
    
    ---
    
    ### Nested Conditions
    
    Conditions can be **nested** within each other, allowing for more complex logic in decision-making.
    
    Syntax
    
    ```jsx
    if condition1 {
        if condition2 {
            // code if both conditions are true
        } else {
            // code if only condition1 is true
        }
    } else {
        // code if condition1 is false
    }
    ```
    
    Example
    
    ```jsx
    if x > 10 {
        if y > 5 {
            print("x is greater than 10 and y is greater than 5");
        } else {
            print("x is greater than 10 but y is not greater than 5");
        }
    } else {
        print("x is less than or equal to 10");
    }
    ```
    
    ---
    
    ### `switch` or `case` Statement
    
    Introduce a **switch** or **case** statement as an alternative to `if-else` for handling multiple values in a cleaner way.
    
    Syntax
    
    ```jsx
    switch variable {
        case value1:
            // code for value1
        case value2:
            // code for value2
        default:
            // default code
    }
    ```
    
    Example
    
    ```jsx
    switch x {
        case 1:
            print("x is 1");
        case 2:
            print("x is 2");
        default:
            print("x is something else");
    }
    ```
    
    ---
    
    ### Error Handling with Conditionals
    
    Handle errors and optional values directly in conditions, similar to how Rust handles `Option` and `Result` types.
    
    Syntax
    
    ```jsx
    if value is Some(result) {
        // code if there is a result
    } else {
        // code if value is None
    }
    ```
    
    Example
    
    ```jsx
    let result = get_data();
    
    if result is Some(value) {
        print("Data received: " + value);
    } else {
        print("No data found");
    }
    ```
    
    ---
    
    ### Inline or Ternary-like Conditions for Assignments
    
    Inline conditionals allow you to assign values based on a condition in one line.
    
    Syntax
    
    ```jsx
    let variable = (condition) ? true_value : false_value;
    ```
    
    Example
    
    ```jsx
    let status = (x > y) ? "x is greater" : "y is greater or equal";
    print(status);
    ```
    

---
