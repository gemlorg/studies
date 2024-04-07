# Luna Language Interpreter

## Description

Luna is a Lua-like, imperative programming language that can be typed both statically and dynamically. Most of the syntax is taken from Lua, however there are some modifications, a.e. static typing.

## Syntax and Semantics

### Types

Luna supports:

- `nil` - special type that represents the absence of a value.
- `bool` - true and false values, used for logical operations, default value is false.
- `int` - signed integers, default value is 0
- `string` - arrays of characters, default value is an empty string.
- `fn({type}) -> type` - functions. `{type}` represents zero or more argument types, and `type` represents the return type. By default, return value of a function is `any`. Has no default value.
- `table\[type\]` - indexed arrays, used to store collections of values of the same `type`. type is any by default, and the default value is an empty table.
- `any` - union of all types above. Default value is `nil`.

Examples:

```lua
-- greeting: string
greeting = "Hello, World!"
```

```lua
-- add: fn(any, any) -> any
local fn add(a, b)
    return a
end
```

```lua
-- add: fn(int, int) -> int
local fn add(a: int, b: int) -> int
    return a + b
end
```

### Control Structures

Luna includes a small set of control structures, such as `if`, `while` and `for`.

```lua
if condition then
    ...
else
    ...
end
```

```lua
while condition do
    ...
end
```

```lua
for i = 1, 10,1 do
    ...
end
```

### Arithmetic Operators

Arithmetic operators apply to `int` values only and yield values of the same type.

```
+   sum
-   difference
*   product
/   quotient
%   remainder
-x  negation
```

### Comparison Operators

Comparison operators compare two operands and yield a `bool` value.

```
==  equal
!=  not equal
<   less
<=  less or equal
>   greater
>=  greater or equal
```

### Logical Operators

Logical operators apply to `bool` values and yield the result of the same type.

```
and conditional and
or  conditional or
not logical negaion
```

### Misc Operators

```
..  concatenates two strings
#   an unary operator that return the length of the a string or a table
```

### Variables

```lua
    j = 10         -- global variable
    local i = 1    -- local variable (to the chunk)
    local n: int   -- local variable with type int, value assigned to 0
    -- foo is a variable of type fn() -> (fn() -> int).
    local fn foo() -> (fn() -> int)
      local foo2 = fn() -> int
        return 1
      end
      return foo2
    end
```
