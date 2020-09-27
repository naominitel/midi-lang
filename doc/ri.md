## Intermediate representation

This document describes the intermediate representation (~bytecode) used as an
exchange format between the compiler and the runtime. In the current state of
things, code is not really compiled and the bytecode is more of a binary
serialized AST of a somewhat lower level.

> Note: All multiple byte data are little endian.

### Header

Code is currently transmitted from the compiler to the runtime via the `update`
message transmitting a global function definition. An `update` message begins by
the common 8 bytes message header:

| Bytes | Description  | Type |
| ----- | ------------ | ---- |
| 0-3   | message size | u32  |
| 4     | message type | u8   |
| 5-7   | reserved     |      |

Message type for `update` messages is `0x10`.

Follow 56 bytes of special `update` header (total header size is 64 bytes):

| Bytes | Description    | Type  |
| ----- | -------------- | ----- |
| 8-11  | function name  | ptr32 |
| 12    | inputs number  | u8    |
| 13    | outputs number | u8    |
| 14-15 | locals number  | u16   |
| 16-31 | outputs        | u16*  |
| 32-35 | code sec size  | u32   |
| 36-39 | cst sec size   | u32   |
| 40-47 | unused         | -     |
| 48-63 | unused         | -     |

> ptr32 is a u32 that points to the address (in number of bytes from beginning
> of the message) in the constant section where the name string is stored.

* Function name: Name of the function, as specified in user program text
* Inputs number: Number of arguments this function takes
* Outputs number: Number of return values. Should be 0 for functions for now.
* Locals number: Number of local context variables.
* Outputs: For each return value, the index in the code section of the node for
  computing that value, each as a node number (u16), as many as outputs number.
* Code sec size: Total size of the code section (in number of bytes)
* Cst sec size: Total size of the constant section (in number of bytes)

### Code section

Starting at byte 64 begins the code section. This section is a succession of
nodes encoding an expression. Each node can be of variable length depending on
the type, but is always a multiple of 8 bytes.

A *node number* refers to a node in this section by its index (node number of 1
refers to the second node of the section, no matter the size of node number 0).

The first byte of a node gives its type:

| Op     | Code |
| ------ | ---- |
| Nop    | 0x00 |
| Cons   | 0x01 |
| Get    | 0x02 |
| Set    | 0x03 |
| Glob   | 0x04 |
| Lambda | 0x05 |
| Pre    | 0x08 |
| Add    | 0x10 |
| Sub    | 0x11 |
| Mul    | 0x12 |
| Div    | 0x13 |
| Mod    | 0x14 |
| Min    | 0x17 |
| Eq     | 0x18 |
| Neq    | 0x19 |
| Ge     | 0x1A |
| Le     | 0x1B |
| Gt     | 0x1C |
| Lt     | 0x1D |
| And    | 0x20 |
| Or     | 0x21 |
| Not    | 0x27 |
| Fby    | 0x28 |
| If     | 0x40 |
| Call   | 0x50 |
| Indx   | 0x54 |
| Fld    | 0x58 |
| Poly   | 0x60 |
| Mono   | 0x68 |

#### Node Types

* Nop

Size: variable

| Byte          | Description   | Type               |
| ------------- | ------------- | ------------------ |
| 0             | Opcode (0x00) | u8                 |
| 1             | Seq length    | u8                 |
| 2-3, 3-4, ... | Seq nodes     | u16                |

`Nop` evaluates a series of expressions, in sequence. Seq length gives the
number of expressions, minus 1 (so minimum number of expressions is always 1).
Seq nodes are the node numbers of the expressions, in order

* Cons

Size: variable

| Byte          | Description   | Type               |
| ------------- | ------------- | ------------------ |
| 0             | Opcode (0x01) | u8                 |
| 1             | Data type     | u8                 |

`Cons` evaluates to a static constant. The rest of the structure depends on the
data type:

** Int (data type = 0x1, total size: 16)

A 64 bits signed integer

| Byte          | Description   | Type               |
| ------------- | ------------- | ------------------ |
| 2-7           | Unused        | -                  |
| 8-15          | Value         | i64                |

** Bool (data type = 0x2, total size: 8)

A boolean

| Byte          | Description   | Type                     |
| ------------- | ------------- | ------------------------ |
| 2             | Value         | u8 (1 = True, 0 = False) |
| 3-7           | Unused        | -                        |

** Gate (data type = 0x8, total size: 8)

A gate information (for MIDI data)

| Byte          | Description   | Type                          |
| ------------- | ------------- | ----------------------------- |
| 2             | Value         | u8 (2 = Tie, 1 = On, 0 = Off) |
| 3-7           | Unused        | -                             |

** Str (data type = 0x10, total size: 8)

A static string (as address of string stored in constant section)

| Byte          | Description   | Type                          |
| ------------- | ------------- | ----------------------------- |
| 2-3           | Unused        | -                             |
| 4-7           | Value         | ptr32                         |

* Get

Size: 8

| Byte          | Description   | Type               |
| ------------- | ------------- | ------------------ |
| 0             | Opcode (0x02) | u8                 |
| 1             | Unused        | u8                 |
| 2-3           | Var index     | u16                |
| 4-5           | Var depth     | u16                |
| 6-7           | Unused        | u16                |

Evaluates to the value of a context variable (local variables of functions). The
variable is identified by the number of the context frame it's defined in (var
depth, 0 is toplevel, highest is the function currently executing) and its index
(variable number in the given frame).

* Set

Size: 8

| Byte          | Description   | Type               |
| ------------- | ------------- | ------------------ |
| 0             | Opcode (0x03) | u8                 |
| 1             | Unused        | u8                 |
| 2-3           | Var index     | u16                |
| 4-5           | Var depth     | u16                |
| 6-7           | Value         | u16                |

Sets the value of a context variable (local variables of functions). The
variable is identified by the number of the context frame it's defined in (var
depth, 0 is toplevel, highest is the function currently executing) and its index
(variable number in the given frame). Value is the node number of the expression
computing the newly set value.

equation records always start after previous one, aligned 8 bytes

* Glob

Size: 8

An access to a global variable (undocumented). Opcode = 0x04.

* Lambda

Size: 8

| Byte          | Description   | Type               |
| ------------- | ------------- | ------------------ |
| 0             | Opcode (0x05) | u8                 |
| 1             | Inputs number | u8                 |
| 2-3           | Locals number | u16                |
| 4-5           | Entry         | u16                |
| 6-7           | Unused        | u16                |

Creates a closure. Inputs number is the number of arguments, locals number the
number of local variables. Entry is the node number of the expression that
constitutes the body of the function.

Closures capture their context (the stack of context frames currently in use on
creation).

* Pre

Size: 8

Deprecated / undocumented. Opcode = 0x08.

* Binary operators

Size: 8

| Byte          | Description         | Type               |
| ------------- | -------------       | ------------------ |
| 0             | Opcode (0x10-0x28)* | u8                 |
| 1             | Unused              | u8                 |
| 2-3           | Lhs                 | u16                |
| 4-5           | Rhs                 | u16                |
| 6-7           | Unused              | -                  |

> * Excluding 0x17 (Unary minus)

Applies a binary operator to the values computed by node numbers `Lhs` and
`Rhs`. Operator depends on opcode (see list).

* Unary operators

Size: 8

| Byte          | Description         | Type               |
| ------------- | -------------       | ------------------ |
| 0             | Opcode (0x17, 0x27) | u8                 |
| 1             | Unused              | -                  |
| 2-3           | Argument            | u16                |
| 4-5           | Unused              | -                  |
| 6-7           | Unused              | -                  |

Applies a unary operator to the value computed by node number `Argument`.
Operator depends on opcode (see list).

* If

Size: 8

| Byte          | Description   | Type               |
| ------------- | ------------- | ------------------ |
| 0             | Opcode (0x40) | u8                 |
| 1             | Unused        | -                  |
| 2-3           | Condition     | u16                |
| 4-5           | True branch   | u16                |
| 6-7           | False branch  | u16                |

Evaluates the expression of node number `Condition`. If true, evaluates and
yields value of node number `True branch`, otherwise `False branch`.

* Call

Size: Variable

| Byte            | Description   | Type               |
| --------------- | ------------- | ------------------ |
| 0               | Opcode (0x50) | u8                 |
| 1               | Type          | u8                 |
| 2-3             | Args number   | u8                 |
| 4-7             | Function      | u16 *OR* ptr32     |
| 8-9, 10-11, ... | Arguments     | u16                |

Calls a function. `Args number` is the number of argument passed as input,
`Arguments` is node numbers of expressions for computing argument values (in
order, as many as `Args number`).

If `Type` is 0, this is a call of a function name defined in the global function
environment, and `Function` is a 32 bits pointer to the function name as a
string in the constant section.

If `Type` is 1, this is a call of a higher-order function (`funcall`), and two
lower bytes of `Function` is the node number of the expression to compute the
functional value.

* Index

Size: 8

| Byte           | Description   | Type               |
| -------------- | ------------- | ------------------ |
| 0              | Opcode (0x54) | u8                 |
| 1              | Unused        | -                  |
| 2-3            | Object        | u16                |
| 4-5            | Index         | u16                |
| 6-7            | Unused        | -                  |

Access field number computed by node number `Index` of object computed by node
number `Object`.

* Field

Size: 8

| Byte           | Description   | Type               |
| -------------- | ------------- | ------------------ |
| 0              | Opcode (0x58) | u8                 |
| 1              | Unused        | -                  |
| 2-3            | Object        | u16                |
| 4-7            | Field         | ptr32              |

Access field by name `Field` (pointer to string in constant section) of object
computed by node number `Object`.

* Poly

Size: Variable

| Byte             | Description   | Type               |
| ---------------- | ------------- | ------------------ |
| 0                | Opcode (0x60) | u8                 |
| 1                | Notes number  | u8                 |
| 2-7              | Unused        | -                  |
| 8-15, 16-31, ... | Notes         | struct note        |

Creates a `poly` MIDI signal with the given notes. Follow `Notes number` 8 bytes
records of notes as follow:

| Byte   | Description   | Type  |
| ------ | ------------- | ----- |
| 0-1    | Pitch         | u16   |
| 2-3    | Gate          | u16   |
| 4-5    | Velocity      | u16   |
| 6-7    | Unused        | -     |

`Pitch`, `Gate`, and `Velocity` are node numbres of expression evaluating to
pitch, gate, and velocity of the note.

* Mono

Size: 8

| Byte             | Description   | Type               |
| ---------------- | ------------- | ------------------ |
| 0                | Opcode (0x68) | u8                 |
| 1                | Unused        | -                  |
| 2-3              | Pitch         | u16                |
| 2-3              | Gate          | u16                |
| 2-3              | Velocity      | u16                |

Creates a `mono` MIDI singal. `Pitch`, `Gate`, and `Velocity` are node numbres
of expression evaluating to pitch, gate, and velocity of the note.

### Locals section

After the code section begins the locals section which is unused and
unocumented. Locals section always begin at the next 16 bytes alignment. It is
an array of `n*2` bytes, where `n` is the number of local variables in the
function header, aligned to multiple of 16 bytes. If number of local variables
is 0, this section is absent and constant section immediately follows.

### Constant section

After the locals section begins the constant section which encodes constant
strings present in the code.

Each entry in the constant section is a zero-terminated string of 1 byte chars.
Every entries are 16-bytes aligned.
