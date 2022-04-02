# C-- Compiler

This project constitutes a basic compiler for the C-- Language, implemented according to a sub-spec. As C-- is intended as an intermediate
form between a more fully-fleshed language and platform assembly, this compiler does *not* include a parser. Rather, it is intended that
high-level compilers import `cmm`'s lang module and use the definitions there to assemble the C-- AST. Alternatively `cmm` can be fed
the AST in its serialized form.