---
project: ffmt
summary: A formatter for modern Fortran source code
author: Jacob Williams
license: MIT
src_dir: ./src
output_dir: ./doc
display: public
         protected
         private
source: true
graph: true
sort: alpha
search: true
---

A formatter for free-form Fortran. The source is split into tokens ([[ffmt_lexer]]), grouped into
statements ([[ffmt_statements]]), classified by their role in the block structure
([[ffmt_constructs]]) and analyzed for nesting depth ([[ffmt_structure]]).
