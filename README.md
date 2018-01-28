# FSQL - File System Query Language [![Release](https://img.shields.io/github/release/gahag/fsql.svg)](https://github.com/gahag/fsql/releases)

FSQL is a tool for performing queries on the local file system with syntax similar to SQL.


## Usage examples

FSQL will evaluate a single query if provided from the command line, or launch as a REPL otherwise.

### Basic select clause
The supported selections are:  
**name**: file name.  
**date**: file modification date.  
**size**: file size (bytes).
```sql
select name from path/dir
```

### Multiple selections
The columns will be delimited by a single tab character.  
The order of selections is respected.
```sql
select name, date from path/to/dir
```

### Recursive query
Will traverse the directory recursively.
```sql
select name recursive from path/to/dir
```

### Join clause
The supported join types are: **inner**, **left**, **right**, **outer** (left + right), **full**.
```sql
select name from path/to/dir inner join path/to/dir2 on date
```

### Where clause
FSQL supports simple predicates for filtering the results.  
A predicate is a logical expression that dictates whether a file or directory will be included in the final result.
#### Selection
A selection in an expression will represent the respective information from the file.  
The supported selections are: **name**, **date**, **size**.
#### Literals
There are 4 types of literals: **string**, **date**, **size**, **regex**.  
A **string** literal may or not be enclosed in double quotes.  
A **date** literal must be a valid YYYY-MM-DD date.  
A **size** literal must be a valid integer.  
A **regex** literal must be a valid [POSIX extended regular expression](https://www.gnu.org/software/findutils/manual/html_node/find_html/posix_002dextended-regular-expression-syntax.html), and it may or not be enclosed in double quotes.
#### Operators
| Operator           | Meaning                     |
| ------------------ | --------------------------- |
| !                  | logical not                 |
| &&                 | logical and                 |
| &#124;&#124;       | logical or                  |
| &gt;               | relational greater          |
| &lt;               | relational lower            |
| &gt;=              | relational greater or equal |
| &lt;=              | relational lower or equal   |
| ==                 | relational equal            |
| /=                 | relational not equal        |
| =~                 | regex match                 |
#### Types
FSQL features a type checker for expressions.  
Logical operators must have both operands of boolean type.  
Relational operators must have operands of the same type, and an selection operand will dictate the type.  
The regex match operator must have a left operand of type string, and a right operand of type regex literal.  
When used with the regex match operator, the selections **date** and **size** are converted to string.
#### Examples
```sql
select date from path/to/dir where name == file && date >= 2015-12-30
select size from path/to/dir where name =~ .*test-[1-9]
select name from path/to/dir where size > 20000
```


## Installation

### Binary release
FSQL comes as a single executable file, that can be downloaded from the
[releases page](https://github.com/gahag/FSQL/releases).

### Build from source
FSQL uses [Stack](https://www.haskellstack.org/): a modern, cross-platform build tool for Haskell code.  
To build FSQL, you need to have stack installed.  
With just 3 commands, it will download the compiler, build all dependencies and generate the final executable.
```sh
$ stack setup
$ stack build
$ stack install
```


## License

Copyright &copy; 2015-2018 gahag.  
All rights reserved.

This software may be modified and distributed under the terms
of the BSD license. See the [LICENSE](LICENSE) file for details.
