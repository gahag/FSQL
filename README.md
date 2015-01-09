## FSQL - File System Query Language

FSQL is a tool for performing queries on the local filesystem
with syntax similar to SQL.

## Usage examples
  # Standard select clause
    select name from dir

  # Standard where clause
    select date from dir where name == myFile && date == 2015-12-30 || name > t

  # Standard inner join clause
    select name from dir inner join dir' on date

## Installation

FSQL comes as a single executable file, that can be downloaded from the releases
page at 

## License

Copyright (C) 2015 Gabriel Silva Bastos

All rights reserved.

This software may be modified and distributed under the terms
of the BSD license. See the LICENSE file for details.
