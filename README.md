# FSQL - File System Query Language

FSQL is a tool for performing queries on the local filesystem
with syntax similar to SQL.

## Usage examples
Check the [Specification file](Specification) for the complete usage guide.

### Select clause
```sql
select name from dir
```

### Where clause
```sql
select date from dir where name == file && date == 2015-12-30
select size from dir where name =~ "test*"
select name from dir where name > b
```

### Inner join clause
```sql
select name from dir inner join dir' on date
```

### Recursive query
```sql
select name recursive from dir
```

### Multiple selections
```sql
select name, date from dir
```

## Installation

FSQL comes as a single executable file, that can be downloaded from the
[releases page](https://github.com/gahag/FSQL/releases).

## License

Copyright (C) 2015 gahag

All rights reserved.

This software may be modified and distributed under the terms
of the BSD license. See the [LICENSE](LICENSE) file for details.
