# FSQL - File System Query Language

FSQL is a tool for performing queries on the local filesystem
with syntax similar to SQL.

## Usage examples
Check the Specification file for the complete usage guide.

### Standard select clause
```sql
select name from dir
```

### Standard where clause
```sql
select date from dir where name == myFile && date == 2015-12-30 || name > t
```

### Standard inner join clause
```sql
<<<<<<< HEAD
select name from dir inner join dir on date
=======
select name from dir inner join dir on date
>>>>>>> bc5b838cbc20b9c86e1db6dc80772bf771abeed5
```

## Installation

FSQL comes as a single executable file, that can be downloaded from the
[releases page](https://github.com/gahag/FSQL/releases)

## License

Copyright (C) 2015 gahag

All rights reserved.

This software may be modified and distributed under the terms
of the BSD license. See the LICENSE file for details.
