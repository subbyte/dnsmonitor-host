# DNS Monitor on Single Host

#### Functionality

1. Monitor outbound DNS queries
2. Deduplicate records such as the same domain on `A` and `AAAA`
3. Filter out special record types such as `PTR`
4. Aggregate the information from the first query
    - How many times the top-level domain (TLD) is queried
    - How many times the second-level domain (SLD) is queried
    - Select SLD/TLD as the domain key to print with the full domain
    - Color the domain key in printing regarding how many times it is queried
5. Handle all types of errors and print them to stderr asynchronously
    - Errors from `tcpdump` (the source)
    - Errors in information extraction (invalid records)

#### Installation

compile it, copy it to `/usr/bin` and set correct permissions for it to run
```
make install
```

if you just need to compile
```
make
```
