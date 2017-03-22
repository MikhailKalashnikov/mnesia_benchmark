benchmark_mnesia
=====


- set nodes name in config/var.config. Run in each nodes `rebar3 run`
- From one of the running node execute `mnesia_benchmark:init(['mnesia_benchmark@127.0.0.1','mnesia_benchmark2@127.0.0.1']).`
 and `mnesia_benchmark:insert_test_data(100000).` to insert 100000 rows.
- `mnesia_benchmark:test_s_n(10, 10000).` Run 10 processes to do 10000 read-writes.  
