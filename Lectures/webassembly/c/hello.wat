(module
 (type $0 (func (param i32 i32) (result i32)))
 (type $1 (func (param i32 i32 i32) (result i32)))
 (type $2 (func (param i32) (result i32)))
 (type $3 (func))
 (type $4 (func (param i32 i32 i32 i32) (result i32)))
 (type $5 (func (param i32 i32 i32 i32 i32) (result i32)))
 (type $6 (func (param i32 i32 i32 i64 i32) (result i32)))
 (type $7 (func (param i32 i64 i32) (result i64)))
 (type $8 (func (param i32 i64 i64 i32) (result i32)))
 (type $9 (func (param i32 i64 i64) (result i32)))
 (type $10 (func (param i32 i32 i32 i32 i32 i32) (result i32)))
 (type $11 (func (param i32)))
 (type $12 (func (result i32)))
 (type $13 (func (param i32 i64 i32) (result i32)))
 (type $14 (func (param i32 i64) (result i32)))
 (type $15 (func (param i32 i64 i32 i32) (result i32)))
 (type $16 (func (param i32 i32 i32 i32 i64 i64 i32) (result i32)))
 (type $17 (func (param i32 i32 i32 i32 i32 i32 i32) (result i32)))
 (type $18 (func (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
 (type $19 (func (param i32 i32 i32 i64 i64 i32) (result i32)))
 (type $20 (func (param i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
 (type $21 (func (param f64 i32) (result f64)))
 (type $22 (func (param i32 i32 i32)))
 (type $23 (func (param i32 i32 i32 i32 i32)))
 (import "wasi_snapshot_preview1" "args_get" (func $__imported_wasi_snapshot_preview1_args_get (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "args_sizes_get" (func $__imported_wasi_snapshot_preview1_args_sizes_get (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "environ_get" (func $__imported_wasi_snapshot_preview1_environ_get (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "environ_sizes_get" (func $__imported_wasi_snapshot_preview1_environ_sizes_get (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "clock_res_get" (func $__imported_wasi_snapshot_preview1_clock_res_get (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "clock_time_get" (func $__imported_wasi_snapshot_preview1_clock_time_get (param i32 i64 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_advise" (func $__imported_wasi_snapshot_preview1_fd_advise (param i32 i64 i64 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_allocate" (func $__imported_wasi_snapshot_preview1_fd_allocate (param i32 i64 i64) (result i32)))
 (import "wasi_snapshot_preview1" "fd_close" (func $__imported_wasi_snapshot_preview1_fd_close (param i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_datasync" (func $__imported_wasi_snapshot_preview1_fd_datasync (param i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_fdstat_get" (func $__imported_wasi_snapshot_preview1_fd_fdstat_get (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_fdstat_set_flags" (func $__imported_wasi_snapshot_preview1_fd_fdstat_set_flags (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_fdstat_set_rights" (func $__imported_wasi_snapshot_preview1_fd_fdstat_set_rights (param i32 i64 i64) (result i32)))
 (import "wasi_snapshot_preview1" "fd_filestat_get" (func $__imported_wasi_snapshot_preview1_fd_filestat_get (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_filestat_set_size" (func $__imported_wasi_snapshot_preview1_fd_filestat_set_size (param i32 i64) (result i32)))
 (import "wasi_snapshot_preview1" "fd_filestat_set_times" (func $__imported_wasi_snapshot_preview1_fd_filestat_set_times (param i32 i64 i64 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_pread" (func $__imported_wasi_snapshot_preview1_fd_pread (param i32 i32 i32 i64 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_prestat_get" (func $__imported_wasi_snapshot_preview1_fd_prestat_get (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_prestat_dir_name" (func $__imported_wasi_snapshot_preview1_fd_prestat_dir_name (param i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_pwrite" (func $__imported_wasi_snapshot_preview1_fd_pwrite (param i32 i32 i32 i64 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_read" (func $__imported_wasi_snapshot_preview1_fd_read (param i32 i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_readdir" (func $__imported_wasi_snapshot_preview1_fd_readdir (param i32 i32 i32 i64 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_renumber" (func $__imported_wasi_snapshot_preview1_fd_renumber (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_seek" (func $__imported_wasi_snapshot_preview1_fd_seek (param i32 i64 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_sync" (func $__imported_wasi_snapshot_preview1_fd_sync (param i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_tell" (func $__imported_wasi_snapshot_preview1_fd_tell (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "fd_write" (func $__imported_wasi_snapshot_preview1_fd_write (param i32 i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "path_create_directory" (func $__imported_wasi_snapshot_preview1_path_create_directory (param i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "path_filestat_get" (func $__imported_wasi_snapshot_preview1_path_filestat_get (param i32 i32 i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "path_filestat_set_times" (func $__imported_wasi_snapshot_preview1_path_filestat_set_times (param i32 i32 i32 i32 i64 i64 i32) (result i32)))
 (import "wasi_snapshot_preview1" "path_link" (func $__imported_wasi_snapshot_preview1_path_link (param i32 i32 i32 i32 i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "path_open" (func $__imported_wasi_snapshot_preview1_path_open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "path_readlink" (func $__imported_wasi_snapshot_preview1_path_readlink (param i32 i32 i32 i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "path_remove_directory" (func $__imported_wasi_snapshot_preview1_path_remove_directory (param i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "path_rename" (func $__imported_wasi_snapshot_preview1_path_rename (param i32 i32 i32 i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "path_symlink" (func $__imported_wasi_snapshot_preview1_path_symlink (param i32 i32 i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "path_unlink_file" (func $__imported_wasi_snapshot_preview1_path_unlink_file (param i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "poll_oneoff" (func $__imported_wasi_snapshot_preview1_poll_oneoff (param i32 i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "proc_exit" (func $__imported_wasi_snapshot_preview1_proc_exit (param i32)))
 (import "wasi_snapshot_preview1" "sched_yield" (func $__imported_wasi_snapshot_preview1_sched_yield (result i32)))
 (import "wasi_snapshot_preview1" "random_get" (func $__imported_wasi_snapshot_preview1_random_get (param i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "sock_accept" (func $__imported_wasi_snapshot_preview1_sock_accept (param i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "sock_recv" (func $__imported_wasi_snapshot_preview1_sock_recv (param i32 i32 i32 i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "sock_send" (func $__imported_wasi_snapshot_preview1_sock_send (param i32 i32 i32 i32 i32) (result i32)))
 (import "wasi_snapshot_preview1" "sock_shutdown" (func $__imported_wasi_snapshot_preview1_sock_shutdown (param i32 i32) (result i32)))
 (global $__stack_pointer (mut i32) (i32.const 70304))
 (global $GOT.data.internal.__memory_base i32 (i32.const 0))
 (global $global$2 i32 (i32.const 0))
 (global $global$3 i32 (i32.const 3668))
 (global $global$4 i32 (i32.const 4720))
 (global $global$5 i32 (i32.const 4724))
 (global $global$6 i32 (i32.const 4728))
 (global $global$7 i32 (i32.const 4756))
 (global $global$8 i32 (i32.const 4720))
 (global $global$9 i32 (i32.const 4724))
 (global $global$10 i32 (i32.const 4716))
 (global $global$11 i32 (i32.const 3536))
 (global $global$12 i32 (i32.const 3656))
 (global $global$13 i32 (i32.const 3424))
 (global $global$14 i32 (i32.const 3544))
 (global $global$15 i32 (i32.const 2940))
 (global $global$16 i32 (i32.const 1224))
 (global $global$17 i32 (i32.const 1024))
 (global $global$18 i32 (i32.const 4768))
 (global $global$19 i32 (i32.const 4768))
 (global $global$20 i32 (i32.const 70304))
 (global $global$21 i32 (i32.const 1024))
 (global $global$22 i32 (i32.const 70304))
 (global $global$23 i32 (i32.const 131072))
 (global $global$24 i32 (i32.const 1))
 (memory $0 2)
 (data $.rodata (i32.const 1024) "-+   0X0x\00-0X+0X 0X-0x+0x 0x\00nan\00inf\00NAN\00INF\00.\00(null)\00Support for formatting long double values is currently disabled.\nTo enable it, add -lc-printscan-long-double to the link command.\n\00Hello, World!\n\00`\r\00\00Success\00Illegal byte sequence\00Domain error\00Result not representable\00Not a tty\00Permission denied\00Operation not permitted\00No such file or directory\00No such process\00File exists\00Value too large for data type\00No space left on device\00Out of memory\00Resource busy\00Interrupted system call\00Resource temporarily unavailable\00Invalid seek\00Cross-device link\00Read-only file system\00Directory not empty\00Connection reset by peer\00Operation timed out\00Connection refused\00Host is unreachable\00Address in use\00Broken pipe\00I/O error\00No such device or address\00No such device\00Not a directory\00Is a directory\00Text file busy\00Exec format error\00Invalid argument\00Argument list too long\00Symbolic link loop\00Filename too long\00Too many open files in system\00No file descriptors available\00Bad file descriptor\00No child process\00Bad address\00File too large\00Too many links\00No locks available\00Resource deadlock would occur\00State not recoverable\00Previous owner died\00Operation canceled\00Function not implemented\00No message of desired type\00Identifier removed\00Link has been severed\00Protocol error\00Bad message\00Not a socket\00Destination address required\00Message too large\00Protocol wrong type for socket\00Protocol not available\00Protocol not supported\00Not supported\00Address family not supported by protocol\00Address not available\00Network is down\00Network unreachable\00Connection reset by network\00Connection aborted\00No buffer space available\00Socket is connected\00Socket not connected\00Operation already in progress\00Operation in progress\00Stale file handle\00Quota exceeded\00Multihop attempted\00Capabilities insufficient\00\00\00\00\00\00\00u\02N\00\d6\01\e2\04\b9\04\18\01\8e\05\ed\02\16\04\f2\00\97\03\01\038\05\af\01\82\01O\03/\04\1e\00\d4\05\a2\00\12\03\1e\03\c2\01\de\03\08\00\ac\05\00\01d\02\f1\01e\054\02\8c\02\cf\02-\03L\04\e3\05\9f\02\f8\04\1c\05\08\05\b1\02K\05\15\02x\00R\02<\03\f1\03\e4\00\c3\03}\04\cc\00\aa\03y\05$\02n\01m\03\"\04\ab\04D\00\fb\01\ae\00\83\03`\00\e5\01\07\04\94\04^\04+\00X\019\01\92\00\c2\05\9b\01C\02F\01\f6\05\00\00\d8\r\00\00\19\00\n\00\19\19\19\00\00\00\00\05\00\00\00\00\00\00\t\00\00\00\00\0b\00\00\00\00\00\00\00\00\19\00\11\n\19\19\19\03\n\07\00\01\1b\t\0b\18\00\00\t\06\0b\00\00\0b\00\06\19\00\00\00\19\19\19\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\0e\00\00\00\00\00\00\00\00\19\00\n\r\19\19\19\00\r\00\00\02\00\t\0e\00\00\00\t\00\0e\00\00\0e\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\0c\00\00\00\00\00\00\00\00\00\00\00\13\00\00\00\00\13\00\00\00\00\t\0c\00\00\00\00\00\0c\00\00\0c\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\10\00\00\00\00\00\00\00\00\00\00\00\0f\00\00\00\04\0f\00\00\00\00\t\10\00\00\00\00\00\10\00\00\10\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\12\00\00\00\00\00\00\00\00\00\00\00\11\00\00\00\00\11\00\00\00\00\t\12\00\00\00\00\00\12\00\00\12\00\00\1a\00\00\00\1a\1a\1a\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\1a\00\00\00\1a\1a\1a\00\00\00\00\00\00\t\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\14\00\00\00\00\00\00\00\00\00\00\00\17\00\00\00\00\17\00\00\00\00\t\14\00\00\00\00\00\14\00\00\14\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\16\00\00\00\00\00\00\00\00\00\00\00\15\00\00\00\00\15\00\00\00\00\t\16\00\00\00\00\00\16\00\00\16\00\000123456789ABCDEF")
 (data $.data (i32.const 3424) "\05\00\00\00\00\00\00\00\00\00\00\00\02\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\03\00\00\00\04\00\00\00h\0e\00\00\00\04\00\00\00\00\00\00\00\00\00\00\01\00\00\00\00\00\00\00\n\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00`\r\00\00\00\00\00\00\05\00\00\00\00\00\00\00\00\00\00\00\02\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\01\00\00\00\04\00\00\00\a0\12\00\00\00\00\00\00\00\00\00\00\00\00\00\00\02\00\00\00\00\00\00\00\ff\ff\ff\ff\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\d8\r\00\00")
 (table $0 5 5 funcref)
 (elem $0 (i32.const 1) $__stdio_write $__stdio_close $__stdout_write $__stdio_seek)
 (export "memory" (memory $0))
 (export "__wasm_call_ctors" (func $__wasm_call_ctors))
 (export "_start" (func $_start))
 (export "__memory_base" (global $global$2))
 (export "__main_void" (func $__original_main))
 (export "__wasm_call_dtors" (func $__wasm_call_dtors))
 (export "__wasi_proc_exit" (func $__wasi_proc_exit))
 (export "__indirect_function_table" (table $0))
 (export "__original_main" (func $__original_main))
 (export "printf" (func $printf))
 (export "main" (func $main))
 (export "errno" (global $global$3))
 (export "_Exit" (func $_Exit))
 (export "_exit" (func $_Exit))
 (export "writev" (func $writev))
 (export "__lseek" (func $__lseek))
 (export "lseek" (func $__lseek))
 (export "__wasilibc_fd_renumber" (func $__wasilibc_fd_renumber))
 (export "__wasilibc_populate_preopens" (func $__wasilibc_populate_preopens))
 (export "close" (func $close))
 (export "__wasi_args_get" (func $__wasi_args_get))
 (export "__wasi_args_sizes_get" (func $__wasi_args_sizes_get))
 (export "__wasi_environ_get" (func $__wasi_environ_get))
 (export "__wasi_environ_sizes_get" (func $__wasi_environ_sizes_get))
 (export "__wasi_clock_res_get" (func $__wasi_clock_res_get))
 (export "__wasi_clock_time_get" (func $__wasi_clock_time_get))
 (export "__wasi_fd_advise" (func $__wasi_fd_advise))
 (export "__wasi_fd_allocate" (func $__wasi_fd_allocate))
 (export "__wasi_fd_close" (func $__wasi_fd_close))
 (export "__wasi_fd_datasync" (func $__wasi_fd_datasync))
 (export "__wasi_fd_fdstat_get" (func $__wasi_fd_fdstat_get))
 (export "__wasi_fd_fdstat_set_flags" (func $__wasi_fd_fdstat_set_flags))
 (export "__wasi_fd_fdstat_set_rights" (func $__wasi_fd_fdstat_set_rights))
 (export "__wasi_fd_filestat_get" (func $__wasi_fd_filestat_get))
 (export "__wasi_fd_filestat_set_size" (func $__wasi_fd_filestat_set_size))
 (export "__wasi_fd_filestat_set_times" (func $__wasi_fd_filestat_set_times))
 (export "__wasi_fd_pread" (func $__wasi_fd_pread))
 (export "__wasi_fd_prestat_get" (func $__wasi_fd_prestat_get))
 (export "__wasi_fd_prestat_dir_name" (func $__wasi_fd_prestat_dir_name))
 (export "__wasi_fd_pwrite" (func $__wasi_fd_pwrite))
 (export "__wasi_fd_read" (func $__wasi_fd_read))
 (export "__wasi_fd_readdir" (func $__wasi_fd_readdir))
 (export "__wasi_fd_renumber" (func $__wasi_fd_renumber))
 (export "__wasi_fd_seek" (func $__wasi_fd_seek))
 (export "__wasi_fd_sync" (func $__wasi_fd_sync))
 (export "__wasi_fd_tell" (func $__wasi_fd_tell))
 (export "__wasi_fd_write" (func $__wasi_fd_write))
 (export "__wasi_path_create_directory" (func $__wasi_path_create_directory))
 (export "__wasi_path_filestat_get" (func $__wasi_path_filestat_get))
 (export "__wasi_path_filestat_set_times" (func $__wasi_path_filestat_set_times))
 (export "__wasi_path_link" (func $__wasi_path_link))
 (export "__wasi_path_open" (func $__wasi_path_open))
 (export "__wasi_path_readlink" (func $__wasi_path_readlink))
 (export "__wasi_path_remove_directory" (func $__wasi_path_remove_directory))
 (export "__wasi_path_rename" (func $__wasi_path_rename))
 (export "__wasi_path_symlink" (func $__wasi_path_symlink))
 (export "__wasi_path_unlink_file" (func $__wasi_path_unlink_file))
 (export "__wasi_poll_oneoff" (func $__wasi_poll_oneoff))
 (export "strlen" (func $strlen))
 (export "__wasi_sched_yield" (func $__wasi_sched_yield))
 (export "__wasi_random_get" (func $__wasi_random_get))
 (export "__wasi_sock_accept" (func $__wasi_sock_accept))
 (export "__wasi_sock_recv" (func $__wasi_sock_recv))
 (export "__wasi_sock_send" (func $__wasi_sock_send))
 (export "__wasi_sock_shutdown" (func $__wasi_sock_shutdown))
 (export "abort" (func $abort))
 (export "__isatty" (func $__isatty))
 (export "isatty" (func $__isatty))
 (export "__strerror_l" (func $__strerror_l))
 (export "strerror" (func $strerror))
 (export "strerror_l" (func $__strerror_l))
 (export "__funcs_on_exit" (func $dummy))
 (export "__stdio_exit" (func $__stdio_exit))
 (export "exit" (func $exit))
 (export "__progname" (global $global$4))
 (export "__progname_full" (global $global$5))
 (export "__libc" (global $global$6))
 (export "__hwcap" (global $global$7))
 (export "program_invocation_short_name" (global $global$8))
 (export "program_invocation_name" (global $global$9))
 (export "__lctrans" (func $__lctrans))
 (export "__lctrans_impl" (func $dummy.1))
 (export "__lctrans_cur" (func $__lctrans_cur))
 (export "frexp" (func $frexp))
 (export "wcrtomb" (func $wcrtomb))
 (export "wctomb" (func $wctomb))
 (export "__stdio_close" (func $__stdio_close))
 (export "__stdin_used" (global $global$10))
 (export "__stdout_used" (global $global$11))
 (export "__stderr_used" (global $global$12))
 (export "__stdio_exit_needed" (func $__stdio_exit))
 (export "__stdio_seek" (func $__stdio_seek))
 (export "__stdio_write" (func $__stdio_write))
 (export "__stdout_write" (func $__stdout_write))
 (export "__towrite" (func $__towrite))
 (export "__towrite_needs_stdio_exit" (func $__towrite_needs_stdio_exit))
 (export "fputs" (func $fputs))
 (export "fputs_unlocked" (func $fputs))
 (export "__fwritex" (func $__fwritex))
 (export "fwrite" (func $fwrite))
 (export "fwrite_unlocked" (func $fwrite))
 (export "__ofl_lock" (func $__ofl_lock))
 (export "__ofl_unlock" (func $__ofl_unlock))
 (export "__stdout_FILE" (global $global$13))
 (export "vfprintf" (func $vfprintf))
 (export "iprintf" (func $printf))
 (export "__small_printf" (func $printf))
 (export "__stderr_FILE" (global $global$14))
 (export "stderr" (global $global$15))
 (export "stdout" (global $global$16))
 (export "memcpy" (func $memcpy))
 (export "strnlen" (func $strnlen))
 (export "memset" (func $memset))
 (export "memchr" (func $memchr))
 (export "__dso_handle" (global $global$17))
 (export "__data_end" (global $global$18))
 (export "__stack_low" (global $global$19))
 (export "__stack_high" (global $global$20))
 (export "__global_base" (global $global$21))
 (export "__heap_base" (global $global$22))
 (export "__heap_end" (global $global$23))
 (export "__table_base" (global $global$24))
 (func $__wasm_call_ctors
 )
 (func $_start
  (local $0 i32)
  (block $label$1
   (block $label$2
    (br_if $label$2
     (i32.load
      (i32.add
       (global.get $GOT.data.internal.__memory_base)
       (i32.const 3664)
      )
     )
    )
    (i32.store
     (i32.add
      (global.get $GOT.data.internal.__memory_base)
      (i32.const 3664)
     )
     (i32.const 1)
    )
    (call $__wasm_call_ctors)
    (local.set $0
     (call $__original_main)
    )
    (call $__wasm_call_dtors)
    (br_if $label$1
     (local.get $0)
    )
    (return)
   )
   (unreachable)
  )
  (call $__wasi_proc_exit
   (local.get $0)
  )
  (unreachable)
 )
 (func $__original_main (result i32)
  (local $0 i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local.set $0
   (global.get $__stack_pointer)
  )
  (local.set $1
   (i32.const 16)
  )
  (local.set $2
   (i32.sub
    (local.get $0)
    (local.get $1)
   )
  )
  (global.set $__stack_pointer
   (local.get $2)
  )
  (local.set $3
   (i32.const 0)
  )
  (i32.store offset=12
   (local.get $2)
   (local.get $3)
  )
  (local.set $4
   (i32.const 1209)
  )
  (local.set $5
   (i32.const 0)
  )
  (drop
   (call $printf
    (local.get $4)
    (local.get $5)
   )
  )
  (local.set $6
   (i32.const 0)
  )
  (local.set $7
   (i32.const 16)
  )
  (local.set $8
   (i32.add
    (local.get $2)
    (local.get $7)
   )
  )
  (global.set $__stack_pointer
   (local.get $8)
  )
  (return
   (local.get $6)
  )
 )
 (func $main (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local.set $2
   (call $__original_main)
  )
  (return
   (local.get $2)
  )
 )
 (func $__wasi_args_get (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_args_get
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_args_sizes_get (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_args_sizes_get
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_environ_get (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_environ_get
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_environ_sizes_get (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_environ_sizes_get
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_clock_res_get (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_clock_res_get
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_clock_time_get (param $0 i32) (param $1 i64) (param $2 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_clock_time_get
    (local.get $0)
    (local.get $1)
    (local.get $2)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_advise (param $0 i32) (param $1 i64) (param $2 i64) (param $3 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_advise
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_allocate (param $0 i32) (param $1 i64) (param $2 i64) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_allocate
    (local.get $0)
    (local.get $1)
    (local.get $2)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_close (param $0 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_close
    (local.get $0)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_datasync (param $0 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_datasync
    (local.get $0)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_fdstat_get (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_fdstat_get
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_fdstat_set_flags (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_fdstat_set_flags
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_fdstat_set_rights (param $0 i32) (param $1 i64) (param $2 i64) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_fdstat_set_rights
    (local.get $0)
    (local.get $1)
    (local.get $2)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_filestat_get (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_filestat_get
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_filestat_set_size (param $0 i32) (param $1 i64) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_filestat_set_size
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_filestat_set_times (param $0 i32) (param $1 i64) (param $2 i64) (param $3 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_filestat_set_times
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_pread (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i64) (param $4 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_pread
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
    (local.get $4)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_prestat_get (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_prestat_get
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_prestat_dir_name (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_prestat_dir_name
    (local.get $0)
    (local.get $1)
    (local.get $2)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_pwrite (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i64) (param $4 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_pwrite
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
    (local.get $4)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_read (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_read
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_readdir (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i64) (param $4 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_readdir
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
    (local.get $4)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_renumber (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_renumber
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_seek (param $0 i32) (param $1 i64) (param $2 i32) (param $3 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_seek
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_sync (param $0 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_sync
    (local.get $0)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_tell (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_tell
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_fd_write (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_fd_write
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_path_create_directory (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_path_create_directory
    (local.get $0)
    (local.get $1)
    (call $strlen
     (local.get $1)
    )
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_path_filestat_get (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_path_filestat_get
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (call $strlen
     (local.get $2)
    )
    (local.get $3)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_path_filestat_set_times (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i64) (param $4 i64) (param $5 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_path_filestat_set_times
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (call $strlen
     (local.get $2)
    )
    (local.get $3)
    (local.get $4)
    (local.get $5)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_path_link (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_path_link
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (call $strlen
     (local.get $2)
    )
    (local.get $3)
    (local.get $4)
    (call $strlen
     (local.get $4)
    )
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_path_open (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i64) (param $5 i64) (param $6 i32) (param $7 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_path_open
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (call $strlen
     (local.get $2)
    )
    (local.get $3)
    (local.get $4)
    (local.get $5)
    (local.get $6)
    (local.get $7)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_path_readlink (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_path_readlink
    (local.get $0)
    (local.get $1)
    (call $strlen
     (local.get $1)
    )
    (local.get $2)
    (local.get $3)
    (local.get $4)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_path_remove_directory (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_path_remove_directory
    (local.get $0)
    (local.get $1)
    (call $strlen
     (local.get $1)
    )
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_path_rename (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_path_rename
    (local.get $0)
    (local.get $1)
    (call $strlen
     (local.get $1)
    )
    (local.get $2)
    (local.get $3)
    (call $strlen
     (local.get $3)
    )
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_path_symlink (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_path_symlink
    (local.get $0)
    (call $strlen
     (local.get $0)
    )
    (local.get $1)
    (local.get $2)
    (call $strlen
     (local.get $2)
    )
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_path_unlink_file (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_path_unlink_file
    (local.get $0)
    (local.get $1)
    (call $strlen
     (local.get $1)
    )
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_poll_oneoff (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_poll_oneoff
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_proc_exit (param $0 i32)
  (call $__imported_wasi_snapshot_preview1_proc_exit
   (local.get $0)
  )
  (unreachable)
 )
 (func $__wasi_sched_yield (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_sched_yield)
   (i32.const 65535)
  )
 )
 (func $__wasi_random_get (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_random_get
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_sock_accept (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_sock_accept
    (local.get $0)
    (local.get $1)
    (local.get $2)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_sock_recv (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (param $5 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_sock_recv
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
    (local.get $4)
    (local.get $5)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_sock_send (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_sock_send
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (local.get $3)
    (local.get $4)
   )
   (i32.const 65535)
  )
 )
 (func $__wasi_sock_shutdown (param $0 i32) (param $1 i32) (result i32)
  (i32.and
   (call $__imported_wasi_snapshot_preview1_sock_shutdown
    (local.get $0)
    (local.get $1)
   )
   (i32.const 65535)
  )
 )
 (func $_Exit (param $0 i32)
  (call $__wasi_proc_exit
   (local.get $0)
  )
  (unreachable)
 )
 (func $dummy
 )
 (func $__wasm_call_dtors
  (call $dummy)
  (call $__stdio_exit)
 )
 (func $exit (param $0 i32)
  (call $dummy)
  (call $__stdio_exit)
  (call $_Exit
   (local.get $0)
  )
  (unreachable)
 )
 (func $printf (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (global.set $__stack_pointer
   (local.tee $2
    (i32.sub
     (global.get $__stack_pointer)
     (i32.const 16)
    )
   )
  )
  (i32.store offset=12
   (local.get $2)
   (local.get $1)
  )
  (local.set $1
   (call $vfprintf
    (i32.const 3424)
    (local.get $0)
    (local.get $1)
   )
  )
  (global.set $__stack_pointer
   (i32.add
    (local.get $2)
    (i32.const 16)
   )
  )
  (local.get $1)
 )
 (func $__wasilibc_fd_renumber (param $0 i32) (param $1 i32) (result i32)
  (call $__wasilibc_populate_preopens)
  (block $label$1
   (br_if $label$1
    (local.tee $1
     (call $__wasi_fd_renumber
      (local.get $0)
      (local.get $1)
     )
    )
   )
   (return
    (i32.const 0)
   )
  )
  (i32.store offset=3668
   (i32.const 0)
   (local.get $1)
  )
  (i32.const -1)
 )
 (func $__wasilibc_populate_preopens
 )
 (func $close (param $0 i32) (result i32)
  (call $__wasilibc_populate_preopens)
  (block $label$1
   (br_if $label$1
    (local.tee $0
     (call $__wasi_fd_close
      (local.get $0)
     )
    )
   )
   (return
    (i32.const 0)
   )
  )
  (i32.store offset=3668
   (i32.const 0)
   (local.get $0)
  )
  (i32.const -1)
 )
 (func $__stdio_close (param $0 i32) (result i32)
  (call $close
   (i32.load offset=56
    (local.get $0)
   )
  )
 )
 (func $writev (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local $4 i32)
  (global.set $__stack_pointer
   (local.tee $3
    (i32.sub
     (global.get $__stack_pointer)
     (i32.const 16)
    )
   )
  )
  (local.set $4
   (i32.const -1)
  )
  (block $label$1
   (block $label$2
    (br_if $label$2
     (i32.gt_s
      (local.get $2)
      (i32.const -1)
     )
    )
    (i32.store offset=3668
     (i32.const 0)
     (i32.const 28)
    )
    (br $label$1)
   )
   (block $label$3
    (br_if $label$3
     (i32.eqz
      (local.tee $2
       (call $__wasi_fd_write
        (local.get $0)
        (local.get $1)
        (local.get $2)
        (i32.add
         (local.get $3)
         (i32.const 12)
        )
       )
      )
     )
    )
    (i32.store offset=3668
     (i32.const 0)
     (local.get $2)
    )
    (local.set $4
     (i32.const -1)
    )
    (br $label$1)
   )
   (local.set $4
    (i32.load offset=12
     (local.get $3)
    )
   )
  )
  (global.set $__stack_pointer
   (i32.add
    (local.get $3)
    (i32.const 16)
   )
  )
  (local.get $4)
 )
 (func $__stdio_write (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local $9 i32)
  (global.set $__stack_pointer
   (local.tee $3
    (i32.sub
     (global.get $__stack_pointer)
     (i32.const 16)
    )
   )
  )
  (i32.store offset=12
   (local.get $3)
   (local.get $2)
  )
  (i32.store offset=8
   (local.get $3)
   (local.get $1)
  )
  (i32.store
   (local.get $3)
   (local.tee $1
    (i32.load offset=24
     (local.get $0)
    )
   )
  )
  (i32.store offset=4
   (local.get $3)
   (local.tee $4
    (i32.sub
     (i32.load offset=20
      (local.get $0)
     )
     (local.get $1)
    )
   )
  )
  (local.set $5
   (i32.const 2)
  )
  (block $label$1
   (block $label$2
    (br_if $label$2
     (i32.eq
      (local.tee $1
       (call $writev
        (i32.load offset=56
         (local.get $0)
        )
        (local.get $3)
        (i32.const 2)
       )
      )
      (local.tee $6
       (i32.add
        (local.get $4)
        (local.get $2)
       )
      )
     )
    )
    (local.set $4
     (local.get $3)
    )
    (loop $label$3
     (block $label$4
      (br_if $label$4
       (i32.gt_s
        (local.get $1)
        (i32.const -1)
       )
      )
      (local.set $1
       (i32.const 0)
      )
      (i32.store offset=24
       (local.get $0)
       (i32.const 0)
      )
      (i64.store offset=16
       (local.get $0)
       (i64.const 0)
      )
      (i32.store
       (local.get $0)
       (i32.or
        (i32.load
         (local.get $0)
        )
        (i32.const 32)
       )
      )
      (br_if $label$1
       (i32.eq
        (local.get $5)
        (i32.const 2)
       )
      )
      (local.set $1
       (i32.sub
        (local.get $2)
        (i32.load offset=4
         (local.get $4)
        )
       )
      )
      (br $label$1)
     )
     (i32.store
      (local.tee $9
       (i32.add
        (local.get $4)
        (i32.shl
         (local.tee $8
          (i32.gt_u
           (local.get $1)
           (local.tee $7
            (i32.load offset=4
             (local.get $4)
            )
           )
          )
         )
         (i32.const 3)
        )
       )
      )
      (i32.add
       (i32.load
        (local.get $9)
       )
       (local.tee $7
        (i32.sub
         (local.get $1)
         (select
          (local.get $7)
          (i32.const 0)
          (local.get $8)
         )
        )
       )
      )
     )
     (i32.store
      (local.tee $4
       (i32.add
        (local.get $4)
        (select
         (i32.const 12)
         (i32.const 4)
         (local.get $8)
        )
       )
      )
      (i32.sub
       (i32.load
        (local.get $4)
       )
       (local.get $7)
      )
     )
     (local.set $4
      (local.get $9)
     )
     (br_if $label$3
      (i32.ne
       (local.tee $6
        (i32.sub
         (local.get $6)
         (local.get $1)
        )
       )
       (local.tee $1
        (call $writev
         (i32.load offset=56
          (local.get $0)
         )
         (local.get $9)
         (local.tee $5
          (i32.sub
           (local.get $5)
           (local.get $8)
          )
         )
        )
       )
      )
     )
    )
   )
   (i32.store offset=24
    (local.get $0)
    (local.tee $1
     (i32.load offset=40
      (local.get $0)
     )
    )
   )
   (i32.store offset=20
    (local.get $0)
    (local.get $1)
   )
   (i32.store offset=16
    (local.get $0)
    (i32.add
     (local.get $1)
     (i32.load offset=44
      (local.get $0)
     )
    )
   )
   (local.set $1
    (local.get $2)
   )
  )
  (global.set $__stack_pointer
   (i32.add
    (local.get $3)
    (i32.const 16)
   )
  )
  (local.get $1)
 )
 (func $__isatty (param $0 i32) (result i32)
  (local $1 i32)
  (local $2 i32)
  (global.set $__stack_pointer
   (local.tee $1
    (i32.sub
     (global.get $__stack_pointer)
     (i32.const 32)
    )
   )
  )
  (block $label$1
   (block $label$2
    (br_if $label$2
     (local.tee $0
      (call $__wasi_fd_fdstat_get
       (local.get $0)
       (i32.add
        (local.get $1)
        (i32.const 8)
       )
      )
     )
    )
    (local.set $0
     (i32.const 59)
    )
    (br_if $label$2
     (i32.ne
      (i32.load8_u offset=8
       (local.get $1)
      )
      (i32.const 2)
     )
    )
    (br_if $label$2
     (i32.and
      (i32.load8_u offset=16
       (local.get $1)
      )
      (i32.const 36)
     )
    )
    (local.set $2
     (i32.const 1)
    )
    (br $label$1)
   )
   (local.set $2
    (i32.const 0)
   )
   (i32.store offset=3668
    (i32.const 0)
    (local.get $0)
   )
  )
  (global.set $__stack_pointer
   (i32.add
    (local.get $1)
    (i32.const 32)
   )
  )
  (local.get $2)
 )
 (func $__stdout_write (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (i32.store offset=32
   (local.get $0)
   (i32.const 1)
  )
  (block $label$1
   (br_if $label$1
    (i32.and
     (i32.load8_u
      (local.get $0)
     )
     (i32.const 64)
    )
   )
   (br_if $label$1
    (call $__isatty
     (i32.load offset=56
      (local.get $0)
     )
    )
   )
   (i32.store offset=64
    (local.get $0)
    (i32.const -1)
   )
  )
  (call $__stdio_write
   (local.get $0)
   (local.get $1)
   (local.get $2)
  )
 )
 (func $__lseek (param $0 i32) (param $1 i64) (param $2 i32) (result i64)
  (local $3 i32)
  (global.set $__stack_pointer
   (local.tee $3
    (i32.sub
     (global.get $__stack_pointer)
     (i32.const 16)
    )
   )
  )
  (block $label$1
   (block $label$2
    (br_if $label$2
     (i32.eqz
      (local.tee $2
       (call $__wasi_fd_seek
        (local.get $0)
        (local.get $1)
        (i32.and
         (local.get $2)
         (i32.const 255)
        )
        (i32.add
         (local.get $3)
         (i32.const 8)
        )
       )
      )
     )
    )
    (i32.store offset=3668
     (i32.const 0)
     (select
      (i32.const 70)
      (local.get $2)
      (i32.eq
       (local.get $2)
       (i32.const 76)
      )
     )
    )
    (local.set $1
     (i64.const -1)
    )
    (br $label$1)
   )
   (local.set $1
    (i64.load offset=8
     (local.get $3)
    )
   )
  )
  (global.set $__stack_pointer
   (i32.add
    (local.get $3)
    (i32.const 16)
   )
  )
  (local.get $1)
 )
 (func $__stdio_seek (param $0 i32) (param $1 i64) (param $2 i32) (result i64)
  (call $__lseek
   (i32.load offset=56
    (local.get $0)
   )
   (local.get $1)
   (local.get $2)
  )
 )
 (func $__ofl_lock (result i32)
  (i32.const 4712)
 )
 (func $__ofl_unlock
 )
 (func $__stdio_exit
  (local $0 i32)
  (local $1 i32)
  (local $2 i32)
  (block $label$1
   (br_if $label$1
    (i32.eqz
     (local.tee $0
      (i32.load
       (call $__ofl_lock)
      )
     )
    )
   )
   (loop $label$2
    (block $label$3
     (br_if $label$3
      (i32.eq
       (i32.load offset=20
        (local.get $0)
       )
       (i32.load offset=24
        (local.get $0)
       )
      )
     )
     (drop
      (call_indirect (type $1)
       (local.get $0)
       (i32.const 0)
       (i32.const 0)
       (i32.load offset=32
        (local.get $0)
       )
      )
     )
    )
    (block $label$4
     (br_if $label$4
      (i32.eq
       (local.tee $1
        (i32.load offset=4
         (local.get $0)
        )
       )
       (local.tee $2
        (i32.load offset=8
         (local.get $0)
        )
       )
      )
     )
     (drop
      (call_indirect (type $7)
       (local.get $0)
       (i64.extend_i32_s
        (i32.sub
         (local.get $1)
         (local.get $2)
        )
       )
       (i32.const 1)
       (i32.load offset=36
        (local.get $0)
       )
      )
     )
    )
    (br_if $label$2
     (local.tee $0
      (i32.load offset=52
       (local.get $0)
      )
     )
    )
   )
  )
  (block $label$5
   (br_if $label$5
    (i32.eqz
     (local.tee $0
      (i32.load offset=4716
       (i32.const 0)
      )
     )
    )
   )
   (block $label$6
    (br_if $label$6
     (i32.eq
      (i32.load offset=20
       (local.get $0)
      )
      (i32.load offset=24
       (local.get $0)
      )
     )
    )
    (drop
     (call_indirect (type $1)
      (local.get $0)
      (i32.const 0)
      (i32.const 0)
      (i32.load offset=32
       (local.get $0)
      )
     )
    )
   )
   (br_if $label$5
    (i32.eq
     (local.tee $1
      (i32.load offset=4
       (local.get $0)
      )
     )
     (local.tee $2
      (i32.load offset=8
       (local.get $0)
      )
     )
    )
   )
   (drop
    (call_indirect (type $7)
     (local.get $0)
     (i64.extend_i32_s
      (i32.sub
       (local.get $1)
       (local.get $2)
      )
     )
     (i32.const 1)
     (i32.load offset=36
      (local.get $0)
     )
    )
   )
  )
  (block $label$7
   (br_if $label$7
    (i32.eqz
     (local.tee $0
      (i32.load offset=3536
       (i32.const 0)
      )
     )
    )
   )
   (block $label$8
    (br_if $label$8
     (i32.eq
      (i32.load offset=20
       (local.get $0)
      )
      (i32.load offset=24
       (local.get $0)
      )
     )
    )
    (drop
     (call_indirect (type $1)
      (local.get $0)
      (i32.const 0)
      (i32.const 0)
      (i32.load offset=32
       (local.get $0)
      )
     )
    )
   )
   (br_if $label$7
    (i32.eq
     (local.tee $1
      (i32.load offset=4
       (local.get $0)
      )
     )
     (local.tee $2
      (i32.load offset=8
       (local.get $0)
      )
     )
    )
   )
   (drop
    (call_indirect (type $7)
     (local.get $0)
     (i64.extend_i32_s
      (i32.sub
       (local.get $1)
       (local.get $2)
      )
     )
     (i32.const 1)
     (i32.load offset=36
      (local.get $0)
     )
    )
   )
  )
  (block $label$9
   (br_if $label$9
    (i32.eqz
     (local.tee $0
      (i32.load offset=3656
       (i32.const 0)
      )
     )
    )
   )
   (block $label$10
    (br_if $label$10
     (i32.eq
      (i32.load offset=20
       (local.get $0)
      )
      (i32.load offset=24
       (local.get $0)
      )
     )
    )
    (drop
     (call_indirect (type $1)
      (local.get $0)
      (i32.const 0)
      (i32.const 0)
      (i32.load offset=32
       (local.get $0)
      )
     )
    )
   )
   (br_if $label$9
    (i32.eq
     (local.tee $1
      (i32.load offset=4
       (local.get $0)
      )
     )
     (local.tee $2
      (i32.load offset=8
       (local.get $0)
      )
     )
    )
   )
   (drop
    (call_indirect (type $7)
     (local.get $0)
     (i64.extend_i32_s
      (i32.sub
       (local.get $1)
       (local.get $2)
      )
     )
     (i32.const 1)
     (i32.load offset=36
      (local.get $0)
     )
    )
   )
  )
 )
 (func $__towrite (param $0 i32) (result i32)
  (local $1 i32)
  (i32.store offset=60
   (local.get $0)
   (i32.or
    (i32.add
     (local.tee $1
      (i32.load offset=60
       (local.get $0)
      )
     )
     (i32.const -1)
    )
    (local.get $1)
   )
  )
  (block $label$1
   (br_if $label$1
    (i32.eqz
     (i32.and
      (local.tee $1
       (i32.load
        (local.get $0)
       )
      )
      (i32.const 8)
     )
    )
   )
   (i32.store
    (local.get $0)
    (i32.or
     (local.get $1)
     (i32.const 32)
    )
   )
   (return
    (i32.const -1)
   )
  )
  (i64.store offset=4 align=4
   (local.get $0)
   (i64.const 0)
  )
  (i32.store offset=24
   (local.get $0)
   (local.tee $1
    (i32.load offset=40
     (local.get $0)
    )
   )
  )
  (i32.store offset=20
   (local.get $0)
   (local.get $1)
  )
  (i32.store offset=16
   (local.get $0)
   (i32.add
    (local.get $1)
    (i32.load offset=44
     (local.get $0)
    )
   )
  )
  (i32.const 0)
 )
 (func $__towrite_needs_stdio_exit
  (call $__stdio_exit)
 )
 (func $__fwritex (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (block $label$1
   (block $label$2
    (br_if $label$2
     (local.tee $3
      (i32.load offset=16
       (local.get $2)
      )
     )
    )
    (local.set $4
     (i32.const 0)
    )
    (br_if $label$1
     (call $__towrite
      (local.get $2)
     )
    )
    (local.set $3
     (i32.load offset=16
      (local.get $2)
     )
    )
   )
   (block $label$3
    (br_if $label$3
     (i32.ge_u
      (i32.sub
       (local.get $3)
       (local.tee $5
        (i32.load offset=20
         (local.get $2)
        )
       )
      )
      (local.get $1)
     )
    )
    (return
     (call_indirect (type $1)
      (local.get $2)
      (local.get $0)
      (local.get $1)
      (i32.load offset=32
       (local.get $2)
      )
     )
    )
   )
   (local.set $6
    (i32.const 0)
   )
   (block $label$4
    (br_if $label$4
     (i32.lt_s
      (i32.load offset=64
       (local.get $2)
      )
      (i32.const 0)
     )
    )
    (local.set $6
     (i32.const 0)
    )
    (local.set $4
     (local.get $0)
    )
    (local.set $3
     (i32.const 0)
    )
    (loop $label$5
     (br_if $label$4
      (i32.eq
       (local.get $1)
       (local.get $3)
      )
     )
     (local.set $3
      (i32.add
       (local.get $3)
       (i32.const 1)
      )
     )
     (br_if $label$5
      (i32.ne
       (i32.load8_u
        (local.tee $7
         (i32.add
          (local.tee $4
           (i32.add
            (local.get $4)
            (i32.const -1)
           )
          )
          (local.get $1)
         )
        )
       )
       (i32.const 10)
      )
     )
    )
    (br_if $label$1
     (i32.lt_u
      (local.tee $4
       (call_indirect (type $1)
        (local.get $2)
        (local.get $0)
        (local.tee $6
         (i32.add
          (i32.sub
           (local.get $1)
           (local.get $3)
          )
          (i32.const 1)
         )
        )
        (i32.load offset=32
         (local.get $2)
        )
       )
      )
      (local.get $6)
     )
    )
    (local.set $1
     (i32.add
      (local.get $3)
      (i32.const -1)
     )
    )
    (local.set $0
     (i32.add
      (local.get $7)
      (i32.const 1)
     )
    )
    (local.set $5
     (i32.load offset=20
      (local.get $2)
     )
    )
   )
   (drop
    (call $memcpy
     (local.get $5)
     (local.get $0)
     (local.get $1)
    )
   )
   (i32.store offset=20
    (local.get $2)
    (i32.add
     (i32.load offset=20
      (local.get $2)
     )
     (local.get $1)
    )
   )
   (local.set $4
    (i32.add
     (local.get $6)
     (local.get $1)
    )
   )
  )
  (local.get $4)
 )
 (func $fwrite (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local $9 i32)
  (local.set $4
   (i32.mul
    (local.get $2)
    (local.get $1)
   )
  )
  (block $label$1
   (block $label$2
    (br_if $label$2
     (local.tee $5
      (i32.load offset=16
       (local.get $3)
      )
     )
    )
    (local.set $6
     (i32.const 0)
    )
    (br_if $label$1
     (call $__towrite
      (local.get $3)
     )
    )
    (local.set $5
     (i32.load offset=16
      (local.get $3)
     )
    )
   )
   (block $label$3
    (br_if $label$3
     (i32.ge_u
      (i32.sub
       (local.get $5)
       (local.tee $7
        (i32.load offset=20
         (local.get $3)
        )
       )
      )
      (local.get $4)
     )
    )
    (local.set $6
     (call_indirect (type $1)
      (local.get $3)
      (local.get $0)
      (local.get $4)
      (i32.load offset=32
       (local.get $3)
      )
     )
    )
    (br $label$1)
   )
   (local.set $8
    (i32.const 0)
   )
   (block $label$4
    (block $label$5
     (br_if $label$5
      (i32.ge_s
       (i32.load offset=64
        (local.get $3)
       )
       (i32.const 0)
      )
     )
     (local.set $5
      (local.get $4)
     )
     (br $label$4)
    )
    (local.set $6
     (i32.add
      (local.get $0)
      (local.get $4)
     )
    )
    (local.set $8
     (i32.const 0)
    )
    (local.set $5
     (i32.const 0)
    )
    (loop $label$6
     (block $label$7
      (br_if $label$7
       (i32.add
        (local.get $4)
        (local.get $5)
       )
      )
      (local.set $5
       (local.get $4)
      )
      (br $label$4)
     )
     (br_if $label$6
      (i32.ne
       (i32.load8_u
        (local.tee $9
         (i32.add
          (local.tee $5
           (i32.add
            (local.get $5)
            (i32.const -1)
           )
          )
          (local.get $6)
         )
        )
       )
       (i32.const 10)
      )
     )
    )
    (br_if $label$1
     (i32.lt_u
      (local.tee $6
       (call_indirect (type $1)
        (local.get $3)
        (local.get $0)
        (local.tee $8
         (i32.add
          (i32.add
           (local.get $4)
           (local.get $5)
          )
          (i32.const 1)
         )
        )
        (i32.load offset=32
         (local.get $3)
        )
       )
      )
      (local.get $8)
     )
    )
    (local.set $5
     (i32.xor
      (local.get $5)
      (i32.const -1)
     )
    )
    (local.set $0
     (i32.add
      (local.get $9)
      (i32.const 1)
     )
    )
    (local.set $7
     (i32.load offset=20
      (local.get $3)
     )
    )
   )
   (drop
    (call $memcpy
     (local.get $7)
     (local.get $0)
     (local.get $5)
    )
   )
   (i32.store offset=20
    (local.get $3)
    (i32.add
     (i32.load offset=20
      (local.get $3)
     )
     (local.get $5)
    )
   )
   (local.set $6
    (i32.add
     (local.get $8)
     (local.get $5)
    )
   )
  )
  (block $label$8
   (br_if $label$8
    (i32.ne
     (local.get $6)
     (local.get $4)
    )
   )
   (return
    (select
     (local.get $2)
     (i32.const 0)
     (local.get $1)
    )
   )
  )
  (i32.div_u
   (local.get $6)
   (local.get $1)
  )
 )
 (func $dummy.1 (param $0 i32) (param $1 i32) (result i32)
  (local.get $0)
 )
 (func $__lctrans (param $0 i32) (param $1 i32) (result i32)
  (call $dummy.1
   (local.get $0)
   (local.get $1)
  )
 )
 (func $__lctrans_cur (param $0 i32) (result i32)
  (local $1 i32)
  (block $label$1
   (br_if $label$1
    (local.tee $1
     (i32.load offset=4752
      (i32.const 0)
     )
    )
   )
   (local.set $1
    (i32.const 4728)
   )
   (i32.store offset=4752
    (i32.const 0)
    (i32.const 4728)
   )
  )
  (call $dummy.1
   (local.get $0)
   (i32.load offset=20
    (local.get $1)
   )
  )
 )
 (func $__strerror_l (param $0 i32) (param $1 i32) (result i32)
  (call $__lctrans
   (i32.add
    (i32.load16_u
     (i32.add
      (i32.shl
       (select
        (i32.const 0)
        (local.get $0)
        (i32.gt_u
         (local.get $0)
         (i32.const 76)
        )
       )
       (i32.const 1)
      )
      (i32.const 2784)
     )
    )
    (i32.const 1228)
   )
   (i32.load offset=20
    (local.get $1)
   )
  )
 )
 (func $strerror (param $0 i32) (result i32)
  (local $1 i32)
  (block $label$1
   (br_if $label$1
    (local.tee $1
     (i32.load offset=4752
      (i32.const 0)
     )
    )
   )
   (local.set $1
    (i32.const 4728)
   )
   (i32.store offset=4752
    (i32.const 0)
    (i32.const 4728)
   )
  )
  (call $__lctrans
   (i32.add
    (i32.load16_u
     (i32.add
      (i32.shl
       (select
        (i32.const 0)
        (local.get $0)
        (i32.gt_u
         (local.get $0)
         (i32.const 76)
        )
       )
       (i32.const 1)
      )
      (i32.const 2784)
     )
    )
    (i32.const 1228)
   )
   (i32.load offset=20
    (local.get $1)
   )
  )
 )
 (func $wcrtomb (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local.set $3
   (i32.const 1)
  )
  (block $label$1
   (br_if $label$1
    (i32.eqz
     (local.get $0)
    )
   )
   (block $label$2
    (br_if $label$2
     (i32.gt_u
      (local.get $1)
      (i32.const 127)
     )
    )
    (i32.store8
     (local.get $0)
     (local.get $1)
    )
    (return
     (i32.const 1)
    )
   )
   (block $label$3
    (block $label$4
     (br_if $label$4
      (i32.load offset=4728
       (i32.const 0)
      )
     )
     (block $label$5
      (br_if $label$5
       (i32.eq
        (i32.and
         (local.get $1)
         (i32.const -128)
        )
        (i32.const 57216)
       )
      )
      (i32.store offset=3668
       (i32.const 0)
       (i32.const 25)
      )
      (br $label$3)
     )
     (i32.store8
      (local.get $0)
      (local.get $1)
     )
     (return
      (i32.const 1)
     )
    )
    (block $label$6
     (br_if $label$6
      (i32.gt_u
       (local.get $1)
       (i32.const 2047)
      )
     )
     (i32.store8 offset=1
      (local.get $0)
      (i32.or
       (i32.and
        (local.get $1)
        (i32.const 63)
       )
       (i32.const 128)
      )
     )
     (i32.store8
      (local.get $0)
      (i32.or
       (i32.shr_u
        (local.get $1)
        (i32.const 6)
       )
       (i32.const 192)
      )
     )
     (return
      (i32.const 2)
     )
    )
    (block $label$7
     (block $label$8
      (br_if $label$8
       (i32.lt_u
        (local.get $1)
        (i32.const 55296)
       )
      )
      (br_if $label$7
       (i32.ne
        (i32.and
         (local.get $1)
         (i32.const -8192)
        )
        (i32.const 57344)
       )
      )
     )
     (i32.store8 offset=2
      (local.get $0)
      (i32.or
       (i32.and
        (local.get $1)
        (i32.const 63)
       )
       (i32.const 128)
      )
     )
     (i32.store8
      (local.get $0)
      (i32.or
       (i32.shr_u
        (local.get $1)
        (i32.const 12)
       )
       (i32.const 224)
      )
     )
     (i32.store8 offset=1
      (local.get $0)
      (i32.or
       (i32.and
        (i32.shr_u
         (local.get $1)
         (i32.const 6)
        )
        (i32.const 63)
       )
       (i32.const 128)
      )
     )
     (return
      (i32.const 3)
     )
    )
    (block $label$9
     (br_if $label$9
      (i32.gt_u
       (i32.add
        (local.get $1)
        (i32.const -65536)
       )
       (i32.const 1048575)
      )
     )
     (i32.store8 offset=3
      (local.get $0)
      (i32.or
       (i32.and
        (local.get $1)
        (i32.const 63)
       )
       (i32.const 128)
      )
     )
     (i32.store8
      (local.get $0)
      (i32.or
       (i32.shr_u
        (local.get $1)
        (i32.const 18)
       )
       (i32.const 240)
      )
     )
     (i32.store8 offset=2
      (local.get $0)
      (i32.or
       (i32.and
        (i32.shr_u
         (local.get $1)
         (i32.const 6)
        )
        (i32.const 63)
       )
       (i32.const 128)
      )
     )
     (i32.store8 offset=1
      (local.get $0)
      (i32.or
       (i32.and
        (i32.shr_u
         (local.get $1)
         (i32.const 12)
        )
        (i32.const 63)
       )
       (i32.const 128)
      )
     )
     (return
      (i32.const 4)
     )
    )
    (i32.store offset=3668
     (i32.const 0)
     (i32.const 25)
    )
   )
   (local.set $3
    (i32.const -1)
   )
  )
  (local.get $3)
 )
 (func $wctomb (param $0 i32) (param $1 i32) (result i32)
  (block $label$1
   (br_if $label$1
    (local.get $0)
   )
   (return
    (i32.const 0)
   )
  )
  (call $wcrtomb
   (local.get $0)
   (local.get $1)
   (i32.const 0)
  )
 )
 (func $frexp (param $0 f64) (param $1 i32) (result f64)
  (local $2 i64)
  (local $3 i32)
  (block $label$1
   (br_if $label$1
    (i32.eq
     (local.tee $3
      (i32.and
       (i32.wrap_i64
        (i64.shr_u
         (local.tee $2
          (i64.reinterpret_f64
           (local.get $0)
          )
         )
         (i64.const 52)
        )
       )
       (i32.const 2047)
      )
     )
     (i32.const 2047)
    )
   )
   (block $label$2
    (br_if $label$2
     (local.get $3)
    )
    (block $label$3
     (br_if $label$3
      (f64.ne
       (local.get $0)
       (f64.const 0)
      )
     )
     (i32.store
      (local.get $1)
      (i32.const 0)
     )
     (return
      (local.get $0)
     )
    )
    (local.set $0
     (call $frexp
      (f64.mul
       (local.get $0)
       (f64.const 18446744073709551615)
      )
      (local.get $1)
     )
    )
    (i32.store
     (local.get $1)
     (i32.add
      (i32.load
       (local.get $1)
      )
      (i32.const -64)
     )
    )
    (return
     (local.get $0)
    )
   )
   (i32.store
    (local.get $1)
    (i32.add
     (local.get $3)
     (i32.const -1022)
    )
   )
   (local.set $0
    (f64.reinterpret_i64
     (i64.or
      (i64.and
       (local.get $2)
       (i64.const -9218868437227405313)
      )
      (i64.const 4602678819172646912)
     )
    )
   )
  )
  (local.get $0)
 )
 (func $fputs (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local.set $2
   (call $strlen
    (local.get $0)
   )
  )
  (select
   (i32.const -1)
   (i32.const 0)
   (i32.ne
    (local.get $2)
    (call $fwrite
     (local.get $0)
     (i32.const 1)
     (local.get $2)
     (local.get $1)
    )
   )
  )
 )
 (func $abort
  (unreachable)
 )
 (func $vfprintf (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (global.set $__stack_pointer
   (local.tee $3
    (i32.sub
     (global.get $__stack_pointer)
     (i32.const 208)
    )
   )
  )
  (i32.store offset=204
   (local.get $3)
   (local.get $2)
  )
  (i64.store
   (i32.add
    (i32.add
     (local.get $3)
     (i32.const 160)
    )
    (i32.const 32)
   )
   (i64.const 0)
  )
  (i64.store
   (i32.add
    (local.get $3)
    (i32.const 184)
   )
   (i64.const 0)
  )
  (i64.store
   (i32.add
    (local.get $3)
    (i32.const 176)
   )
   (i64.const 0)
  )
  (i64.store offset=168
   (local.get $3)
   (i64.const 0)
  )
  (i64.store offset=160
   (local.get $3)
   (i64.const 0)
  )
  (i32.store offset=200
   (local.get $3)
   (local.get $2)
  )
  (block $label$1
   (block $label$2
    (br_if $label$2
     (i32.ge_s
      (call $printf_core
       (i32.const 0)
       (local.get $1)
       (i32.add
        (local.get $3)
        (i32.const 200)
       )
       (i32.add
        (local.get $3)
        (i32.const 80)
       )
       (i32.add
        (local.get $3)
        (i32.const 160)
       )
      )
      (i32.const 0)
     )
    )
    (local.set $0
     (i32.const -1)
    )
    (br $label$1)
   )
   (local.set $4
    (i32.load
     (local.get $0)
    )
   )
   (block $label$3
    (br_if $label$3
     (i32.gt_s
      (i32.load offset=60
       (local.get $0)
      )
      (i32.const 0)
     )
    )
    (i32.store
     (local.get $0)
     (i32.and
      (local.get $4)
      (i32.const -33)
     )
    )
   )
   (block $label$4
    (block $label$5
     (block $label$6
      (block $label$7
       (br_if $label$7
        (i32.load offset=44
         (local.get $0)
        )
       )
       (i32.store offset=44
        (local.get $0)
        (i32.const 80)
       )
       (i32.store offset=24
        (local.get $0)
        (i32.const 0)
       )
       (i64.store offset=16
        (local.get $0)
        (i64.const 0)
       )
       (local.set $5
        (i32.load offset=40
         (local.get $0)
        )
       )
       (i32.store offset=40
        (local.get $0)
        (local.get $3)
       )
       (br $label$6)
      )
      (local.set $5
       (i32.const 0)
      )
      (br_if $label$5
       (i32.load offset=16
        (local.get $0)
       )
      )
     )
     (local.set $2
      (i32.const -1)
     )
     (br_if $label$4
      (call $__towrite
       (local.get $0)
      )
     )
    )
    (local.set $2
     (call $printf_core
      (local.get $0)
      (local.get $1)
      (i32.add
       (local.get $3)
       (i32.const 200)
      )
      (i32.add
       (local.get $3)
       (i32.const 80)
      )
      (i32.add
       (local.get $3)
       (i32.const 160)
      )
     )
    )
   )
   (local.set $1
    (i32.and
     (local.get $4)
     (i32.const 32)
    )
   )
   (block $label$8
    (br_if $label$8
     (i32.eqz
      (local.get $5)
     )
    )
    (drop
     (call_indirect (type $1)
      (local.get $0)
      (i32.const 0)
      (i32.const 0)
      (i32.load offset=32
       (local.get $0)
      )
     )
    )
    (i32.store offset=44
     (local.get $0)
     (i32.const 0)
    )
    (i32.store offset=40
     (local.get $0)
     (local.get $5)
    )
    (i32.store offset=24
     (local.get $0)
     (i32.const 0)
    )
    (local.set $5
     (i32.load offset=20
      (local.get $0)
     )
    )
    (i64.store offset=16
     (local.get $0)
     (i64.const 0)
    )
    (local.set $2
     (select
      (local.get $2)
      (i32.const -1)
      (local.get $5)
     )
    )
   )
   (i32.store
    (local.get $0)
    (i32.or
     (local.tee $5
      (i32.load
       (local.get $0)
      )
     )
     (local.get $1)
    )
   )
   (local.set $0
    (select
     (i32.const -1)
     (local.get $2)
     (i32.and
      (local.get $5)
      (i32.const 32)
     )
    )
   )
  )
  (global.set $__stack_pointer
   (i32.add
    (local.get $3)
    (i32.const 208)
   )
  )
  (local.get $0)
 )
 (func $printf_core (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (result i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local $9 i32)
  (local $10 i32)
  (local $11 i32)
  (local $12 i32)
  (local $13 i32)
  (local $14 i32)
  (local $15 i32)
  (local $16 i32)
  (local $17 i32)
  (local $18 i32)
  (local $19 i32)
  (local $20 i32)
  (local $21 i32)
  (local $22 i32)
  (local $23 i32)
  (local $24 i32)
  (local $25 i32)
  (local $26 i32)
  (local $27 i32)
  (local $28 i32)
  (local $29 i32)
  (local $30 i32)
  (local $31 i64)
  (local $32 i64)
  (local $33 f64)
  (local $34 i32)
  (local $35 i32)
  (local $36 i32)
  (local $37 i32)
  (local $38 i32)
  (local $39 i32)
  (local $40 i32)
  (local $41 i32)
  (local $42 f64)
  (global.set $__stack_pointer
   (local.tee $5
    (i32.sub
     (global.get $__stack_pointer)
     (i32.const 880)
    )
   )
  )
  (local.set $6
   (i32.add
    (i32.add
     (local.get $5)
     (i32.const 68)
    )
    (i32.const 12)
   )
  )
  (local.set $7
   (i32.sub
    (i32.const 0)
    (i32.add
     (local.get $5)
     (i32.const 112)
    )
   )
  )
  (local.set $8
   (i32.add
    (local.get $5)
    (i32.const -3988)
   )
  )
  (local.set $9
   (i32.add
    (local.get $5)
    (i32.const 55)
   )
  )
  (local.set $10
   (i32.add
    (i32.add
     (local.get $5)
     (i32.const 68)
    )
    (i32.const 11)
   )
  )
  (local.set $11
   (i32.add
    (i32.add
     (local.get $5)
     (i32.const 80)
    )
    (i32.const -1)
   )
  )
  (local.set $12
   (i32.or
    (i32.add
     (local.get $5)
     (i32.const 80)
    )
    (i32.const 8)
   )
  )
  (local.set $13
   (i32.or
    (i32.add
     (local.get $5)
     (i32.const 80)
    )
    (i32.const 9)
   )
  )
  (local.set $14
   (i32.add
    (i32.add
     (local.get $5)
     (i32.const 68)
    )
    (i32.const 10)
   )
  )
  (local.set $15
   (i32.add
    (local.get $5)
    (i32.const 56)
   )
  )
  (local.set $16
   (i32.const 0)
  )
  (local.set $17
   (i32.const 0)
  )
  (local.set $18
   (i32.const 0)
  )
  (block $label$1
   (block $label$2
    (block $label$3
     (loop $label$4
      (local.set $19
       (local.get $1)
      )
      (br_if $label$3
       (i32.gt_s
        (local.get $18)
        (i32.xor
         (local.get $17)
         (i32.const 2147483647)
        )
       )
      )
      (local.set $17
       (i32.add
        (local.get $18)
        (local.get $17)
       )
      )
      (block $label$5
       (block $label$6
        (block $label$7
         (block $label$8
          (block $label$9
           (block $label$10
            (block $label$11
             (block $label$12
              (block $label$13
               (br_if $label$13
                (i32.eqz
                 (local.tee $18
                  (i32.load8_u
                   (local.get $19)
                  )
                 )
                )
               )
               (local.set $1
                (local.get $19)
               )
               (loop $label$14
                (block $label$15
                 (block $label$16
                  (block $label$17
                   (br_if $label$17
                    (i32.eqz
                     (local.tee $18
                      (i32.and
                       (local.get $18)
                       (i32.const 255)
                      )
                     )
                    )
                   )
                   (br_if $label$15
                    (i32.ne
                     (local.get $18)
                     (i32.const 37)
                    )
                   )
                   (local.set $20
                    (local.get $1)
                   )
                   (local.set $18
                    (local.get $1)
                   )
                   (loop $label$18
                    (block $label$19
                     (br_if $label$19
                      (i32.eq
                       (i32.load8_u offset=1
                        (local.get $18)
                       )
                       (i32.const 37)
                      )
                     )
                     (local.set $1
                      (local.get $18)
                     )
                     (br $label$16)
                    )
                    (local.set $20
                     (i32.add
                      (local.get $20)
                      (i32.const 1)
                     )
                    )
                    (local.set $21
                     (i32.load8_u offset=2
                      (local.get $18)
                     )
                    )
                    (local.set $18
                     (local.tee $1
                      (i32.add
                       (local.get $18)
                       (i32.const 2)
                      )
                     )
                    )
                    (br_if $label$18
                     (i32.eq
                      (local.get $21)
                      (i32.const 37)
                     )
                    )
                    (br $label$16)
                   )
                  )
                  (local.set $20
                   (local.get $1)
                  )
                 )
                 (br_if $label$3
                  (i32.gt_s
                   (local.tee $18
                    (i32.sub
                     (local.get $20)
                     (local.get $19)
                    )
                   )
                   (local.tee $20
                    (i32.xor
                     (local.get $17)
                     (i32.const 2147483647)
                    )
                   )
                  )
                 )
                 (block $label$20
                  (br_if $label$20
                   (i32.eqz
                    (local.get $0)
                   )
                  )
                  (br_if $label$20
                   (i32.and
                    (i32.load8_u
                     (local.get $0)
                    )
                    (i32.const 32)
                   )
                  )
                  (drop
                   (call $__fwritex
                    (local.get $19)
                    (local.get $18)
                    (local.get $0)
                   )
                  )
                 )
                 (br_if $label$4
                  (local.get $18)
                 )
                 (local.set $18
                  (i32.add
                   (local.get $1)
                   (i32.const 1)
                  )
                 )
                 (local.set $22
                  (i32.const -1)
                 )
                 (block $label$21
                  (br_if $label$21
                   (i32.gt_u
                    (local.tee $21
                     (i32.add
                      (local.tee $23
                       (i32.load8_s offset=1
                        (local.get $1)
                       )
                      )
                      (i32.const -48)
                     )
                    )
                    (i32.const 9)
                   )
                  )
                  (br_if $label$21
                   (i32.ne
                    (i32.load8_u offset=2
                     (local.get $1)
                    )
                    (i32.const 36)
                   )
                  )
                  (local.set $18
                   (i32.add
                    (local.get $1)
                    (i32.const 3)
                   )
                  )
                  (local.set $23
                   (i32.load8_s offset=3
                    (local.get $1)
                   )
                  )
                  (local.set $16
                   (i32.const 1)
                  )
                  (local.set $22
                   (local.get $21)
                  )
                 )
                 (local.set $24
                  (i32.const 0)
                 )
                 (block $label$22
                  (br_if $label$22
                   (i32.gt_u
                    (local.tee $1
                     (i32.add
                      (local.get $23)
                      (i32.const -32)
                     )
                    )
                    (i32.const 31)
                   )
                  )
                  (br_if $label$22
                   (i32.eqz
                    (i32.and
                     (local.tee $1
                      (i32.shl
                       (i32.const 1)
                       (local.get $1)
                      )
                     )
                     (i32.const 75913)
                    )
                   )
                  )
                  (local.set $21
                   (i32.add
                    (local.get $18)
                    (i32.const 1)
                   )
                  )
                  (local.set $24
                   (i32.const 0)
                  )
                  (loop $label$23
                   (local.set $24
                    (i32.or
                     (local.get $1)
                     (local.get $24)
                    )
                   )
                   (br_if $label$22
                    (i32.ge_u
                     (local.tee $1
                      (i32.add
                       (local.tee $23
                        (i32.load8_s
                         (local.tee $18
                          (local.get $21)
                         )
                        )
                       )
                       (i32.const -32)
                      )
                     )
                     (i32.const 32)
                    )
                   )
                   (local.set $21
                    (i32.add
                     (local.get $18)
                     (i32.const 1)
                    )
                   )
                   (br_if $label$23
                    (i32.and
                     (local.tee $1
                      (i32.shl
                       (i32.const 1)
                       (local.get $1)
                      )
                     )
                     (i32.const 75913)
                    )
                   )
                  )
                 )
                 (block $label$24
                  (br_if $label$24
                   (i32.ne
                    (local.get $23)
                    (i32.const 42)
                   )
                  )
                  (block $label$25
                   (block $label$26
                    (br_if $label$26
                     (i32.gt_u
                      (local.tee $1
                       (i32.add
                        (i32.load8_s offset=1
                         (local.get $18)
                        )
                        (i32.const -48)
                       )
                      )
                      (i32.const 9)
                     )
                    )
                    (br_if $label$26
                     (i32.ne
                      (i32.load8_u offset=2
                       (local.get $18)
                      )
                      (i32.const 36)
                     )
                    )
                    (i32.store
                     (i32.add
                      (local.get $4)
                      (i32.shl
                       (local.get $1)
                       (i32.const 2)
                      )
                     )
                     (i32.const 10)
                    )
                    (local.set $21
                     (i32.add
                      (local.get $18)
                      (i32.const 3)
                     )
                    )
                    (local.set $25
                     (i32.load
                      (i32.add
                       (i32.add
                        (i32.shl
                         (i32.load8_s offset=1
                          (local.get $18)
                         )
                         (i32.const 3)
                        )
                        (local.get $3)
                       )
                       (i32.const -384)
                      )
                     )
                    )
                    (local.set $16
                     (i32.const 1)
                    )
                    (br $label$25)
                   )
                   (br_if $label$11
                    (local.get $16)
                   )
                   (local.set $21
                    (i32.add
                     (local.get $18)
                     (i32.const 1)
                    )
                   )
                   (block $label$27
                    (br_if $label$27
                     (local.get $0)
                    )
                    (local.set $16
                     (i32.const 0)
                    )
                    (local.set $25
                     (i32.const 0)
                    )
                    (br $label$12)
                   )
                   (i32.store
                    (local.get $2)
                    (i32.add
                     (local.tee $1
                      (i32.load
                       (local.get $2)
                      )
                     )
                     (i32.const 4)
                    )
                   )
                   (local.set $25
                    (i32.load
                     (local.get $1)
                    )
                   )
                   (local.set $16
                    (i32.const 0)
                   )
                  )
                  (br_if $label$12
                   (i32.gt_s
                    (local.get $25)
                    (i32.const -1)
                   )
                  )
                  (local.set $25
                   (i32.sub
                    (i32.const 0)
                    (local.get $25)
                   )
                  )
                  (local.set $24
                   (i32.or
                    (local.get $24)
                    (i32.const 8192)
                   )
                  )
                  (br $label$12)
                 )
                 (local.set $25
                  (i32.const 0)
                 )
                 (block $label$28
                  (br_if $label$28
                   (i32.le_u
                    (local.tee $1
                     (i32.add
                      (local.get $23)
                      (i32.const -48)
                     )
                    )
                    (i32.const 9)
                   )
                  )
                  (local.set $21
                   (local.get $18)
                  )
                  (br $label$12)
                 )
                 (local.set $25
                  (i32.const 0)
                 )
                 (loop $label$29
                  (block $label$30
                   (br_if $label$30
                    (i32.gt_u
                     (local.get $25)
                     (i32.const 214748364)
                    )
                   )
                   (local.set $25
                    (select
                     (i32.const -1)
                     (i32.add
                      (local.tee $21
                       (i32.mul
                        (local.get $25)
                        (i32.const 10)
                       )
                      )
                      (local.get $1)
                     )
                     (i32.gt_u
                      (local.get $1)
                      (i32.xor
                       (local.get $21)
                       (i32.const 2147483647)
                      )
                     )
                    )
                   )
                   (local.set $1
                    (i32.load8_s offset=1
                     (local.get $18)
                    )
                   )
                   (local.set $18
                    (local.tee $21
                     (i32.add
                      (local.get $18)
                      (i32.const 1)
                     )
                    )
                   )
                   (br_if $label$29
                    (i32.lt_u
                     (local.tee $1
                      (i32.add
                       (local.get $1)
                       (i32.const -48)
                      )
                     )
                     (i32.const 10)
                    )
                   )
                   (br_if $label$3
                    (i32.lt_s
                     (local.get $25)
                     (i32.const 0)
                    )
                   )
                   (br $label$12)
                  )
                  (local.set $1
                   (i32.load8_s offset=1
                    (local.get $18)
                   )
                  )
                  (local.set $25
                   (i32.const -1)
                  )
                  (local.set $18
                   (i32.add
                    (local.get $18)
                    (i32.const 1)
                   )
                  )
                  (br_if $label$29
                   (i32.lt_u
                    (local.tee $1
                     (i32.add
                      (local.get $1)
                      (i32.const -48)
                     )
                    )
                    (i32.const 10)
                   )
                  )
                  (br $label$3)
                 )
                )
                (local.set $18
                 (i32.load8_u offset=1
                  (local.get $1)
                 )
                )
                (local.set $1
                 (i32.add
                  (local.get $1)
                  (i32.const 1)
                 )
                )
                (br $label$14)
               )
              )
              (br_if $label$1
               (local.get $0)
              )
              (block $label$31
               (br_if $label$31
                (local.get $16)
               )
               (local.set $17
                (i32.const 0)
               )
               (br $label$1)
              )
              (block $label$32
               (block $label$33
                (block $label$34
                 (br_if $label$34
                  (local.tee $1
                   (i32.load offset=4
                    (local.get $4)
                   )
                  )
                 )
                 (local.set $1
                  (i32.const 1)
                 )
                 (br $label$33)
                )
                (call $pop_arg
                 (i32.add
                  (local.get $3)
                  (i32.const 8)
                 )
                 (local.get $1)
                 (local.get $2)
                )
                (block $label$35
                 (br_if $label$35
                  (local.tee $1
                   (i32.load offset=8
                    (local.get $4)
                   )
                  )
                 )
                 (local.set $1
                  (i32.const 2)
                 )
                 (br $label$33)
                )
                (call $pop_arg
                 (i32.add
                  (local.get $3)
                  (i32.const 16)
                 )
                 (local.get $1)
                 (local.get $2)
                )
                (block $label$36
                 (br_if $label$36
                  (local.tee $1
                   (i32.load offset=12
                    (local.get $4)
                   )
                  )
                 )
                 (local.set $1
                  (i32.const 3)
                 )
                 (br $label$33)
                )
                (call $pop_arg
                 (i32.add
                  (local.get $3)
                  (i32.const 24)
                 )
                 (local.get $1)
                 (local.get $2)
                )
                (block $label$37
                 (br_if $label$37
                  (local.tee $1
                   (i32.load offset=16
                    (local.get $4)
                   )
                  )
                 )
                 (local.set $1
                  (i32.const 4)
                 )
                 (br $label$33)
                )
                (call $pop_arg
                 (i32.add
                  (local.get $3)
                  (i32.const 32)
                 )
                 (local.get $1)
                 (local.get $2)
                )
                (block $label$38
                 (br_if $label$38
                  (local.tee $1
                   (i32.load offset=20
                    (local.get $4)
                   )
                  )
                 )
                 (local.set $1
                  (i32.const 5)
                 )
                 (br $label$33)
                )
                (call $pop_arg
                 (i32.add
                  (local.get $3)
                  (i32.const 40)
                 )
                 (local.get $1)
                 (local.get $2)
                )
                (block $label$39
                 (br_if $label$39
                  (local.tee $1
                   (i32.load offset=24
                    (local.get $4)
                   )
                  )
                 )
                 (local.set $1
                  (i32.const 6)
                 )
                 (br $label$33)
                )
                (call $pop_arg
                 (i32.add
                  (local.get $3)
                  (i32.const 48)
                 )
                 (local.get $1)
                 (local.get $2)
                )
                (block $label$40
                 (br_if $label$40
                  (local.tee $1
                   (i32.load offset=28
                    (local.get $4)
                   )
                  )
                 )
                 (local.set $1
                  (i32.const 7)
                 )
                 (br $label$33)
                )
                (call $pop_arg
                 (i32.add
                  (local.get $3)
                  (i32.const 56)
                 )
                 (local.get $1)
                 (local.get $2)
                )
                (block $label$41
                 (br_if $label$41
                  (local.tee $1
                   (i32.load offset=32
                    (local.get $4)
                   )
                  )
                 )
                 (local.set $1
                  (i32.const 8)
                 )
                 (br $label$33)
                )
                (call $pop_arg
                 (i32.add
                  (local.get $3)
                  (i32.const 64)
                 )
                 (local.get $1)
                 (local.get $2)
                )
                (br_if $label$32
                 (local.tee $1
                  (i32.load offset=36
                   (local.get $4)
                  )
                 )
                )
                (local.set $1
                 (i32.const 9)
                )
               )
               (local.set $1
                (i32.shl
                 (local.get $1)
                 (i32.const 2)
                )
               )
               (loop $label$42
                (br_if $label$11
                 (i32.load
                  (i32.add
                   (local.get $4)
                   (local.get $1)
                  )
                 )
                )
                (br_if $label$42
                 (i32.ne
                  (local.tee $1
                   (i32.add
                    (local.get $1)
                    (i32.const 4)
                   )
                  )
                  (i32.const 40)
                 )
                )
               )
               (local.set $17
                (i32.const 1)
               )
               (br $label$1)
              )
              (call $pop_arg
               (i32.add
                (local.get $3)
                (i32.const 72)
               )
               (local.get $1)
               (local.get $2)
              )
              (local.set $17
               (i32.const 1)
              )
              (br $label$1)
             )
             (local.set $18
              (i32.const 0)
             )
             (local.set $23
              (i32.const -1)
             )
             (block $label$43
              (block $label$44
               (br_if $label$44
                (i32.eq
                 (i32.load8_u
                  (local.get $21)
                 )
                 (i32.const 46)
                )
               )
               (local.set $1
                (local.get $21)
               )
               (local.set $26
                (i32.const 0)
               )
               (br $label$43)
              )
              (block $label$45
               (br_if $label$45
                (i32.ne
                 (local.tee $23
                  (i32.load8_s offset=1
                   (local.get $21)
                  )
                 )
                 (i32.const 42)
                )
               )
               (block $label$46
                (block $label$47
                 (br_if $label$47
                  (i32.gt_u
                   (local.tee $1
                    (i32.add
                     (i32.load8_s offset=2
                      (local.get $21)
                     )
                     (i32.const -48)
                    )
                   )
                   (i32.const 9)
                  )
                 )
                 (br_if $label$47
                  (i32.ne
                   (i32.load8_u offset=3
                    (local.get $21)
                   )
                   (i32.const 36)
                  )
                 )
                 (i32.store
                  (i32.add
                   (local.get $4)
                   (i32.shl
                    (local.get $1)
                    (i32.const 2)
                   )
                  )
                  (i32.const 10)
                 )
                 (local.set $1
                  (i32.add
                   (local.get $21)
                   (i32.const 4)
                  )
                 )
                 (local.set $23
                  (i32.load
                   (i32.add
                    (i32.add
                     (i32.shl
                      (i32.load8_s offset=2
                       (local.get $21)
                      )
                      (i32.const 3)
                     )
                     (local.get $3)
                    )
                    (i32.const -384)
                   )
                  )
                 )
                 (br $label$46)
                )
                (br_if $label$11
                 (local.get $16)
                )
                (local.set $1
                 (i32.add
                  (local.get $21)
                  (i32.const 2)
                 )
                )
                (block $label$48
                 (br_if $label$48
                  (local.get $0)
                 )
                 (local.set $23
                  (i32.const 0)
                 )
                 (br $label$46)
                )
                (i32.store
                 (local.get $2)
                 (i32.add
                  (local.tee $21
                   (i32.load
                    (local.get $2)
                   )
                  )
                  (i32.const 4)
                 )
                )
                (local.set $23
                 (i32.load
                  (local.get $21)
                 )
                )
               )
               (local.set $26
                (i32.shr_u
                 (i32.xor
                  (local.get $23)
                  (i32.const -1)
                 )
                 (i32.const 31)
                )
               )
               (br $label$43)
              )
              (local.set $1
               (i32.add
                (local.get $21)
                (i32.const 1)
               )
              )
              (block $label$49
               (br_if $label$49
                (i32.le_u
                 (local.tee $27
                  (i32.add
                   (local.get $23)
                   (i32.const -48)
                  )
                 )
                 (i32.const 9)
                )
               )
               (local.set $26
                (i32.const 1)
               )
               (local.set $23
                (i32.const 0)
               )
               (br $label$43)
              )
              (local.set $28
               (i32.const 0)
              )
              (local.set $21
               (local.get $1)
              )
              (loop $label$50
               (local.set $23
                (i32.const -1)
               )
               (block $label$51
                (br_if $label$51
                 (i32.gt_u
                  (local.get $28)
                  (i32.const 214748364)
                 )
                )
                (local.set $23
                 (select
                  (i32.const -1)
                  (i32.add
                   (local.tee $1
                    (i32.mul
                     (local.get $28)
                     (i32.const 10)
                    )
                   )
                   (local.get $27)
                  )
                  (i32.gt_u
                   (local.get $27)
                   (i32.xor
                    (local.get $1)
                    (i32.const 2147483647)
                   )
                  )
                 )
                )
               )
               (local.set $26
                (i32.const 1)
               )
               (local.set $27
                (i32.load8_s offset=1
                 (local.get $21)
                )
               )
               (local.set $28
                (local.get $23)
               )
               (local.set $21
                (local.tee $1
                 (i32.add
                  (local.get $21)
                  (i32.const 1)
                 )
                )
               )
               (br_if $label$50
                (i32.lt_u
                 (local.tee $27
                  (i32.add
                   (local.get $27)
                   (i32.const -48)
                  )
                 )
                 (i32.const 10)
                )
               )
              )
             )
             (loop $label$52
              (local.set $21
               (local.get $18)
              )
              (br_if $label$11
               (i32.lt_u
                (i32.add
                 (local.tee $18
                  (i32.load8_s
                   (local.get $1)
                  )
                 )
                 (i32.const -123)
                )
                (i32.const -58)
               )
              )
              (local.set $1
               (i32.add
                (local.get $1)
                (i32.const 1)
               )
              )
              (br_if $label$52
               (i32.lt_u
                (i32.add
                 (local.tee $18
                  (i32.load8_u
                   (i32.add
                    (i32.add
                     (local.get $18)
                     (i32.mul
                      (local.get $21)
                      (i32.const 58)
                     )
                    )
                    (i32.const 2879)
                   )
                  )
                 )
                 (i32.const -1)
                )
                (i32.const 8)
               )
              )
             )
             (block $label$53
              (block $label$54
               (block $label$55
                (br_if $label$55
                 (i32.eq
                  (local.get $18)
                  (i32.const 27)
                 )
                )
                (br_if $label$11
                 (i32.eqz
                  (local.get $18)
                 )
                )
                (block $label$56
                 (br_if $label$56
                  (i32.lt_s
                   (local.get $22)
                   (i32.const 0)
                  )
                 )
                 (i32.store
                  (i32.add
                   (local.get $4)
                   (i32.shl
                    (local.get $22)
                    (i32.const 2)
                   )
                  )
                  (local.get $18)
                 )
                 (i64.store offset=56
                  (local.get $5)
                  (i64.load
                   (i32.add
                    (local.get $3)
                    (i32.shl
                     (local.get $22)
                     (i32.const 3)
                    )
                   )
                  )
                 )
                 (br $label$54)
                )
                (block $label$57
                 (br_if $label$57
                  (local.get $0)
                 )
                 (local.set $17
                  (i32.const 0)
                 )
                 (br $label$1)
                )
                (call $pop_arg
                 (i32.add
                  (local.get $5)
                  (i32.const 56)
                 )
                 (local.get $18)
                 (local.get $2)
                )
                (br $label$53)
               )
               (br_if $label$11
                (i32.gt_s
                 (local.get $22)
                 (i32.const -1)
                )
               )
              )
              (local.set $18
               (i32.const 0)
              )
              (br_if $label$4
               (i32.eqz
                (local.get $0)
               )
              )
             )
             (local.set $22
              (select
               (local.tee $28
                (i32.and
                 (local.get $24)
                 (i32.const -65537)
                )
               )
               (local.get $24)
               (i32.and
                (local.get $24)
                (i32.const 8192)
               )
              )
             )
             (block $label$58
              (block $label$59
               (block $label$60
                (block $label$61
                 (block $label$62
                  (block $label$63
                   (block $label$64
                    (block $label$65
                     (block $label$66
                      (block $label$67
                       (block $label$68
                        (block $label$69
                         (block $label$70
                          (block $label$71
                           (block $label$72
                            (block $label$73
                             (block $label$74
                              (br_table $label$58 $label$10 $label$61 $label$10 $label$58 $label$58 $label$58 $label$10 $label$10 $label$10 $label$10 $label$10 $label$10 $label$10 $label$10 $label$10 $label$10 $label$10 $label$62 $label$10 $label$10 $label$10 $label$10 $label$71 $label$10 $label$10 $label$10 $label$10 $label$10 $label$10 $label$10 $label$10 $label$58 $label$10 $label$66 $label$69 $label$58 $label$58 $label$58 $label$10 $label$69 $label$10 $label$10 $label$10 $label$65 $label$73 $label$70 $label$72 $label$10 $label$10 $label$64 $label$10 $label$74 $label$10 $label$10 $label$71 $label$10
                               (i32.add
                                (local.tee $29
                                 (select
                                  (select
                                   (i32.and
                                    (local.tee $18
                                     (i32.load8_s
                                      (i32.add
                                       (local.get $1)
                                       (i32.const -1)
                                      )
                                     )
                                    )
                                    (i32.const -33)
                                   )
                                   (local.get $18)
                                   (i32.eq
                                    (i32.and
                                     (local.get $18)
                                     (i32.const 15)
                                    )
                                    (i32.const 3)
                                   )
                                  )
                                  (local.get $18)
                                  (local.get $21)
                                 )
                                )
                                (i32.const -65)
                               )
                              )
                             )
                             (local.set $27
                              (i32.const 0)
                             )
                             (local.set $30
                              (i32.const 1024)
                             )
                             (local.set $31
                              (i64.load offset=56
                               (local.get $5)
                              )
                             )
                             (br $label$68)
                            )
                            (local.set $18
                             (i32.const 0)
                            )
                            (block $label$75
                             (block $label$76
                              (block $label$77
                               (block $label$78
                                (block $label$79
                                 (block $label$80
                                  (block $label$81
                                   (br_table $label$81 $label$80 $label$79 $label$78 $label$77 $label$4 $label$76 $label$75 $label$4
                                    (i32.and
                                     (local.get $21)
                                     (i32.const 255)
                                    )
                                   )
                                  )
                                  (i32.store
                                   (i32.load offset=56
                                    (local.get $5)
                                   )
                                   (local.get $17)
                                  )
                                  (br $label$4)
                                 )
                                 (i32.store
                                  (i32.load offset=56
                                   (local.get $5)
                                  )
                                  (local.get $17)
                                 )
                                 (br $label$4)
                                )
                                (i64.store
                                 (i32.load offset=56
                                  (local.get $5)
                                 )
                                 (i64.extend_i32_s
                                  (local.get $17)
                                 )
                                )
                                (br $label$4)
                               )
                               (i32.store16
                                (i32.load offset=56
                                 (local.get $5)
                                )
                                (local.get $17)
                               )
                               (br $label$4)
                              )
                              (i32.store8
                               (i32.load offset=56
                                (local.get $5)
                               )
                               (local.get $17)
                              )
                              (br $label$4)
                             )
                             (i32.store
                              (i32.load offset=56
                               (local.get $5)
                              )
                              (local.get $17)
                             )
                             (br $label$4)
                            )
                            (i64.store
                             (i32.load offset=56
                              (local.get $5)
                             )
                             (i64.extend_i32_s
                              (local.get $17)
                             )
                            )
                            (br $label$4)
                           )
                           (local.set $23
                            (select
                             (local.get $23)
                             (i32.const 8)
                             (i32.gt_u
                              (local.get $23)
                              (i32.const 8)
                             )
                            )
                           )
                           (local.set $22
                            (i32.or
                             (local.get $22)
                             (i32.const 8)
                            )
                           )
                           (local.set $29
                            (i32.const 120)
                           )
                          )
                          (local.set $27
                           (i32.const 0)
                          )
                          (local.set $30
                           (i32.const 1024)
                          )
                          (block $label$82
                           (br_if $label$82
                            (i32.eqz
                             (i64.eqz
                              (local.tee $31
                               (i64.load offset=56
                                (local.get $5)
                               )
                              )
                             )
                            )
                           )
                           (local.set $19
                            (local.get $15)
                           )
                           (br $label$67)
                          )
                          (local.set $21
                           (i32.and
                            (local.get $29)
                            (i32.const 32)
                           )
                          )
                          (local.set $19
                           (local.get $15)
                          )
                          (loop $label$83
                           (i32.store8
                            (local.tee $19
                             (i32.add
                              (local.get $19)
                              (i32.const -1)
                             )
                            )
                            (i32.or
                             (i32.load8_u
                              (i32.add
                               (i32.and
                                (i32.wrap_i64
                                 (local.get $31)
                                )
                                (i32.const 15)
                               )
                               (i32.const 3408)
                              )
                             )
                             (local.get $21)
                            )
                           )
                           (local.set $18
                            (i64.gt_u
                             (local.get $31)
                             (i64.const 15)
                            )
                           )
                           (local.set $31
                            (i64.shr_u
                             (local.get $31)
                             (i64.const 4)
                            )
                           )
                           (br_if $label$83
                            (local.get $18)
                           )
                          )
                          (br_if $label$67
                           (i32.eqz
                            (i32.and
                             (local.get $22)
                             (i32.const 8)
                            )
                           )
                          )
                          (local.set $30
                           (i32.add
                            (i32.shr_s
                             (local.get $29)
                             (i32.const 4)
                            )
                            (i32.const 1024)
                           )
                          )
                          (local.set $27
                           (i32.const 2)
                          )
                          (br $label$67)
                         )
                         (local.set $19
                          (local.get $15)
                         )
                         (block $label$84
                          (br_if $label$84
                           (i64.eqz
                            (local.tee $31
                             (i64.load offset=56
                              (local.get $5)
                             )
                            )
                           )
                          )
                          (local.set $19
                           (local.get $15)
                          )
                          (loop $label$85
                           (i32.store8
                            (local.tee $19
                             (i32.add
                              (local.get $19)
                              (i32.const -1)
                             )
                            )
                            (i32.or
                             (i32.and
                              (i32.wrap_i64
                               (local.get $31)
                              )
                              (i32.const 7)
                             )
                             (i32.const 48)
                            )
                           )
                           (local.set $18
                            (i64.gt_u
                             (local.get $31)
                             (i64.const 7)
                            )
                           )
                           (local.set $31
                            (i64.shr_u
                             (local.get $31)
                             (i64.const 3)
                            )
                           )
                           (br_if $label$85
                            (local.get $18)
                           )
                          )
                         )
                         (local.set $27
                          (i32.const 0)
                         )
                         (local.set $30
                          (i32.const 1024)
                         )
                         (br_if $label$67
                          (i32.eqz
                           (i32.and
                            (local.get $22)
                            (i32.const 8)
                           )
                          )
                         )
                         (local.set $23
                          (select
                           (local.get $23)
                           (i32.add
                            (local.tee $18
                             (i32.sub
                              (local.get $15)
                              (local.get $19)
                             )
                            )
                            (i32.const 1)
                           )
                           (i32.gt_s
                            (local.get $23)
                            (local.get $18)
                           )
                          )
                         )
                         (br $label$67)
                        )
                        (block $label$86
                         (br_if $label$86
                          (i64.gt_s
                           (local.tee $31
                            (i64.load offset=56
                             (local.get $5)
                            )
                           )
                           (i64.const -1)
                          )
                         )
                         (i64.store offset=56
                          (local.get $5)
                          (local.tee $31
                           (i64.sub
                            (i64.const 0)
                            (local.get $31)
                           )
                          )
                         )
                         (local.set $27
                          (i32.const 1)
                         )
                         (local.set $30
                          (i32.const 1024)
                         )
                         (br $label$68)
                        )
                        (block $label$87
                         (br_if $label$87
                          (i32.eqz
                           (i32.and
                            (local.get $22)
                            (i32.const 2048)
                           )
                          )
                         )
                         (local.set $27
                          (i32.const 1)
                         )
                         (local.set $30
                          (i32.const 1025)
                         )
                         (br $label$68)
                        )
                        (local.set $30
                         (select
                          (i32.const 1026)
                          (i32.const 1024)
                          (local.tee $27
                           (i32.and
                            (local.get $22)
                            (i32.const 1)
                           )
                          )
                         )
                        )
                       )
                       (block $label$88
                        (block $label$89
                         (br_if $label$89
                          (i64.ge_u
                           (local.get $31)
                           (i64.const 4294967296)
                          )
                         )
                         (local.set $32
                          (local.get $31)
                         )
                         (local.set $19
                          (local.get $15)
                         )
                         (br $label$88)
                        )
                        (local.set $19
                         (local.get $15)
                        )
                        (loop $label$90
                         (i32.store8
                          (local.tee $19
                           (i32.add
                            (local.get $19)
                            (i32.const -1)
                           )
                          )
                          (i32.or
                           (i32.wrap_i64
                            (i64.sub
                             (local.get $31)
                             (i64.mul
                              (local.tee $32
                               (i64.div_u
                                (local.get $31)
                                (i64.const 10)
                               )
                              )
                              (i64.const 10)
                             )
                            )
                           )
                           (i32.const 48)
                          )
                         )
                         (local.set $18
                          (i64.gt_u
                           (local.get $31)
                           (i64.const 42949672959)
                          )
                         )
                         (local.set $31
                          (local.get $32)
                         )
                         (br_if $label$90
                          (local.get $18)
                         )
                        )
                       )
                       (br_if $label$67
                        (i32.eqz
                         (local.tee $18
                          (i32.wrap_i64
                           (local.get $32)
                          )
                         )
                        )
                       )
                       (loop $label$91
                        (i32.store8
                         (local.tee $19
                          (i32.add
                           (local.get $19)
                           (i32.const -1)
                          )
                         )
                         (i32.or
                          (i32.sub
                           (local.get $18)
                           (i32.mul
                            (local.tee $21
                             (i32.div_u
                              (local.get $18)
                              (i32.const 10)
                             )
                            )
                            (i32.const 10)
                           )
                          )
                          (i32.const 48)
                         )
                        )
                        (local.set $24
                         (i32.gt_u
                          (local.get $18)
                          (i32.const 9)
                         )
                        )
                        (local.set $18
                         (local.get $21)
                        )
                        (br_if $label$91
                         (local.get $24)
                        )
                       )
                      )
                      (block $label$92
                       (br_if $label$92
                        (i32.eqz
                         (local.get $26)
                        )
                       )
                       (br_if $label$3
                        (i32.lt_s
                         (local.get $23)
                         (i32.const 0)
                        )
                       )
                      )
                      (local.set $28
                       (select
                        (i32.and
                         (local.get $22)
                         (i32.const -65537)
                        )
                        (local.get $22)
                        (local.get $26)
                       )
                      )
                      (block $label$93
                       (br_if $label$93
                        (i64.ne
                         (local.tee $31
                          (i64.load offset=56
                           (local.get $5)
                          )
                         )
                         (i64.const 0)
                        )
                       )
                       (local.set $24
                        (i32.const 0)
                       )
                       (br_if $label$93
                        (local.get $23)
                       )
                       (local.set $19
                        (local.get $15)
                       )
                       (local.set $18
                        (local.get $15)
                       )
                       (br $label$9)
                      )
                      (local.set $24
                       (select
                        (local.get $23)
                        (local.tee $18
                         (i32.add
                          (i32.sub
                           (local.get $15)
                           (local.get $19)
                          )
                          (i64.eqz
                           (local.get $31)
                          )
                         )
                        )
                        (i32.gt_s
                         (local.get $23)
                         (local.get $18)
                        )
                       )
                      )
                      (local.set $18
                       (local.get $15)
                      )
                      (br $label$9)
                     )
                     (i64.store8 offset=55
                      (local.get $5)
                      (i64.load offset=56
                       (local.get $5)
                      )
                     )
                     (local.set $27
                      (i32.const 0)
                     )
                     (local.set $30
                      (i32.const 1024)
                     )
                     (local.set $24
                      (i32.const 1)
                     )
                     (local.set $19
                      (local.get $9)
                     )
                     (local.set $18
                      (local.get $15)
                     )
                     (br $label$9)
                    )
                    (local.set $19
                     (call $strerror
                      (i32.load
                       (i32.const 3668)
                      )
                     )
                    )
                    (br $label$63)
                   )
                   (local.set $19
                    (select
                     (local.tee $18
                      (i32.load offset=56
                       (local.get $5)
                      )
                     )
                     (i32.const 1071)
                     (local.get $18)
                    )
                   )
                  )
                  (local.set $18
                   (i32.add
                    (local.get $19)
                    (local.tee $24
                     (call $strnlen
                      (local.get $19)
                      (select
                       (local.get $23)
                       (i32.const 2147483647)
                       (i32.lt_u
                        (local.get $23)
                        (i32.const 2147483647)
                       )
                      )
                     )
                    )
                   )
                  )
                  (local.set $27
                   (i32.const 0)
                  )
                  (local.set $30
                   (i32.const 1024)
                  )
                  (br_if $label$9
                   (i32.gt_s
                    (local.get $23)
                    (i32.const -1)
                   )
                  )
                  (br_if $label$9
                   (i32.eqz
                    (i32.load8_u
                     (local.get $18)
                    )
                   )
                  )
                  (br $label$3)
                 )
                 (local.set $19
                  (i32.load offset=56
                   (local.get $5)
                  )
                 )
                 (br_if $label$60
                  (local.get $23)
                 )
                 (local.set $18
                  (i32.const 0)
                 )
                 (br $label$59)
                )
                (i32.store offset=12
                 (local.get $5)
                 (i32.const 0)
                )
                (i64.store32 offset=8
                 (local.get $5)
                 (i64.load offset=56
                  (local.get $5)
                 )
                )
                (i32.store offset=56
                 (local.get $5)
                 (i32.add
                  (local.get $5)
                  (i32.const 8)
                 )
                )
                (local.set $19
                 (i32.add
                  (local.get $5)
                  (i32.const 8)
                 )
                )
                (local.set $23
                 (i32.const -1)
                )
               )
               (local.set $18
                (i32.const 0)
               )
               (local.set $20
                (local.get $19)
               )
               (block $label$94
                (loop $label$95
                 (br_if $label$94
                  (i32.eqz
                   (local.tee $21
                    (i32.load
                     (local.get $20)
                    )
                   )
                  )
                 )
                 (block $label$96
                  (br_if $label$96
                   (local.tee $24
                    (i32.lt_s
                     (local.tee $21
                      (call $wctomb
                       (i32.add
                        (local.get $5)
                        (i32.const 4)
                       )
                       (local.get $21)
                      )
                     )
                     (i32.const 0)
                    )
                   )
                  )
                  (br_if $label$96
                   (i32.gt_u
                    (local.get $21)
                    (i32.sub
                     (local.get $23)
                     (local.get $18)
                    )
                   )
                  )
                  (local.set $20
                   (i32.add
                    (local.get $20)
                    (i32.const 4)
                   )
                  )
                  (br_if $label$95
                   (i32.lt_u
                    (local.tee $18
                     (i32.add
                      (local.get $21)
                      (local.get $18)
                     )
                    )
                    (local.get $23)
                   )
                  )
                  (br $label$94)
                 )
                )
                (br_if $label$2
                 (local.get $24)
                )
               )
               (br_if $label$3
                (i32.lt_s
                 (local.get $18)
                 (i32.const 0)
                )
               )
              )
              (block $label$97
               (br_if $label$97
                (local.tee $24
                 (i32.and
                  (local.get $22)
                  (i32.const 73728)
                 )
                )
               )
               (br_if $label$97
                (i32.le_s
                 (local.get $25)
                 (local.get $18)
                )
               )
               (drop
                (call $memset
                 (i32.add
                  (local.get $5)
                  (i32.const 112)
                 )
                 (i32.const 32)
                 (select
                  (local.tee $20
                   (i32.sub
                    (local.get $25)
                    (local.get $18)
                   )
                  )
                  (i32.const 256)
                  (local.tee $21
                   (i32.lt_u
                    (local.get $20)
                    (i32.const 256)
                   )
                  )
                 )
                )
               )
               (block $label$98
                (br_if $label$98
                 (local.get $21)
                )
                (loop $label$99
                 (block $label$100
                  (br_if $label$100
                   (i32.and
                    (i32.load8_u
                     (local.get $0)
                    )
                    (i32.const 32)
                   )
                  )
                  (drop
                   (call $__fwritex
                    (i32.add
                     (local.get $5)
                     (i32.const 112)
                    )
                    (i32.const 256)
                    (local.get $0)
                   )
                  )
                 )
                 (br_if $label$99
                  (i32.gt_u
                   (local.tee $20
                    (i32.add
                     (local.get $20)
                     (i32.const -256)
                    )
                   )
                   (i32.const 255)
                  )
                 )
                )
               )
               (br_if $label$97
                (i32.and
                 (i32.load8_u
                  (local.get $0)
                 )
                 (i32.const 32)
                )
               )
               (drop
                (call $__fwritex
                 (i32.add
                  (local.get $5)
                  (i32.const 112)
                 )
                 (local.get $20)
                 (local.get $0)
                )
               )
              )
              (block $label$101
               (br_if $label$101
                (i32.eqz
                 (local.get $18)
                )
               )
               (local.set $20
                (i32.const 0)
               )
               (loop $label$102
                (br_if $label$101
                 (i32.eqz
                  (local.tee $21
                   (i32.load
                    (local.get $19)
                   )
                  )
                 )
                )
                (br_if $label$101
                 (i32.gt_u
                  (local.tee $20
                   (i32.add
                    (local.tee $21
                     (call $wctomb
                      (i32.add
                       (local.get $5)
                       (i32.const 4)
                      )
                      (local.get $21)
                     )
                    )
                    (local.get $20)
                   )
                  )
                  (local.get $18)
                 )
                )
                (block $label$103
                 (br_if $label$103
                  (i32.and
                   (i32.load8_u
                    (local.get $0)
                   )
                   (i32.const 32)
                  )
                 )
                 (drop
                  (call $__fwritex
                   (i32.add
                    (local.get $5)
                    (i32.const 4)
                   )
                   (local.get $21)
                   (local.get $0)
                  )
                 )
                )
                (local.set $19
                 (i32.add
                  (local.get $19)
                  (i32.const 4)
                 )
                )
                (br_if $label$102
                 (i32.lt_u
                  (local.get $20)
                  (local.get $18)
                 )
                )
               )
              )
              (block $label$104
               (br_if $label$104
                (i32.ne
                 (local.get $24)
                 (i32.const 8192)
                )
               )
               (br_if $label$104
                (i32.le_s
                 (local.get $25)
                 (local.get $18)
                )
               )
               (drop
                (call $memset
                 (i32.add
                  (local.get $5)
                  (i32.const 112)
                 )
                 (i32.const 32)
                 (select
                  (local.tee $20
                   (i32.sub
                    (local.get $25)
                    (local.get $18)
                   )
                  )
                  (i32.const 256)
                  (local.tee $21
                   (i32.lt_u
                    (local.get $20)
                    (i32.const 256)
                   )
                  )
                 )
                )
               )
               (block $label$105
                (br_if $label$105
                 (local.get $21)
                )
                (loop $label$106
                 (block $label$107
                  (br_if $label$107
                   (i32.and
                    (i32.load8_u
                     (local.get $0)
                    )
                    (i32.const 32)
                   )
                  )
                  (drop
                   (call $__fwritex
                    (i32.add
                     (local.get $5)
                     (i32.const 112)
                    )
                    (i32.const 256)
                    (local.get $0)
                   )
                  )
                 )
                 (br_if $label$106
                  (i32.gt_u
                   (local.tee $20
                    (i32.add
                     (local.get $20)
                     (i32.const -256)
                    )
                   )
                   (i32.const 255)
                  )
                 )
                )
               )
               (br_if $label$104
                (i32.and
                 (i32.load8_u
                  (local.get $0)
                 )
                 (i32.const 32)
                )
               )
               (drop
                (call $__fwritex
                 (i32.add
                  (local.get $5)
                  (i32.const 112)
                 )
                 (local.get $20)
                 (local.get $0)
                )
               )
              )
              (local.set $18
               (select
                (local.get $25)
                (local.get $18)
                (i32.gt_s
                 (local.get $25)
                 (local.get $18)
                )
               )
              )
              (br $label$4)
             )
             (block $label$108
              (br_if $label$108
               (i32.eqz
                (local.get $26)
               )
              )
              (br_if $label$3
               (i32.lt_s
                (local.get $23)
                (i32.const 0)
               )
              )
             )
             (local.set $33
              (f64.load offset=56
               (local.get $5)
              )
             )
             (i32.store offset=108
              (local.get $5)
              (i32.const 0)
             )
             (block $label$109
              (block $label$110
               (br_if $label$110
                (i64.gt_s
                 (i64.reinterpret_f64
                  (local.get $33)
                 )
                 (i64.const -1)
                )
               )
               (local.set $33
                (f64.neg
                 (local.get $33)
                )
               )
               (local.set $34
                (i32.const 1)
               )
               (local.set $35
                (i32.const 0)
               )
               (local.set $36
                (i32.const 1034)
               )
               (br $label$109)
              )
              (block $label$111
               (br_if $label$111
                (i32.eqz
                 (i32.and
                  (local.get $22)
                  (i32.const 2048)
                 )
                )
               )
               (local.set $34
                (i32.const 1)
               )
               (local.set $35
                (i32.const 0)
               )
               (local.set $36
                (i32.const 1037)
               )
               (br $label$109)
              )
              (local.set $36
               (select
                (i32.const 1040)
                (i32.const 1035)
                (local.tee $34
                 (i32.and
                  (local.get $22)
                  (i32.const 1)
                 )
                )
               )
              )
              (local.set $35
               (i32.eqz
                (local.get $34)
               )
              )
             )
             (block $label$112
              (br_if $label$112
               (i64.lt_s
                (i64.and
                 (i64.reinterpret_f64
                  (local.get $33)
                 )
                 (i64.const 9223372036854775807)
                )
                (i64.const 9218868437227405312)
               )
              )
              (local.set $20
               (i32.add
                (local.get $34)
                (i32.const 3)
               )
              )
              (block $label$113
               (br_if $label$113
                (i32.and
                 (local.get $22)
                 (i32.const 8192)
                )
               )
               (br_if $label$113
                (i32.le_s
                 (local.get $25)
                 (local.get $20)
                )
               )
               (drop
                (call $memset
                 (i32.add
                  (local.get $5)
                  (i32.const 624)
                 )
                 (i32.const 32)
                 (select
                  (local.tee $18
                   (i32.sub
                    (local.get $25)
                    (local.get $20)
                   )
                  )
                  (i32.const 256)
                  (local.tee $21
                   (i32.lt_u
                    (local.get $18)
                    (i32.const 256)
                   )
                  )
                 )
                )
               )
               (block $label$114
                (br_if $label$114
                 (local.get $21)
                )
                (loop $label$115
                 (block $label$116
                  (br_if $label$116
                   (i32.and
                    (i32.load8_u
                     (local.get $0)
                    )
                    (i32.const 32)
                   )
                  )
                  (drop
                   (call $__fwritex
                    (i32.add
                     (local.get $5)
                     (i32.const 624)
                    )
                    (i32.const 256)
                    (local.get $0)
                   )
                  )
                 )
                 (br_if $label$115
                  (i32.gt_u
                   (local.tee $18
                    (i32.add
                     (local.get $18)
                     (i32.const -256)
                    )
                   )
                   (i32.const 255)
                  )
                 )
                )
               )
               (br_if $label$113
                (i32.and
                 (i32.load8_u
                  (local.get $0)
                 )
                 (i32.const 32)
                )
               )
               (drop
                (call $__fwritex
                 (i32.add
                  (local.get $5)
                  (i32.const 624)
                 )
                 (local.get $18)
                 (local.get $0)
                )
               )
              )
              (block $label$117
               (br_if $label$117
                (i32.and
                 (local.tee $18
                  (i32.load
                   (local.get $0)
                  )
                 )
                 (i32.const 32)
                )
               )
               (drop
                (call $__fwritex
                 (local.get $36)
                 (local.get $34)
                 (local.get $0)
                )
               )
               (local.set $18
                (i32.load
                 (local.get $0)
                )
               )
              )
              (block $label$118
               (br_if $label$118
                (i32.and
                 (local.get $18)
                 (i32.const 32)
                )
               )
               (drop
                (call $__fwritex
                 (select
                  (select
                   (i32.const 1053)
                   (i32.const 1061)
                   (local.tee $18
                    (i32.and
                     (local.get $29)
                     (i32.const 32)
                    )
                   )
                  )
                  (select
                   (i32.const 1057)
                   (i32.const 1065)
                   (local.get $18)
                  )
                  (f64.ne
                   (local.get $33)
                   (local.get $33)
                  )
                 )
                 (i32.const 3)
                 (local.get $0)
                )
               )
              )
              (block $label$119
               (br_if $label$119
                (i32.ne
                 (i32.and
                  (local.get $22)
                  (i32.const 73728)
                 )
                 (i32.const 8192)
                )
               )
               (br_if $label$119
                (i32.le_s
                 (local.get $25)
                 (local.get $20)
                )
               )
               (drop
                (call $memset
                 (i32.add
                  (local.get $5)
                  (i32.const 624)
                 )
                 (i32.const 32)
                 (select
                  (local.tee $18
                   (i32.sub
                    (local.get $25)
                    (local.get $20)
                   )
                  )
                  (i32.const 256)
                  (local.tee $21
                   (i32.lt_u
                    (local.get $18)
                    (i32.const 256)
                   )
                  )
                 )
                )
               )
               (block $label$120
                (br_if $label$120
                 (local.get $21)
                )
                (loop $label$121
                 (block $label$122
                  (br_if $label$122
                   (i32.and
                    (i32.load8_u
                     (local.get $0)
                    )
                    (i32.const 32)
                   )
                  )
                  (drop
                   (call $__fwritex
                    (i32.add
                     (local.get $5)
                     (i32.const 624)
                    )
                    (i32.const 256)
                    (local.get $0)
                   )
                  )
                 )
                 (br_if $label$121
                  (i32.gt_u
                   (local.tee $18
                    (i32.add
                     (local.get $18)
                     (i32.const -256)
                    )
                   )
                   (i32.const 255)
                  )
                 )
                )
               )
               (br_if $label$119
                (i32.and
                 (i32.load8_u
                  (local.get $0)
                 )
                 (i32.const 32)
                )
               )
               (drop
                (call $__fwritex
                 (i32.add
                  (local.get $5)
                  (i32.const 624)
                 )
                 (local.get $18)
                 (local.get $0)
                )
               )
              )
              (local.set $18
               (select
                (local.get $20)
                (local.get $25)
                (i32.gt_s
                 (local.get $20)
                 (local.get $25)
                )
               )
              )
              (br $label$4)
             )
             (block $label$123
              (block $label$124
               (block $label$125
                (br_if $label$125
                 (f64.eq
                  (local.tee $33
                   (f64.add
                    (local.tee $33
                     (call $frexp
                      (local.get $33)
                      (i32.add
                       (local.get $5)
                       (i32.const 108)
                      )
                     )
                    )
                    (local.get $33)
                   )
                  )
                  (f64.const 0)
                 )
                )
                (i32.store offset=108
                 (local.get $5)
                 (i32.add
                  (local.tee $18
                   (i32.load offset=108
                    (local.get $5)
                   )
                  )
                  (i32.const -1)
                 )
                )
                (br_if $label$124
                 (i32.ne
                  (local.tee $37
                   (i32.or
                    (local.get $29)
                    (i32.const 32)
                   )
                  )
                  (i32.const 97)
                 )
                )
                (br $label$6)
               )
               (br_if $label$6
                (i32.eq
                 (local.tee $37
                  (i32.or
                   (local.get $29)
                   (i32.const 32)
                  )
                 )
                 (i32.const 97)
                )
               )
               (local.set $26
                (select
                 (i32.const 6)
                 (local.get $23)
                 (i32.lt_s
                  (local.get $23)
                  (i32.const 0)
                 )
                )
               )
               (local.set $19
                (i32.load offset=108
                 (local.get $5)
                )
               )
               (br $label$123)
              )
              (i32.store offset=108
               (local.get $5)
               (local.tee $19
                (i32.add
                 (local.get $18)
                 (i32.const -29)
                )
               )
              )
              (local.set $26
               (select
                (i32.const 6)
                (local.get $23)
                (i32.lt_s
                 (local.get $23)
                 (i32.const 0)
                )
               )
              )
              (local.set $33
               (f64.mul
                (local.get $33)
                (f64.const 268435456)
               )
              )
             )
             (local.set $20
              (local.tee $30
               (i32.add
                (i32.add
                 (local.get $5)
                 (i32.const 112)
                )
                (local.tee $39
                 (i32.shl
                  (select
                   (i32.const 0)
                   (i32.const 72)
                   (local.tee $38
                    (i32.lt_s
                     (local.get $19)
                     (i32.const 0)
                    )
                   )
                  )
                  (i32.const 2)
                 )
                )
               )
              )
             )
             (loop $label$126
              (block $label$127
               (block $label$128
                (br_if $label$128
                 (i32.eqz
                  (i32.and
                   (f64.lt
                    (local.get $33)
                    (f64.const 4294967296)
                   )
                   (f64.ge
                    (local.get $33)
                    (f64.const 0)
                   )
                  )
                 )
                )
                (local.set $18
                 (i32.trunc_f64_u
                  (local.get $33)
                 )
                )
                (br $label$127)
               )
               (local.set $18
                (i32.const 0)
               )
              )
              (i32.store
               (local.get $20)
               (local.get $18)
              )
              (local.set $20
               (i32.add
                (local.get $20)
                (i32.const 4)
               )
              )
              (br_if $label$126
               (f64.ne
                (local.tee $33
                 (f64.mul
                  (f64.sub
                   (local.get $33)
                   (f64.convert_i32_u
                    (local.get $18)
                   )
                  )
                  (f64.const 1e9)
                 )
                )
                (f64.const 0)
               )
              )
             )
             (block $label$129
              (block $label$130
               (br_if $label$130
                (i32.ge_s
                 (local.get $19)
                 (i32.const 1)
                )
               )
               (local.set $18
                (local.get $20)
               )
               (local.set $21
                (local.get $30)
               )
               (br $label$129)
              )
              (local.set $21
               (local.get $30)
              )
              (loop $label$131
               (local.set $19
                (select
                 (local.get $19)
                 (i32.const 29)
                 (i32.lt_s
                  (local.get $19)
                  (i32.const 29)
                 )
                )
               )
               (block $label$132
                (br_if $label$132
                 (i32.lt_u
                  (local.tee $18
                   (i32.add
                    (local.get $20)
                    (i32.const -4)
                   )
                  )
                  (local.get $21)
                 )
                )
                (local.set $32
                 (i64.extend_i32_u
                  (local.get $19)
                 )
                )
                (local.set $31
                 (i64.const 0)
                )
                (loop $label$133
                 (i64.store32
                  (local.get $18)
                  (i64.sub
                   (local.tee $31
                    (i64.add
                     (i64.shl
                      (i64.load32_u
                       (local.get $18)
                      )
                      (local.get $32)
                     )
                     (i64.and
                      (local.get $31)
                      (i64.const 4294967295)
                     )
                    )
                   )
                   (i64.mul
                    (local.tee $31
                     (i64.div_u
                      (local.get $31)
                      (i64.const 1000000000)
                     )
                    )
                    (i64.const 1000000000)
                   )
                  )
                 )
                 (br_if $label$133
                  (i32.ge_u
                   (local.tee $18
                    (i32.add
                     (local.get $18)
                     (i32.const -4)
                    )
                   )
                   (local.get $21)
                  )
                 )
                )
                (br_if $label$132
                 (i32.eqz
                  (local.tee $18
                   (i32.wrap_i64
                    (local.get $31)
                   )
                  )
                 )
                )
                (i32.store
                 (local.tee $21
                  (i32.add
                   (local.get $21)
                   (i32.const -4)
                  )
                 )
                 (local.get $18)
                )
               )
               (block $label$134
                (loop $label$135
                 (br_if $label$134
                  (i32.le_u
                   (local.tee $18
                    (local.get $20)
                   )
                   (local.get $21)
                  )
                 )
                 (br_if $label$135
                  (i32.eqz
                   (i32.load
                    (local.tee $20
                     (i32.add
                      (local.get $18)
                      (i32.const -4)
                     )
                    )
                   )
                  )
                 )
                )
               )
               (i32.store offset=108
                (local.get $5)
                (local.tee $19
                 (i32.sub
                  (i32.load offset=108
                   (local.get $5)
                  )
                  (local.get $19)
                 )
                )
               )
               (local.set $20
                (local.get $18)
               )
               (br_if $label$131
                (i32.gt_s
                 (local.get $19)
                 (i32.const 0)
                )
               )
              )
             )
             (block $label$136
              (br_if $label$136
               (i32.gt_s
                (local.get $19)
                (i32.const -1)
               )
              )
              (local.set $40
               (i32.add
                (i32.div_u
                 (i32.add
                  (local.get $26)
                  (i32.const 25)
                 )
                 (i32.const 9)
                )
                (i32.const 1)
               )
              )
              (loop $label$137
               (local.set $23
                (select
                 (local.tee $20
                  (i32.sub
                   (i32.const 0)
                   (local.get $19)
                  )
                 )
                 (i32.const 9)
                 (i32.lt_s
                  (local.get $20)
                  (i32.const 9)
                 )
                )
               )
               (block $label$138
                (block $label$139
                 (br_if $label$139
                  (i32.lt_u
                   (local.get $21)
                   (local.get $18)
                  )
                 )
                 (local.set $20
                  (i32.load
                   (local.get $21)
                  )
                 )
                 (br $label$138)
                )
                (local.set $28
                 (i32.shr_u
                  (i32.const 1000000000)
                  (local.get $23)
                 )
                )
                (local.set $27
                 (i32.xor
                  (i32.shl
                   (i32.const -1)
                   (local.get $23)
                  )
                  (i32.const -1)
                 )
                )
                (local.set $19
                 (i32.const 0)
                )
                (local.set $20
                 (local.get $21)
                )
                (loop $label$140
                 (i32.store
                  (local.get $20)
                  (i32.add
                   (i32.shr_u
                    (local.tee $24
                     (i32.load
                      (local.get $20)
                     )
                    )
                    (local.get $23)
                   )
                   (local.get $19)
                  )
                 )
                 (local.set $19
                  (i32.mul
                   (i32.and
                    (local.get $24)
                    (local.get $27)
                   )
                   (local.get $28)
                  )
                 )
                 (br_if $label$140
                  (i32.lt_u
                   (local.tee $20
                    (i32.add
                     (local.get $20)
                     (i32.const 4)
                    )
                   )
                   (local.get $18)
                  )
                 )
                )
                (local.set $20
                 (i32.load
                  (local.get $21)
                 )
                )
                (br_if $label$138
                 (i32.eqz
                  (local.get $19)
                 )
                )
                (i32.store
                 (local.get $18)
                 (local.get $19)
                )
                (local.set $18
                 (i32.add
                  (local.get $18)
                  (i32.const 4)
                 )
                )
               )
               (i32.store offset=108
                (local.get $5)
                (local.tee $19
                 (i32.add
                  (i32.load offset=108
                   (local.get $5)
                  )
                  (local.get $23)
                 )
                )
               )
               (local.set $18
                (select
                 (i32.add
                  (local.tee $20
                   (select
                    (local.get $30)
                    (local.tee $21
                     (i32.add
                      (local.get $21)
                      (i32.shl
                       (i32.eqz
                        (local.get $20)
                       )
                       (i32.const 2)
                      )
                     )
                    )
                    (i32.eq
                     (local.get $37)
                     (i32.const 102)
                    )
                   )
                  )
                  (i32.shl
                   (local.get $40)
                   (i32.const 2)
                  )
                 )
                 (local.get $18)
                 (i32.gt_s
                  (i32.shr_s
                   (i32.sub
                    (local.get $18)
                    (local.get $20)
                   )
                   (i32.const 2)
                  )
                  (local.get $40)
                 )
                )
               )
               (br_if $label$137
                (i32.lt_s
                 (local.get $19)
                 (i32.const 0)
                )
               )
              )
             )
             (local.set $24
              (i32.const 0)
             )
             (block $label$141
              (br_if $label$141
               (i32.ge_u
                (local.get $21)
                (local.get $18)
               )
              )
              (local.set $24
               (i32.mul
                (i32.shr_s
                 (i32.sub
                  (local.get $30)
                  (local.get $21)
                 )
                 (i32.const 2)
                )
                (i32.const 9)
               )
              )
              (br_if $label$141
               (i32.lt_u
                (local.tee $19
                 (i32.load
                  (local.get $21)
                 )
                )
                (i32.const 10)
               )
              )
              (local.set $20
               (i32.const 10)
              )
              (loop $label$142
               (local.set $24
                (i32.add
                 (local.get $24)
                 (i32.const 1)
                )
               )
               (br_if $label$142
                (i32.ge_u
                 (local.get $19)
                 (local.tee $20
                  (i32.mul
                   (local.get $20)
                   (i32.const 10)
                  )
                 )
                )
               )
              )
             )
             (block $label$143
              (br_if $label$143
               (i32.ge_s
                (local.tee $20
                 (i32.sub
                  (i32.sub
                   (local.get $26)
                   (select
                    (i32.const 0)
                    (local.get $24)
                    (i32.eq
                     (local.get $37)
                     (i32.const 102)
                    )
                   )
                  )
                  (i32.and
                   (i32.ne
                    (local.get $26)
                    (i32.const 0)
                   )
                   (local.tee $27
                    (i32.eq
                     (local.get $37)
                     (i32.const 103)
                    )
                   )
                  )
                 )
                )
                (i32.add
                 (i32.mul
                  (i32.shr_s
                   (i32.sub
                    (local.get $18)
                    (local.get $30)
                   )
                   (i32.const 2)
                  )
                  (i32.const 9)
                 )
                 (i32.const -9)
                )
               )
              )
              (local.set $28
               (i32.add
                (i32.add
                 (local.tee $41
                  (i32.shl
                   (local.tee $23
                    (i32.div_s
                     (local.tee $19
                      (i32.add
                       (local.get $20)
                       (i32.const 9216)
                      )
                     )
                     (i32.const 9)
                    )
                   )
                   (i32.const 2)
                  )
                 )
                 (i32.add
                  (i32.add
                   (local.get $5)
                   (i32.const 112)
                  )
                  (local.tee $38
                   (i32.shl
                    (select
                     (i32.const 1)
                     (i32.const 73)
                     (local.get $38)
                    )
                    (i32.const 2)
                   )
                  )
                 )
                )
                (i32.const -4096)
               )
              )
              (local.set $20
               (i32.const 10)
              )
              (block $label$144
               (br_if $label$144
                (i32.gt_s
                 (local.tee $23
                  (i32.sub
                   (local.get $19)
                   (i32.mul
                    (local.get $23)
                    (i32.const 9)
                   )
                  )
                 )
                 (i32.const 7)
                )
               )
               (local.set $19
                (i32.and
                 (local.tee $40
                  (i32.sub
                   (i32.const 8)
                   (local.get $23)
                  )
                 )
                 (i32.const 7)
                )
               )
               (local.set $20
                (i32.const 10)
               )
               (block $label$145
                (br_if $label$145
                 (i32.lt_u
                  (i32.add
                   (local.get $23)
                   (i32.const -1)
                  )
                  (i32.const 7)
                 )
                )
                (local.set $23
                 (i32.and
                  (local.get $40)
                  (i32.const -8)
                 )
                )
                (local.set $20
                 (i32.const 10)
                )
                (loop $label$146
                 (local.set $20
                  (i32.mul
                   (local.get $20)
                   (i32.const 100000000)
                  )
                 )
                 (br_if $label$146
                  (local.tee $23
                   (i32.add
                    (local.get $23)
                    (i32.const -8)
                   )
                  )
                 )
                )
               )
               (br_if $label$144
                (i32.eqz
                 (local.get $19)
                )
               )
               (loop $label$147
                (local.set $20
                 (i32.mul
                  (local.get $20)
                  (i32.const 10)
                 )
                )
                (br_if $label$147
                 (local.tee $19
                  (i32.add
                   (local.get $19)
                   (i32.const -1)
                  )
                 )
                )
               )
              )
              (local.set $40
               (i32.add
                (local.get $28)
                (i32.const 4)
               )
              )
              (block $label$148
               (block $label$149
                (br_if $label$149
                 (local.tee $23
                  (i32.sub
                   (local.tee $19
                    (i32.load
                     (local.get $28)
                    )
                   )
                   (i32.mul
                    (local.tee $37
                     (i32.div_u
                      (local.get $19)
                      (local.get $20)
                     )
                    )
                    (local.get $20)
                   )
                  )
                 )
                )
                (br_if $label$148
                 (i32.eq
                  (local.get $40)
                  (local.get $18)
                 )
                )
               )
               (block $label$150
                (block $label$151
                 (br_if $label$151
                  (i32.and
                   (local.get $37)
                   (i32.const 1)
                  )
                 )
                 (local.set $33
                  (f64.const 9007199254740992)
                 )
                 (br_if $label$150
                  (i32.ne
                   (local.get $20)
                   (i32.const 1000000000)
                  )
                 )
                 (br_if $label$150
                  (i32.le_u
                   (local.get $28)
                   (local.get $21)
                  )
                 )
                 (br_if $label$150
                  (i32.eqz
                   (i32.and
                    (i32.load8_u
                     (i32.add
                      (local.get $28)
                      (i32.const -4)
                     )
                    )
                    (i32.const 1)
                   )
                  )
                 )
                )
                (local.set $33
                 (f64.const 9007199254740994)
                )
               )
               (local.set $42
                (select
                 (f64.const 0.5)
                 (select
                  (select
                   (f64.const 1)
                   (f64.const 1.5)
                   (i32.eq
                    (local.get $40)
                    (local.get $18)
                   )
                  )
                  (f64.const 1.5)
                  (i32.eq
                   (local.get $23)
                   (local.tee $40
                    (i32.shr_u
                     (local.get $20)
                     (i32.const 1)
                    )
                   )
                  )
                 )
                 (i32.lt_u
                  (local.get $23)
                  (local.get $40)
                 )
                )
               )
               (block $label$152
                (br_if $label$152
                 (local.get $35)
                )
                (br_if $label$152
                 (i32.ne
                  (i32.load8_u
                   (local.get $36)
                  )
                  (i32.const 45)
                 )
                )
                (local.set $42
                 (f64.neg
                  (local.get $42)
                 )
                )
                (local.set $33
                 (f64.neg
                  (local.get $33)
                 )
                )
               )
               (i32.store
                (local.get $28)
                (local.tee $19
                 (i32.sub
                  (local.get $19)
                  (local.get $23)
                 )
                )
               )
               (br_if $label$148
                (f64.eq
                 (f64.add
                  (local.get $33)
                  (local.get $42)
                 )
                 (local.get $33)
                )
               )
               (i32.store
                (local.get $28)
                (local.tee $20
                 (i32.add
                  (local.get $19)
                  (local.get $20)
                 )
                )
               )
               (block $label$153
                (br_if $label$153
                 (i32.lt_u
                  (local.get $20)
                  (i32.const 1000000000)
                 )
                )
                (local.set $20
                 (i32.add
                  (local.get $8)
                  (i32.add
                   (local.get $38)
                   (local.get $41)
                  )
                 )
                )
                (loop $label$154
                 (i32.store
                  (i32.add
                   (local.get $20)
                   (i32.const 4)
                  )
                  (i32.const 0)
                 )
                 (block $label$155
                  (br_if $label$155
                   (i32.ge_u
                    (local.get $20)
                    (local.get $21)
                   )
                  )
                  (i32.store
                   (local.tee $21
                    (i32.add
                     (local.get $21)
                     (i32.const -4)
                    )
                   )
                   (i32.const 0)
                  )
                 )
                 (i32.store
                  (local.get $20)
                  (local.tee $19
                   (i32.add
                    (i32.load
                     (local.get $20)
                    )
                    (i32.const 1)
                   )
                  )
                 )
                 (local.set $20
                  (i32.add
                   (local.get $20)
                   (i32.const -4)
                  )
                 )
                 (br_if $label$154
                  (i32.gt_u
                   (local.get $19)
                   (i32.const 999999999)
                  )
                 )
                )
                (local.set $28
                 (i32.add
                  (local.get $20)
                  (i32.const 4)
                 )
                )
               )
               (local.set $24
                (i32.mul
                 (i32.shr_s
                  (i32.sub
                   (local.get $30)
                   (local.get $21)
                  )
                  (i32.const 2)
                 )
                 (i32.const 9)
                )
               )
               (br_if $label$148
                (i32.lt_u
                 (local.tee $19
                  (i32.load
                   (local.get $21)
                  )
                 )
                 (i32.const 10)
                )
               )
               (local.set $20
                (i32.const 10)
               )
               (loop $label$156
                (local.set $24
                 (i32.add
                  (local.get $24)
                  (i32.const 1)
                 )
                )
                (br_if $label$156
                 (i32.ge_u
                  (local.get $19)
                  (local.tee $20
                   (i32.mul
                    (local.get $20)
                    (i32.const 10)
                   )
                  )
                 )
                )
               )
              )
              (local.set $18
               (select
                (local.tee $20
                 (i32.add
                  (local.get $28)
                  (i32.const 4)
                 )
                )
                (local.get $18)
                (i32.gt_u
                 (local.get $18)
                 (local.get $20)
                )
               )
              )
             )
             (local.set $20
              (i32.sub
               (i32.add
                (local.get $7)
                (local.get $18)
               )
               (local.get $39)
              )
             )
             (block $label$157
              (loop $label$158
               (local.set $19
                (local.get $20)
               )
               (br_if $label$157
                (local.tee $23
                 (i32.le_u
                  (local.tee $28
                   (local.get $18)
                  )
                  (local.get $21)
                 )
                )
               )
               (local.set $20
                (i32.add
                 (local.get $19)
                 (i32.const -4)
                )
               )
               (br_if $label$158
                (i32.eqz
                 (i32.load
                  (local.tee $18
                   (i32.add
                    (local.get $28)
                    (i32.const -4)
                   )
                  )
                 )
                )
               )
              )
             )
             (block $label$159
              (block $label$160
               (br_if $label$160
                (local.get $27)
               )
               (local.set $40
                (i32.and
                 (local.get $22)
                 (i32.const 8)
                )
               )
               (br $label$159)
              )
              (local.set $26
               (i32.add
                (select
                 (i32.xor
                  (local.get $24)
                  (i32.const -1)
                 )
                 (i32.const -1)
                 (local.tee $20
                  (i32.and
                   (i32.gt_s
                    (local.tee $18
                     (select
                      (local.get $26)
                      (i32.const 1)
                      (local.get $26)
                     )
                    )
                    (local.get $24)
                   )
                   (i32.gt_s
                    (local.get $24)
                    (i32.const -5)
                   )
                  )
                 )
                )
                (local.get $18)
               )
              )
              (local.set $29
               (i32.add
                (select
                 (i32.const -1)
                 (i32.const -2)
                 (local.get $20)
                )
                (local.get $29)
               )
              )
              (br_if $label$159
               (local.tee $40
                (i32.and
                 (local.get $22)
                 (i32.const 8)
                )
               )
              )
              (local.set $18
               (i32.const -9)
              )
              (block $label$161
               (br_if $label$161
                (local.get $23)
               )
               (br_if $label$161
                (i32.eqz
                 (local.tee $23
                  (i32.load
                   (i32.add
                    (local.get $28)
                    (i32.const -4)
                   )
                  )
                 )
                )
               )
               (local.set $18
                (i32.const 0)
               )
               (br_if $label$161
                (i32.rem_u
                 (local.get $23)
                 (i32.const 10)
                )
               )
               (local.set $20
                (i32.const 10)
               )
               (local.set $18
                (i32.const 0)
               )
               (loop $label$162
                (local.set $18
                 (i32.add
                  (local.get $18)
                  (i32.const -1)
                 )
                )
                (br_if $label$162
                 (i32.eqz
                  (i32.rem_u
                   (local.get $23)
                   (local.tee $20
                    (i32.mul
                     (local.get $20)
                     (i32.const 10)
                    )
                   )
                  )
                 )
                )
               )
              )
              (local.set $20
               (i32.mul
                (i32.shr_s
                 (local.get $19)
                 (i32.const 2)
                )
                (i32.const 9)
               )
              )
              (block $label$163
               (br_if $label$163
                (i32.ne
                 (i32.and
                  (local.get $29)
                  (i32.const -33)
                 )
                 (i32.const 70)
                )
               )
               (local.set $40
                (i32.const 0)
               )
               (local.set $26
                (select
                 (local.get $26)
                 (local.tee $18
                  (select
                   (local.tee $18
                    (i32.add
                     (i32.add
                      (local.get $20)
                      (local.get $18)
                     )
                     (i32.const -9)
                    )
                   )
                   (i32.const 0)
                   (i32.gt_s
                    (local.get $18)
                    (i32.const 0)
                   )
                  )
                 )
                 (i32.lt_s
                  (local.get $26)
                  (local.get $18)
                 )
                )
               )
               (br $label$159)
              )
              (local.set $40
               (i32.const 0)
              )
              (local.set $26
               (select
                (local.get $26)
                (local.tee $18
                 (select
                  (local.tee $18
                   (i32.add
                    (i32.add
                     (i32.add
                      (local.get $24)
                      (local.get $20)
                     )
                     (local.get $18)
                    )
                    (i32.const -9)
                   )
                  )
                  (i32.const 0)
                  (i32.gt_s
                   (local.get $18)
                   (i32.const 0)
                  )
                 )
                )
                (i32.lt_s
                 (local.get $26)
                 (local.get $18)
                )
               )
              )
             )
             (br_if $label$3
              (i32.gt_s
               (local.get $26)
               (select
                (i32.const 2147483645)
                (i32.const 2147483646)
                (local.tee $35
                 (i32.or
                  (local.get $26)
                  (local.get $40)
                 )
                )
               )
              )
             )
             (local.set $37
              (i32.add
               (i32.add
                (local.get $26)
                (i32.ne
                 (local.get $35)
                 (i32.const 0)
                )
               )
               (i32.const 1)
              )
             )
             (block $label$164
              (block $label$165
               (br_if $label$165
                (local.tee $38
                 (i32.ne
                  (i32.and
                   (local.get $29)
                   (i32.const -33)
                  )
                  (i32.const 70)
                 )
                )
               )
               (br_if $label$3
                (i32.gt_s
                 (local.get $24)
                 (i32.xor
                  (local.get $37)
                  (i32.const 2147483647)
                 )
                )
               )
               (local.set $18
                (select
                 (local.get $24)
                 (i32.const 0)
                 (i32.gt_s
                  (local.get $24)
                  (i32.const 0)
                 )
                )
               )
               (br $label$164)
              )
              (block $label$166
               (block $label$167
                (br_if $label$167
                 (local.get $24)
                )
                (local.set $19
                 (local.get $6)
                )
                (local.set $20
                 (local.get $6)
                )
                (br $label$166)
               )
               (local.set $18
                (i32.sub
                 (i32.xor
                  (local.get $24)
                  (local.tee $18
                   (i32.shr_s
                    (local.get $24)
                    (i32.const 31)
                   )
                  )
                 )
                 (local.get $18)
                )
               )
               (local.set $19
                (local.get $6)
               )
               (local.set $20
                (local.get $6)
               )
               (loop $label$168
                (i32.store8
                 (local.tee $20
                  (i32.add
                   (local.get $20)
                   (i32.const -1)
                  )
                 )
                 (i32.or
                  (i32.sub
                   (local.get $18)
                   (i32.mul
                    (local.tee $23
                     (i32.div_u
                      (local.get $18)
                      (i32.const 10)
                     )
                    )
                    (i32.const 10)
                   )
                  )
                  (i32.const 48)
                 )
                )
                (local.set $19
                 (i32.add
                  (local.get $19)
                  (i32.const -1)
                 )
                )
                (local.set $27
                 (i32.gt_u
                  (local.get $18)
                  (i32.const 9)
                 )
                )
                (local.set $18
                 (local.get $23)
                )
                (br_if $label$168
                 (local.get $27)
                )
               )
              )
              (block $label$169
               (br_if $label$169
                (i32.gt_s
                 (i32.sub
                  (local.get $6)
                  (local.get $19)
                 )
                 (i32.const 1)
                )
               )
               (drop
                (call $memset
                 (local.tee $20
                  (i32.add
                   (local.get $20)
                   (i32.sub
                    (local.get $14)
                    (local.get $19)
                   )
                  )
                 )
                 (i32.const 48)
                 (i32.add
                  (i32.sub
                   (local.get $19)
                   (i32.add
                    (local.get $5)
                    (i32.const 68)
                   )
                  )
                  (i32.const -10)
                 )
                )
               )
              )
              (i32.store8
               (local.tee $39
                (i32.add
                 (local.get $20)
                 (i32.const -2)
                )
               )
               (local.get $29)
              )
              (i32.store8
               (i32.add
                (local.get $20)
                (i32.const -1)
               )
               (select
                (i32.const 45)
                (i32.const 43)
                (i32.lt_s
                 (local.get $24)
                 (i32.const 0)
                )
               )
              )
              (br_if $label$3
               (i32.gt_s
                (local.tee $18
                 (i32.sub
                  (local.get $6)
                  (local.get $39)
                 )
                )
                (i32.xor
                 (local.get $37)
                 (i32.const 2147483647)
                )
               )
              )
             )
             (br_if $label$3
              (i32.gt_s
               (local.tee $18
                (i32.add
                 (local.get $18)
                 (local.get $37)
                )
               )
               (i32.xor
                (local.get $34)
                (i32.const 2147483647)
               )
              )
             )
             (local.set $27
              (i32.add
               (local.get $18)
               (local.get $34)
              )
             )
             (block $label$170
              (br_if $label$170
               (local.tee $22
                (i32.and
                 (local.get $22)
                 (i32.const 73728)
                )
               )
              )
              (br_if $label$170
               (i32.le_s
                (local.get $25)
                (local.get $27)
               )
              )
              (drop
               (call $memset
                (i32.add
                 (local.get $5)
                 (i32.const 624)
                )
                (i32.const 32)
                (select
                 (local.tee $18
                  (i32.sub
                   (local.get $25)
                   (local.get $27)
                  )
                 )
                 (i32.const 256)
                 (local.tee $20
                  (i32.lt_u
                   (local.get $18)
                   (i32.const 256)
                  )
                 )
                )
               )
              )
              (block $label$171
               (br_if $label$171
                (local.get $20)
               )
               (loop $label$172
                (block $label$173
                 (br_if $label$173
                  (i32.and
                   (i32.load8_u
                    (local.get $0)
                   )
                   (i32.const 32)
                  )
                 )
                 (drop
                  (call $__fwritex
                   (i32.add
                    (local.get $5)
                    (i32.const 624)
                   )
                   (i32.const 256)
                   (local.get $0)
                  )
                 )
                )
                (br_if $label$172
                 (i32.gt_u
                  (local.tee $18
                   (i32.add
                    (local.get $18)
                    (i32.const -256)
                   )
                  )
                  (i32.const 255)
                 )
                )
               )
              )
              (br_if $label$170
               (i32.and
                (i32.load8_u
                 (local.get $0)
                )
                (i32.const 32)
               )
              )
              (drop
               (call $__fwritex
                (i32.add
                 (local.get $5)
                 (i32.const 624)
                )
                (local.get $18)
                (local.get $0)
               )
              )
             )
             (block $label$174
              (br_if $label$174
               (i32.and
                (i32.load8_u
                 (local.get $0)
                )
                (i32.const 32)
               )
              )
              (drop
               (call $__fwritex
                (local.get $36)
                (local.get $34)
                (local.get $0)
               )
              )
             )
             (block $label$175
              (br_if $label$175
               (i32.ne
                (local.get $22)
                (i32.const 65536)
               )
              )
              (br_if $label$175
               (i32.le_s
                (local.get $25)
                (local.get $27)
               )
              )
              (drop
               (call $memset
                (i32.add
                 (local.get $5)
                 (i32.const 624)
                )
                (i32.const 48)
                (select
                 (local.tee $18
                  (i32.sub
                   (local.get $25)
                   (local.get $27)
                  )
                 )
                 (i32.const 256)
                 (local.tee $20
                  (i32.lt_u
                   (local.get $18)
                   (i32.const 256)
                  )
                 )
                )
               )
              )
              (block $label$176
               (br_if $label$176
                (local.get $20)
               )
               (loop $label$177
                (block $label$178
                 (br_if $label$178
                  (i32.and
                   (i32.load8_u
                    (local.get $0)
                   )
                   (i32.const 32)
                  )
                 )
                 (drop
                  (call $__fwritex
                   (i32.add
                    (local.get $5)
                    (i32.const 624)
                   )
                   (i32.const 256)
                   (local.get $0)
                  )
                 )
                )
                (br_if $label$177
                 (i32.gt_u
                  (local.tee $18
                   (i32.add
                    (local.get $18)
                    (i32.const -256)
                   )
                  )
                  (i32.const 255)
                 )
                )
               )
              )
              (br_if $label$175
               (i32.and
                (i32.load8_u
                 (local.get $0)
                )
                (i32.const 32)
               )
              )
              (drop
               (call $__fwritex
                (i32.add
                 (local.get $5)
                 (i32.const 624)
                )
                (local.get $18)
                (local.get $0)
               )
              )
             )
             (br_if $label$8
              (local.get $38)
             )
             (local.set $23
              (local.tee $24
               (select
                (local.get $30)
                (local.get $21)
                (i32.gt_u
                 (local.get $21)
                 (local.get $30)
                )
               )
              )
             )
             (loop $label$179
              (block $label$180
               (block $label$181
                (block $label$182
                 (block $label$183
                  (br_if $label$183
                   (i32.eqz
                    (local.tee $18
                     (i32.load
                      (local.get $23)
                     )
                    )
                   )
                  )
                  (local.set $20
                   (i32.const 8)
                  )
                  (loop $label$184
                   (i32.store8
                    (i32.add
                     (i32.add
                      (local.get $5)
                      (i32.const 80)
                     )
                     (local.get $20)
                    )
                    (i32.or
                     (i32.sub
                      (local.get $18)
                      (i32.mul
                       (local.tee $21
                        (i32.div_u
                         (local.get $18)
                         (i32.const 10)
                        )
                       )
                       (i32.const 10)
                      )
                     )
                     (i32.const 48)
                    )
                   )
                   (local.set $20
                    (i32.add
                     (local.get $20)
                     (i32.const -1)
                    )
                   )
                   (local.set $19
                    (i32.gt_u
                     (local.get $18)
                     (i32.const 9)
                    )
                   )
                   (local.set $18
                    (local.get $21)
                   )
                   (br_if $label$184
                    (local.get $19)
                   )
                  )
                  (local.set $18
                   (i32.add
                    (local.tee $21
                     (i32.add
                      (local.get $20)
                      (i32.const 1)
                     )
                    )
                    (i32.add
                     (local.get $5)
                     (i32.const 80)
                    )
                   )
                  )
                  (block $label$185
                   (br_if $label$185
                    (i32.eq
                     (local.get $23)
                     (local.get $24)
                    )
                   )
                   (br_if $label$180
                    (i32.lt_s
                     (i32.add
                      (local.get $20)
                      (i32.const 2)
                     )
                     (i32.const 2)
                    )
                   )
                   (br $label$181)
                  )
                  (br_if $label$180
                   (i32.ne
                    (local.get $20)
                    (i32.const 8)
                   )
                  )
                  (br $label$182)
                 )
                 (local.set $21
                  (i32.const 9)
                 )
                 (br_if $label$181
                  (i32.ne
                   (local.get $23)
                   (local.get $24)
                  )
                 )
                )
                (i32.store8 offset=88
                 (local.get $5)
                 (i32.const 48)
                )
                (local.set $18
                 (local.get $12)
                )
                (br $label$180)
               )
               (drop
                (call $memset
                 (local.tee $18
                  (select
                   (i32.add
                    (local.get $5)
                    (i32.const 80)
                   )
                   (local.tee $18
                    (i32.add
                     (local.get $11)
                     (local.get $21)
                    )
                   )
                   (i32.lt_u
                    (i32.add
                     (local.get $5)
                     (i32.const 80)
                    )
                    (local.get $18)
                   )
                  )
                 )
                 (i32.const 48)
                 (i32.sub
                  (i32.add
                   (local.get $21)
                   (i32.add
                    (local.get $5)
                    (i32.const 80)
                   )
                  )
                  (local.get $18)
                 )
                )
               )
              )
              (block $label$186
               (br_if $label$186
                (i32.and
                 (i32.load8_u
                  (local.get $0)
                 )
                 (i32.const 32)
                )
               )
               (drop
                (call $__fwritex
                 (local.get $18)
                 (i32.sub
                  (local.get $13)
                  (local.get $18)
                 )
                 (local.get $0)
                )
               )
              )
              (br_if $label$179
               (i32.le_u
                (local.tee $23
                 (i32.add
                  (local.get $23)
                  (i32.const 4)
                 )
                )
                (local.get $30)
               )
              )
             )
             (block $label$187
              (br_if $label$187
               (i32.eqz
                (local.get $35)
               )
              )
              (br_if $label$187
               (i32.and
                (i32.load8_u
                 (local.get $0)
                )
                (i32.const 32)
               )
              )
              (drop
               (call $__fwritex
                (i32.const 1069)
                (i32.const 1)
                (local.get $0)
               )
              )
             )
             (block $label$188
              (block $label$189
               (br_if $label$189
                (i32.ge_s
                 (local.get $26)
                 (i32.const 1)
                )
               )
               (local.set $18
                (local.get $26)
               )
               (br $label$188)
              )
              (block $label$190
               (br_if $label$190
                (i32.lt_u
                 (local.get $23)
                 (local.get $28)
                )
               )
               (local.set $18
                (local.get $26)
               )
               (br $label$188)
              )
              (loop $label$191
               (block $label$192
                (block $label$193
                 (block $label$194
                  (br_if $label$194
                   (local.tee $18
                    (i32.load
                     (local.get $23)
                    )
                   )
                  )
                  (local.set $20
                   (local.get $13)
                  )
                  (local.set $21
                   (local.get $13)
                  )
                  (br $label$193)
                 )
                 (local.set $21
                  (local.get $13)
                 )
                 (local.set $20
                  (local.get $13)
                 )
                 (loop $label$195
                  (i32.store8
                   (local.tee $20
                    (i32.add
                     (local.get $20)
                     (i32.const -1)
                    )
                   )
                   (i32.or
                    (i32.sub
                     (local.get $18)
                     (i32.mul
                      (local.tee $19
                       (i32.div_u
                        (local.get $18)
                        (i32.const 10)
                       )
                      )
                      (i32.const 10)
                     )
                    )
                    (i32.const 48)
                   )
                  )
                  (local.set $21
                   (i32.add
                    (local.get $21)
                    (i32.const -1)
                   )
                  )
                  (local.set $24
                   (i32.gt_u
                    (local.get $18)
                    (i32.const 9)
                   )
                  )
                  (local.set $18
                   (local.get $19)
                  )
                  (br_if $label$195
                   (local.get $24)
                  )
                 )
                 (br_if $label$192
                  (i32.le_u
                   (local.get $20)
                   (i32.add
                    (local.get $5)
                    (i32.const 80)
                   )
                  )
                 )
                )
                (drop
                 (call $memset
                  (local.tee $20
                   (i32.sub
                    (i32.add
                     (local.get $20)
                     (i32.add
                      (local.get $5)
                      (i32.const 80)
                     )
                    )
                    (local.get $21)
                   )
                  )
                  (i32.const 48)
                  (i32.sub
                   (local.get $21)
                   (i32.add
                    (local.get $5)
                    (i32.const 80)
                   )
                  )
                 )
                )
               )
               (block $label$196
                (br_if $label$196
                 (i32.and
                  (i32.load8_u
                   (local.get $0)
                  )
                  (i32.const 32)
                 )
                )
                (drop
                 (call $__fwritex
                  (local.get $20)
                  (select
                   (local.get $26)
                   (i32.const 9)
                   (i32.lt_s
                    (local.get $26)
                    (i32.const 9)
                   )
                  )
                  (local.get $0)
                 )
                )
               )
               (local.set $18
                (i32.add
                 (local.get $26)
                 (i32.const -9)
                )
               )
               (br_if $label$188
                (i32.ge_u
                 (local.tee $23
                  (i32.add
                   (local.get $23)
                   (i32.const 4)
                  )
                 )
                 (local.get $28)
                )
               )
               (local.set $20
                (i32.gt_s
                 (local.get $26)
                 (i32.const 9)
                )
               )
               (local.set $26
                (local.get $18)
               )
               (br_if $label$191
                (local.get $20)
               )
              )
             )
             (call $pad
              (local.get $0)
              (i32.const 48)
              (i32.add
               (local.get $18)
               (i32.const 9)
              )
              (i32.const 9)
              (i32.const 0)
             )
             (br $label$7)
            )
            (i32.store
             (i32.const 3668)
             (i32.const 28)
            )
            (br $label$2)
           )
           (local.set $27
            (i32.const 0)
           )
           (local.set $30
            (i32.const 1024)
           )
           (local.set $18
            (local.get $15)
           )
           (local.set $28
            (local.get $22)
           )
           (local.set $24
            (local.get $23)
           )
          )
          (br_if $label$3
           (i32.gt_s
            (local.tee $26
             (select
              (local.get $24)
              (local.tee $23
               (i32.sub
                (local.get $18)
                (local.get $19)
               )
              )
              (i32.gt_s
               (local.get $24)
               (local.get $23)
              )
             )
            )
            (i32.xor
             (local.get $27)
             (i32.const 2147483647)
            )
           )
          )
          (br_if $label$3
           (i32.gt_s
            (local.tee $18
             (select
              (local.get $25)
              (local.tee $21
               (i32.add
                (local.get $27)
                (local.get $26)
               )
              )
              (i32.gt_s
               (local.get $25)
               (local.get $21)
              )
             )
            )
            (local.get $20)
           )
          )
          (block $label$197
           (br_if $label$197
            (local.tee $28
             (i32.and
              (local.get $28)
              (i32.const 73728)
             )
            )
           )
           (br_if $label$197
            (i32.ge_s
             (local.get $21)
             (local.get $25)
            )
           )
           (drop
            (call $memset
             (i32.add
              (local.get $5)
              (i32.const 112)
             )
             (i32.const 32)
             (select
              (local.tee $20
               (i32.sub
                (local.get $18)
                (local.get $21)
               )
              )
              (i32.const 256)
              (local.tee $22
               (i32.lt_u
                (local.get $20)
                (i32.const 256)
               )
              )
             )
            )
           )
           (block $label$198
            (br_if $label$198
             (local.get $22)
            )
            (loop $label$199
             (block $label$200
              (br_if $label$200
               (i32.and
                (i32.load8_u
                 (local.get $0)
                )
                (i32.const 32)
               )
              )
              (drop
               (call $__fwritex
                (i32.add
                 (local.get $5)
                 (i32.const 112)
                )
                (i32.const 256)
                (local.get $0)
               )
              )
             )
             (br_if $label$199
              (i32.gt_u
               (local.tee $20
                (i32.add
                 (local.get $20)
                 (i32.const -256)
                )
               )
               (i32.const 255)
              )
             )
            )
           )
           (br_if $label$197
            (i32.and
             (i32.load8_u
              (local.get $0)
             )
             (i32.const 32)
            )
           )
           (drop
            (call $__fwritex
             (i32.add
              (local.get $5)
              (i32.const 112)
             )
             (local.get $20)
             (local.get $0)
            )
           )
          )
          (block $label$201
           (br_if $label$201
            (i32.and
             (i32.load8_u
              (local.get $0)
             )
             (i32.const 32)
            )
           )
           (drop
            (call $__fwritex
             (local.get $30)
             (local.get $27)
             (local.get $0)
            )
           )
          )
          (block $label$202
           (br_if $label$202
            (i32.ne
             (local.get $28)
             (i32.const 65536)
            )
           )
           (br_if $label$202
            (i32.ge_s
             (local.get $21)
             (local.get $25)
            )
           )
           (drop
            (call $memset
             (i32.add
              (local.get $5)
              (i32.const 112)
             )
             (i32.const 48)
             (select
              (local.tee $20
               (i32.sub
                (local.get $18)
                (local.get $21)
               )
              )
              (i32.const 256)
              (local.tee $27
               (i32.lt_u
                (local.get $20)
                (i32.const 256)
               )
              )
             )
            )
           )
           (block $label$203
            (br_if $label$203
             (local.get $27)
            )
            (loop $label$204
             (block $label$205
              (br_if $label$205
               (i32.and
                (i32.load8_u
                 (local.get $0)
                )
                (i32.const 32)
               )
              )
              (drop
               (call $__fwritex
                (i32.add
                 (local.get $5)
                 (i32.const 112)
                )
                (i32.const 256)
                (local.get $0)
               )
              )
             )
             (br_if $label$204
              (i32.gt_u
               (local.tee $20
                (i32.add
                 (local.get $20)
                 (i32.const -256)
                )
               )
               (i32.const 255)
              )
             )
            )
           )
           (br_if $label$202
            (i32.and
             (i32.load8_u
              (local.get $0)
             )
             (i32.const 32)
            )
           )
           (drop
            (call $__fwritex
             (i32.add
              (local.get $5)
              (i32.const 112)
             )
             (local.get $20)
             (local.get $0)
            )
           )
          )
          (block $label$206
           (br_if $label$206
            (i32.ge_s
             (local.get $23)
             (local.get $24)
            )
           )
           (drop
            (call $memset
             (i32.add
              (local.get $5)
              (i32.const 112)
             )
             (i32.const 48)
             (select
              (local.tee $20
               (i32.sub
                (local.get $26)
                (local.get $23)
               )
              )
              (i32.const 256)
              (local.tee $24
               (i32.lt_u
                (local.get $20)
                (i32.const 256)
               )
              )
             )
            )
           )
           (block $label$207
            (br_if $label$207
             (local.get $24)
            )
            (loop $label$208
             (block $label$209
              (br_if $label$209
               (i32.and
                (i32.load8_u
                 (local.get $0)
                )
                (i32.const 32)
               )
              )
              (drop
               (call $__fwritex
                (i32.add
                 (local.get $5)
                 (i32.const 112)
                )
                (i32.const 256)
                (local.get $0)
               )
              )
             )
             (br_if $label$208
              (i32.gt_u
               (local.tee $20
                (i32.add
                 (local.get $20)
                 (i32.const -256)
                )
               )
               (i32.const 255)
              )
             )
            )
           )
           (br_if $label$206
            (i32.and
             (i32.load8_u
              (local.get $0)
             )
             (i32.const 32)
            )
           )
           (drop
            (call $__fwritex
             (i32.add
              (local.get $5)
              (i32.const 112)
             )
             (local.get $20)
             (local.get $0)
            )
           )
          )
          (block $label$210
           (br_if $label$210
            (i32.and
             (i32.load8_u
              (local.get $0)
             )
             (i32.const 32)
            )
           )
           (drop
            (call $__fwritex
             (local.get $19)
             (local.get $23)
             (local.get $0)
            )
           )
          )
          (br_if $label$4
           (i32.ne
            (local.get $28)
            (i32.const 8192)
           )
          )
          (br_if $label$4
           (i32.ge_s
            (local.get $21)
            (local.get $25)
           )
          )
          (drop
           (call $memset
            (i32.add
             (local.get $5)
             (i32.const 112)
            )
            (i32.const 32)
            (select
             (local.tee $20
              (i32.sub
               (local.get $18)
               (local.get $21)
              )
             )
             (i32.const 256)
             (local.tee $21
              (i32.lt_u
               (local.get $20)
               (i32.const 256)
              )
             )
            )
           )
          )
          (block $label$211
           (br_if $label$211
            (local.get $21)
           )
           (loop $label$212
            (block $label$213
             (br_if $label$213
              (i32.and
               (i32.load8_u
                (local.get $0)
               )
               (i32.const 32)
              )
             )
             (drop
              (call $__fwritex
               (i32.add
                (local.get $5)
                (i32.const 112)
               )
               (i32.const 256)
               (local.get $0)
              )
             )
            )
            (br_if $label$212
             (i32.gt_u
              (local.tee $20
               (i32.add
                (local.get $20)
                (i32.const -256)
               )
              )
              (i32.const 255)
             )
            )
           )
          )
          (br_if $label$4
           (i32.and
            (i32.load8_u
             (local.get $0)
            )
            (i32.const 32)
           )
          )
          (drop
           (call $__fwritex
            (i32.add
             (local.get $5)
             (i32.const 112)
            )
            (local.get $20)
            (local.get $0)
           )
          )
          (br $label$4)
         )
         (block $label$214
          (br_if $label$214
           (i32.lt_s
            (local.get $26)
            (i32.const 0)
           )
          )
          (local.set $28
           (select
            (local.get $28)
            (i32.add
             (local.get $21)
             (i32.const 4)
            )
            (i32.gt_u
             (local.get $28)
             (local.get $21)
            )
           )
          )
          (local.set $23
           (local.get $21)
          )
          (loop $label$215
           (block $label$216
            (block $label$217
             (br_if $label$217
              (i32.eqz
               (local.tee $18
                (i32.load
                 (local.get $23)
                )
               )
              )
             )
             (local.set $20
              (i32.const 0)
             )
             (loop $label$218
              (i32.store8
               (i32.add
                (i32.add
                 (i32.add
                  (local.get $5)
                  (i32.const 80)
                 )
                 (local.get $20)
                )
                (i32.const 8)
               )
               (i32.or
                (i32.sub
                 (local.get $18)
                 (i32.mul
                  (local.tee $19
                   (i32.div_u
                    (local.get $18)
                    (i32.const 10)
                   )
                  )
                  (i32.const 10)
                 )
                )
                (i32.const 48)
               )
              )
              (local.set $20
               (i32.add
                (local.get $20)
                (i32.const -1)
               )
              )
              (local.set $24
               (i32.gt_u
                (local.get $18)
                (i32.const 9)
               )
              )
              (local.set $18
               (local.get $19)
              )
              (br_if $label$218
               (local.get $24)
              )
             )
             (br_if $label$217
              (i32.eqz
               (local.get $20)
              )
             )
             (local.set $18
              (i32.add
               (i32.add
                (i32.add
                 (local.get $5)
                 (i32.const 80)
                )
                (local.get $20)
               )
               (i32.const 9)
              )
             )
             (br $label$216)
            )
            (i32.store8 offset=88
             (local.get $5)
             (i32.const 48)
            )
            (local.set $18
             (local.get $12)
            )
           )
           (block $label$219
            (block $label$220
             (br_if $label$220
              (i32.eq
               (local.get $23)
               (local.get $21)
              )
             )
             (br_if $label$219
              (i32.le_u
               (local.get $18)
               (i32.add
                (local.get $5)
                (i32.const 80)
               )
              )
             )
             (drop
              (call $memset
               (i32.add
                (local.get $5)
                (i32.const 80)
               )
               (i32.const 48)
               (i32.sub
                (local.get $18)
                (i32.add
                 (local.get $5)
                 (i32.const 80)
                )
               )
              )
             )
             (local.set $18
              (i32.add
               (local.get $5)
               (i32.const 80)
              )
             )
             (br $label$219)
            )
            (block $label$221
             (br_if $label$221
              (i32.and
               (i32.load8_u
                (local.get $0)
               )
               (i32.const 32)
              )
             )
             (drop
              (call $__fwritex
               (local.get $18)
               (i32.const 1)
               (local.get $0)
              )
             )
            )
            (local.set $18
             (i32.add
              (local.get $18)
              (i32.const 1)
             )
            )
            (block $label$222
             (br_if $label$222
              (local.get $40)
             )
             (br_if $label$219
              (i32.lt_s
               (local.get $26)
               (i32.const 1)
              )
             )
            )
            (br_if $label$219
             (i32.and
              (i32.load8_u
               (local.get $0)
              )
              (i32.const 32)
             )
            )
            (drop
             (call $__fwritex
              (i32.const 1069)
              (i32.const 1)
              (local.get $0)
             )
            )
           )
           (local.set $20
            (i32.sub
             (local.get $13)
             (local.get $18)
            )
           )
           (block $label$223
            (br_if $label$223
             (i32.and
              (i32.load8_u
               (local.get $0)
              )
              (i32.const 32)
             )
            )
            (drop
             (call $__fwritex
              (local.get $18)
              (select
               (local.get $20)
               (local.get $26)
               (i32.lt_s
                (local.get $20)
                (local.get $26)
               )
              )
              (local.get $0)
             )
            )
           )
           (local.set $26
            (i32.sub
             (local.get $26)
             (local.get $20)
            )
           )
           (br_if $label$214
            (i32.ge_u
             (local.tee $23
              (i32.add
               (local.get $23)
               (i32.const 4)
              )
             )
             (local.get $28)
            )
           )
           (br_if $label$215
            (i32.gt_s
             (local.get $26)
             (i32.const -1)
            )
           )
          )
         )
         (call $pad
          (local.get $0)
          (i32.const 48)
          (i32.add
           (local.get $26)
           (i32.const 18)
          )
          (i32.const 18)
          (i32.const 0)
         )
         (br_if $label$7
          (i32.and
           (i32.load8_u
            (local.get $0)
           )
           (i32.const 32)
          )
         )
         (drop
          (call $__fwritex
           (local.get $39)
           (i32.sub
            (local.get $6)
            (local.get $39)
           )
           (local.get $0)
          )
         )
        )
        (br_if $label$5
         (i32.ne
          (local.get $22)
          (i32.const 8192)
         )
        )
        (br_if $label$5
         (i32.le_s
          (local.get $25)
          (local.get $27)
         )
        )
        (drop
         (call $memset
          (i32.add
           (local.get $5)
           (i32.const 624)
          )
          (i32.const 32)
          (select
           (local.tee $18
            (i32.sub
             (local.get $25)
             (local.get $27)
            )
           )
           (i32.const 256)
           (local.tee $20
            (i32.lt_u
             (local.get $18)
             (i32.const 256)
            )
           )
          )
         )
        )
        (block $label$224
         (br_if $label$224
          (local.get $20)
         )
         (loop $label$225
          (block $label$226
           (br_if $label$226
            (i32.and
             (i32.load8_u
              (local.get $0)
             )
             (i32.const 32)
            )
           )
           (drop
            (call $__fwritex
             (i32.add
              (local.get $5)
              (i32.const 624)
             )
             (i32.const 256)
             (local.get $0)
            )
           )
          )
          (br_if $label$225
           (i32.gt_u
            (local.tee $18
             (i32.add
              (local.get $18)
              (i32.const -256)
             )
            )
            (i32.const 255)
           )
          )
         )
        )
        (br_if $label$5
         (i32.and
          (i32.load8_u
           (local.get $0)
          )
          (i32.const 32)
         )
        )
        (drop
         (call $__fwritex
          (i32.add
           (local.get $5)
           (i32.const 624)
          )
          (local.get $18)
          (local.get $0)
         )
        )
        (br $label$5)
       )
       (local.set $30
        (i32.add
         (local.get $36)
         (i32.and
          (i32.shr_s
           (i32.shl
            (local.get $29)
            (i32.const 26)
           )
           (i32.const 31)
          )
          (i32.const 9)
         )
        )
       )
       (block $label$227
        (br_if $label$227
         (i32.gt_u
          (local.get $23)
          (i32.const 11)
         )
        )
        (block $label$228
         (block $label$229
          (br_if $label$229
           (local.tee $20
            (i32.and
             (local.tee $18
              (i32.sub
               (i32.const 12)
               (local.get $23)
              )
             )
             (i32.const 7)
            )
           )
          )
          (local.set $42
           (f64.const 16)
          )
          (br $label$228)
         )
         (local.set $18
          (i32.add
           (local.get $23)
           (i32.const -12)
          )
         )
         (local.set $42
          (f64.const 16)
         )
         (loop $label$230
          (local.set $18
           (i32.add
            (local.get $18)
            (i32.const 1)
           )
          )
          (local.set $42
           (f64.mul
            (local.get $42)
            (f64.const 16)
           )
          )
          (br_if $label$230
           (local.tee $20
            (i32.add
             (local.get $20)
             (i32.const -1)
            )
           )
          )
         )
         (local.set $18
          (i32.sub
           (i32.const 0)
           (local.get $18)
          )
         )
        )
        (block $label$231
         (br_if $label$231
          (i32.lt_u
           (i32.add
            (local.get $23)
            (i32.const -5)
           )
           (i32.const 7)
          )
         )
         (loop $label$232
          (local.set $42
           (f64.mul
            (f64.mul
             (f64.mul
              (f64.mul
               (f64.mul
                (f64.mul
                 (f64.mul
                  (f64.mul
                   (local.get $42)
                   (f64.const 16)
                  )
                  (f64.const 16)
                 )
                 (f64.const 16)
                )
                (f64.const 16)
               )
               (f64.const 16)
              )
              (f64.const 16)
             )
             (f64.const 16)
            )
            (f64.const 16)
           )
          )
          (br_if $label$232
           (local.tee $18
            (i32.add
             (local.get $18)
             (i32.const -8)
            )
           )
          )
         )
        )
        (block $label$233
         (br_if $label$233
          (i32.ne
           (i32.load8_u
            (local.get $30)
           )
           (i32.const 45)
          )
         )
         (local.set $33
          (f64.neg
           (f64.add
            (local.get $42)
            (f64.sub
             (f64.neg
              (local.get $33)
             )
             (local.get $42)
            )
           )
          )
         )
         (br $label$227)
        )
        (local.set $33
         (f64.sub
          (f64.add
           (local.get $33)
           (local.get $42)
          )
          (local.get $42)
         )
        )
       )
       (block $label$234
        (block $label$235
         (br_if $label$235
          (i32.eqz
           (local.tee $24
            (i32.load offset=108
             (local.get $5)
            )
           )
          )
         )
         (local.set $18
          (i32.sub
           (i32.xor
            (local.get $24)
            (local.tee $18
             (i32.shr_s
              (local.get $24)
              (i32.const 31)
             )
            )
           )
           (local.get $18)
          )
         )
         (local.set $20
          (i32.const 0)
         )
         (loop $label$236
          (i32.store8
           (i32.add
            (i32.add
             (i32.add
              (local.get $5)
              (i32.const 68)
             )
             (local.get $20)
            )
            (i32.const 11)
           )
           (i32.or
            (i32.sub
             (local.get $18)
             (i32.mul
              (local.tee $21
               (i32.div_u
                (local.get $18)
                (i32.const 10)
               )
              )
              (i32.const 10)
             )
            )
            (i32.const 48)
           )
          )
          (local.set $20
           (i32.add
            (local.get $20)
            (i32.const -1)
           )
          )
          (local.set $19
           (i32.gt_u
            (local.get $18)
            (i32.const 9)
           )
          )
          (local.set $18
           (local.get $21)
          )
          (br_if $label$236
           (local.get $19)
          )
         )
         (br_if $label$235
          (i32.eqz
           (local.get $20)
          )
         )
         (local.set $18
          (i32.add
           (i32.add
            (i32.add
             (local.get $5)
             (i32.const 68)
            )
            (local.get $20)
           )
           (i32.const 12)
          )
         )
         (br $label$234)
        )
        (i32.store8 offset=79
         (local.get $5)
         (i32.const 48)
        )
        (local.set $18
         (local.get $10)
        )
       )
       (local.set $28
        (i32.or
         (local.get $34)
         (i32.const 2)
        )
       )
       (local.set $21
        (i32.and
         (local.get $29)
         (i32.const 32)
        )
       )
       (i32.store8
        (local.tee $26
         (i32.add
          (local.get $18)
          (i32.const -2)
         )
        )
        (i32.add
         (local.get $29)
         (i32.const 15)
        )
       )
       (i32.store8
        (i32.add
         (local.get $18)
         (i32.const -1)
        )
        (select
         (i32.const 45)
         (i32.const 43)
         (i32.lt_s
          (local.get $24)
          (i32.const 0)
         )
        )
       )
       (local.set $19
        (i32.and
         (local.get $22)
         (i32.const 8)
        )
       )
       (local.set $20
        (i32.add
         (local.get $5)
         (i32.const 80)
        )
       )
       (loop $label$237
        (local.set $18
         (local.get $20)
        )
        (block $label$238
         (block $label$239
          (br_if $label$239
           (i32.eqz
            (f64.lt
             (f64.abs
              (local.get $33)
             )
             (f64.const 2147483648)
            )
           )
          )
          (local.set $20
           (i32.trunc_f64_s
            (local.get $33)
           )
          )
          (br $label$238)
         )
         (local.set $20
          (i32.const -2147483648)
         )
        )
        (i32.store8
         (local.get $18)
         (i32.or
          (i32.load8_u
           (i32.add
            (local.get $20)
            (i32.const 3408)
           )
          )
          (local.get $21)
         )
        )
        (local.set $33
         (f64.mul
          (f64.sub
           (local.get $33)
           (f64.convert_i32_s
            (local.get $20)
           )
          )
          (f64.const 16)
         )
        )
        (block $label$240
         (br_if $label$240
          (i32.ne
           (i32.sub
            (local.tee $20
             (i32.add
              (local.get $18)
              (i32.const 1)
             )
            )
            (i32.add
             (local.get $5)
             (i32.const 80)
            )
           )
           (i32.const 1)
          )
         )
         (block $label$241
          (br_if $label$241
           (local.get $19)
          )
          (br_if $label$241
           (i32.gt_s
            (local.get $23)
            (i32.const 0)
           )
          )
          (br_if $label$240
           (f64.eq
            (local.get $33)
            (f64.const 0)
           )
          )
         )
         (i32.store8 offset=1
          (local.get $18)
          (i32.const 46)
         )
         (local.set $20
          (i32.add
           (local.get $18)
           (i32.const 2)
          )
         )
        )
        (br_if $label$237
         (f64.ne
          (local.get $33)
          (f64.const 0)
         )
        )
       )
       (br_if $label$3
        (i32.lt_s
         (i32.sub
          (i32.const 2147483645)
          (local.tee $18
           (i32.add
            (local.tee $24
             (i32.sub
              (local.get $6)
              (local.get $26)
             )
            )
            (local.get $28)
           )
          )
         )
         (local.get $23)
        )
       )
       (local.set $27
        (i32.add
         (local.tee $19
          (select
           (select
            (i32.add
             (local.get $23)
             (i32.const 2)
            )
            (local.tee $20
             (i32.sub
              (local.get $20)
              (i32.add
               (local.get $5)
               (i32.const 80)
              )
             )
            )
            (i32.lt_s
             (i32.add
              (local.get $20)
              (i32.const -2)
             )
             (local.get $23)
            )
           )
           (local.get $20)
           (local.get $23)
          )
         )
         (local.get $18)
        )
       )
       (block $label$242
        (br_if $label$242
         (local.tee $21
          (i32.and
           (local.get $22)
           (i32.const 73728)
          )
         )
        )
        (br_if $label$242
         (i32.le_s
          (local.get $25)
          (local.get $27)
         )
        )
        (drop
         (call $memset
          (i32.add
           (local.get $5)
           (i32.const 624)
          )
          (i32.const 32)
          (select
           (local.tee $18
            (i32.sub
             (local.get $25)
             (local.get $27)
            )
           )
           (i32.const 256)
           (local.tee $23
            (i32.lt_u
             (local.get $18)
             (i32.const 256)
            )
           )
          )
         )
        )
        (block $label$243
         (br_if $label$243
          (local.get $23)
         )
         (loop $label$244
          (block $label$245
           (br_if $label$245
            (i32.and
             (i32.load8_u
              (local.get $0)
             )
             (i32.const 32)
            )
           )
           (drop
            (call $__fwritex
             (i32.add
              (local.get $5)
              (i32.const 624)
             )
             (i32.const 256)
             (local.get $0)
            )
           )
          )
          (br_if $label$244
           (i32.gt_u
            (local.tee $18
             (i32.add
              (local.get $18)
              (i32.const -256)
             )
            )
            (i32.const 255)
           )
          )
         )
        )
        (br_if $label$242
         (i32.and
          (i32.load8_u
           (local.get $0)
          )
          (i32.const 32)
         )
        )
        (drop
         (call $__fwritex
          (i32.add
           (local.get $5)
           (i32.const 624)
          )
          (local.get $18)
          (local.get $0)
         )
        )
       )
       (block $label$246
        (br_if $label$246
         (i32.and
          (i32.load8_u
           (local.get $0)
          )
          (i32.const 32)
         )
        )
        (drop
         (call $__fwritex
          (local.get $30)
          (local.get $28)
          (local.get $0)
         )
        )
       )
       (block $label$247
        (br_if $label$247
         (i32.ne
          (local.get $21)
          (i32.const 65536)
         )
        )
        (br_if $label$247
         (i32.le_s
          (local.get $25)
          (local.get $27)
         )
        )
        (drop
         (call $memset
          (i32.add
           (local.get $5)
           (i32.const 624)
          )
          (i32.const 48)
          (select
           (local.tee $18
            (i32.sub
             (local.get $25)
             (local.get $27)
            )
           )
           (i32.const 256)
           (local.tee $23
            (i32.lt_u
             (local.get $18)
             (i32.const 256)
            )
           )
          )
         )
        )
        (block $label$248
         (br_if $label$248
          (local.get $23)
         )
         (loop $label$249
          (block $label$250
           (br_if $label$250
            (i32.and
             (i32.load8_u
              (local.get $0)
             )
             (i32.const 32)
            )
           )
           (drop
            (call $__fwritex
             (i32.add
              (local.get $5)
              (i32.const 624)
             )
             (i32.const 256)
             (local.get $0)
            )
           )
          )
          (br_if $label$249
           (i32.gt_u
            (local.tee $18
             (i32.add
              (local.get $18)
              (i32.const -256)
             )
            )
            (i32.const 255)
           )
          )
         )
        )
        (br_if $label$247
         (i32.and
          (i32.load8_u
           (local.get $0)
          )
          (i32.const 32)
         )
        )
        (drop
         (call $__fwritex
          (i32.add
           (local.get $5)
           (i32.const 624)
          )
          (local.get $18)
          (local.get $0)
         )
        )
       )
       (block $label$251
        (br_if $label$251
         (i32.and
          (i32.load8_u
           (local.get $0)
          )
          (i32.const 32)
         )
        )
        (drop
         (call $__fwritex
          (i32.add
           (local.get $5)
           (i32.const 80)
          )
          (local.get $20)
          (local.get $0)
         )
        )
       )
       (block $label$252
        (br_if $label$252
         (i32.lt_s
          (local.tee $18
           (i32.sub
            (local.get $19)
            (local.get $20)
           )
          )
          (i32.const 1)
         )
        )
        (drop
         (call $memset
          (i32.add
           (local.get $5)
           (i32.const 624)
          )
          (i32.const 48)
          (select
           (local.get $18)
           (i32.const 256)
           (local.tee $20
            (i32.lt_u
             (local.get $18)
             (i32.const 256)
            )
           )
          )
         )
        )
        (block $label$253
         (br_if $label$253
          (local.get $20)
         )
         (loop $label$254
          (block $label$255
           (br_if $label$255
            (i32.and
             (i32.load8_u
              (local.get $0)
             )
             (i32.const 32)
            )
           )
           (drop
            (call $__fwritex
             (i32.add
              (local.get $5)
              (i32.const 624)
             )
             (i32.const 256)
             (local.get $0)
            )
           )
          )
          (br_if $label$254
           (i32.gt_u
            (local.tee $18
             (i32.add
              (local.get $18)
              (i32.const -256)
             )
            )
            (i32.const 255)
           )
          )
         )
        )
        (br_if $label$252
         (i32.and
          (i32.load8_u
           (local.get $0)
          )
          (i32.const 32)
         )
        )
        (drop
         (call $__fwritex
          (i32.add
           (local.get $5)
           (i32.const 624)
          )
          (local.get $18)
          (local.get $0)
         )
        )
       )
       (block $label$256
        (br_if $label$256
         (i32.and
          (i32.load8_u
           (local.get $0)
          )
          (i32.const 32)
         )
        )
        (drop
         (call $__fwritex
          (local.get $26)
          (local.get $24)
          (local.get $0)
         )
        )
       )
       (br_if $label$5
        (i32.ne
         (local.get $21)
         (i32.const 8192)
        )
       )
       (br_if $label$5
        (i32.le_s
         (local.get $25)
         (local.get $27)
        )
       )
       (drop
        (call $memset
         (i32.add
          (local.get $5)
          (i32.const 624)
         )
         (i32.const 32)
         (select
          (local.tee $18
           (i32.sub
            (local.get $25)
            (local.get $27)
           )
          )
          (i32.const 256)
          (local.tee $20
           (i32.lt_u
            (local.get $18)
            (i32.const 256)
           )
          )
         )
        )
       )
       (block $label$257
        (br_if $label$257
         (local.get $20)
        )
        (loop $label$258
         (block $label$259
          (br_if $label$259
           (i32.and
            (i32.load8_u
             (local.get $0)
            )
            (i32.const 32)
           )
          )
          (drop
           (call $__fwritex
            (i32.add
             (local.get $5)
             (i32.const 624)
            )
            (i32.const 256)
            (local.get $0)
           )
          )
         )
         (br_if $label$258
          (i32.gt_u
           (local.tee $18
            (i32.add
             (local.get $18)
             (i32.const -256)
            )
           )
           (i32.const 255)
          )
         )
        )
       )
       (br_if $label$5
        (i32.and
         (i32.load8_u
          (local.get $0)
         )
         (i32.const 32)
        )
       )
       (drop
        (call $__fwritex
         (i32.add
          (local.get $5)
          (i32.const 624)
         )
         (local.get $18)
         (local.get $0)
        )
       )
      )
      (br_if $label$4
       (i32.ge_s
        (local.tee $18
         (select
          (local.get $27)
          (local.get $25)
          (i32.gt_s
           (local.get $27)
           (local.get $25)
          )
         )
        )
        (i32.const 0)
       )
      )
     )
    )
    (i32.store
     (i32.const 3668)
     (i32.const 61)
    )
   )
   (local.set $17
    (i32.const -1)
   )
  )
  (global.set $__stack_pointer
   (i32.add
    (local.get $5)
    (i32.const 880)
   )
  )
  (local.get $17)
 )
 (func $pop_arg (param $0 i32) (param $1 i32) (param $2 i32)
  (block $label$1
   (block $label$2
    (block $label$3
     (block $label$4
      (block $label$5
       (block $label$6
        (block $label$7
         (block $label$8
          (block $label$9
           (block $label$10
            (block $label$11
             (block $label$12
              (block $label$13
               (block $label$14
                (block $label$15
                 (block $label$16
                  (block $label$17
                   (block $label$18
                    (block $label$19
                     (br_table $label$2 $label$19 $label$18 $label$15 $label$17 $label$16 $label$14 $label$13 $label$12 $label$11 $label$10 $label$9 $label$8 $label$7 $label$6 $label$5 $label$4 $label$3 $label$1
                      (i32.add
                       (local.get $1)
                       (i32.const -9)
                      )
                     )
                    )
                    (i32.store
                     (local.get $2)
                     (i32.add
                      (local.tee $1
                       (i32.load
                        (local.get $2)
                       )
                      )
                      (i32.const 4)
                     )
                    )
                    (i64.store
                     (local.get $0)
                     (i64.load32_s
                      (local.get $1)
                     )
                    )
                    (return)
                   )
                   (i32.store
                    (local.get $2)
                    (i32.add
                     (local.tee $1
                      (i32.load
                       (local.get $2)
                      )
                     )
                     (i32.const 4)
                    )
                   )
                   (i64.store
                    (local.get $0)
                    (i64.load32_u
                     (local.get $1)
                    )
                   )
                   (return)
                  )
                  (i32.store
                   (local.get $2)
                   (i32.add
                    (local.tee $1
                     (i32.load
                      (local.get $2)
                     )
                    )
                    (i32.const 4)
                   )
                  )
                  (i64.store
                   (local.get $0)
                   (i64.load32_s
                    (local.get $1)
                   )
                  )
                  (return)
                 )
                 (i32.store
                  (local.get $2)
                  (i32.add
                   (local.tee $1
                    (i32.load
                     (local.get $2)
                    )
                   )
                   (i32.const 4)
                  )
                 )
                 (i64.store
                  (local.get $0)
                  (i64.load32_u
                   (local.get $1)
                  )
                 )
                 (return)
                )
                (i32.store
                 (local.get $2)
                 (i32.add
                  (local.tee $1
                   (i32.and
                    (i32.add
                     (i32.load
                      (local.get $2)
                     )
                     (i32.const 7)
                    )
                    (i32.const -8)
                   )
                  )
                  (i32.const 8)
                 )
                )
                (i64.store
                 (local.get $0)
                 (i64.load
                  (local.get $1)
                 )
                )
                (return)
               )
               (i32.store
                (local.get $2)
                (i32.add
                 (local.tee $1
                  (i32.load
                   (local.get $2)
                  )
                 )
                 (i32.const 4)
                )
               )
               (i64.store
                (local.get $0)
                (i64.load16_s
                 (local.get $1)
                )
               )
               (return)
              )
              (i32.store
               (local.get $2)
               (i32.add
                (local.tee $1
                 (i32.load
                  (local.get $2)
                 )
                )
                (i32.const 4)
               )
              )
              (i64.store
               (local.get $0)
               (i64.load16_u
                (local.get $1)
               )
              )
              (return)
             )
             (i32.store
              (local.get $2)
              (i32.add
               (local.tee $1
                (i32.load
                 (local.get $2)
                )
               )
               (i32.const 4)
              )
             )
             (i64.store
              (local.get $0)
              (i64.load8_s
               (local.get $1)
              )
             )
             (return)
            )
            (i32.store
             (local.get $2)
             (i32.add
              (local.tee $1
               (i32.load
                (local.get $2)
               )
              )
              (i32.const 4)
             )
            )
            (i64.store
             (local.get $0)
             (i64.load8_u
              (local.get $1)
             )
            )
            (return)
           )
           (i32.store
            (local.get $2)
            (i32.add
             (local.tee $1
              (i32.and
               (i32.add
                (i32.load
                 (local.get $2)
                )
                (i32.const 7)
               )
               (i32.const -8)
              )
             )
             (i32.const 8)
            )
           )
           (i64.store
            (local.get $0)
            (i64.load
             (local.get $1)
            )
           )
           (return)
          )
          (i32.store
           (local.get $2)
           (i32.add
            (local.tee $1
             (i32.load
              (local.get $2)
             )
            )
            (i32.const 4)
           )
          )
          (i64.store
           (local.get $0)
           (i64.load32_u
            (local.get $1)
           )
          )
          (return)
         )
         (i32.store
          (local.get $2)
          (i32.add
           (local.tee $1
            (i32.and
             (i32.add
              (i32.load
               (local.get $2)
              )
              (i32.const 7)
             )
             (i32.const -8)
            )
           )
           (i32.const 8)
          )
         )
         (i64.store
          (local.get $0)
          (i64.load
           (local.get $1)
          )
         )
         (return)
        )
        (i32.store
         (local.get $2)
         (i32.add
          (local.tee $1
           (i32.and
            (i32.add
             (i32.load
              (local.get $2)
             )
             (i32.const 7)
            )
            (i32.const -8)
           )
          )
          (i32.const 8)
         )
        )
        (i64.store
         (local.get $0)
         (i64.load
          (local.get $1)
         )
        )
        (return)
       )
       (i32.store
        (local.get $2)
        (i32.add
         (local.tee $1
          (i32.load
           (local.get $2)
          )
         )
         (i32.const 4)
        )
       )
       (i64.store
        (local.get $0)
        (i64.load32_s
         (local.get $1)
        )
       )
       (return)
      )
      (i32.store
       (local.get $2)
       (i32.add
        (local.tee $1
         (i32.load
          (local.get $2)
         )
        )
        (i32.const 4)
       )
      )
      (i64.store
       (local.get $0)
       (i64.load32_u
        (local.get $1)
       )
      )
      (return)
     )
     (i32.store
      (local.get $2)
      (i32.add
       (local.tee $1
        (i32.and
         (i32.add
          (i32.load
           (local.get $2)
          )
          (i32.const 7)
         )
         (i32.const -8)
        )
       )
       (i32.const 8)
      )
     )
     (f64.store
      (local.get $0)
      (f64.load
       (local.get $1)
      )
     )
     (return)
    )
    (call $long_double_not_supported)
    (unreachable)
   )
   (i32.store
    (local.get $2)
    (i32.add
     (local.tee $1
      (i32.load
       (local.get $2)
      )
     )
     (i32.const 4)
    )
   )
   (i32.store
    (local.get $0)
    (i32.load
     (local.get $1)
    )
   )
  )
 )
 (func $pad (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
  (local $5 i32)
  (global.set $__stack_pointer
   (local.tee $5
    (i32.sub
     (global.get $__stack_pointer)
     (i32.const 256)
    )
   )
  )
  (block $label$1
   (br_if $label$1
    (i32.le_s
     (local.get $2)
     (local.get $3)
    )
   )
   (br_if $label$1
    (i32.and
     (local.get $4)
     (i32.const 73728)
    )
   )
   (local.set $2
    (call $memset
     (local.get $5)
     (local.get $1)
     (select
      (local.tee $3
       (i32.sub
        (local.get $2)
        (local.get $3)
       )
      )
      (i32.const 256)
      (local.tee $4
       (i32.lt_u
        (local.get $3)
        (i32.const 256)
       )
      )
     )
    )
   )
   (block $label$2
    (br_if $label$2
     (local.get $4)
    )
    (loop $label$3
     (block $label$4
      (br_if $label$4
       (i32.and
        (i32.load8_u
         (local.get $0)
        )
        (i32.const 32)
       )
      )
      (drop
       (call $__fwritex
        (local.get $2)
        (i32.const 256)
        (local.get $0)
       )
      )
     )
     (br_if $label$3
      (i32.gt_u
       (local.tee $3
        (i32.add
         (local.get $3)
         (i32.const -256)
        )
       )
       (i32.const 255)
      )
     )
    )
   )
   (br_if $label$1
    (i32.and
     (i32.load8_u
      (local.get $0)
     )
     (i32.const 32)
    )
   )
   (drop
    (call $__fwritex
     (local.get $2)
     (local.get $3)
     (local.get $0)
    )
   )
  )
  (global.set $__stack_pointer
   (i32.add
    (local.get $5)
    (i32.const 256)
   )
  )
 )
 (func $long_double_not_supported
  (drop
   (call $fputs
    (i32.const 1078)
    (i32.const 3544)
   )
  )
  (call $abort)
  (unreachable)
 )
 (func $memcpy (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (block $label$1
   (block $label$2
    (block $label$3
     (br_if $label$3
      (i32.gt_u
       (local.get $2)
       (i32.const 32)
      )
     )
     (br_if $label$2
      (i32.eqz
       (i32.and
        (local.get $1)
        (i32.const 3)
       )
      )
     )
     (br_if $label$2
      (i32.eqz
       (local.get $2)
      )
     )
     (i32.store8
      (local.get $0)
      (i32.load8_u
       (local.get $1)
      )
     )
     (local.set $3
      (i32.add
       (local.get $2)
       (i32.const -1)
      )
     )
     (local.set $4
      (i32.add
       (local.get $0)
       (i32.const 1)
      )
     )
     (br_if $label$1
      (i32.eqz
       (i32.and
        (local.tee $5
         (i32.add
          (local.get $1)
          (i32.const 1)
         )
        )
        (i32.const 3)
       )
      )
     )
     (br_if $label$1
      (i32.eqz
       (local.get $3)
      )
     )
     (i32.store8 offset=1
      (local.get $0)
      (i32.load8_u offset=1
       (local.get $1)
      )
     )
     (local.set $3
      (i32.add
       (local.get $2)
       (i32.const -2)
      )
     )
     (local.set $4
      (i32.add
       (local.get $0)
       (i32.const 2)
      )
     )
     (br_if $label$1
      (i32.eqz
       (i32.and
        (local.tee $5
         (i32.add
          (local.get $1)
          (i32.const 2)
         )
        )
        (i32.const 3)
       )
      )
     )
     (br_if $label$1
      (i32.eqz
       (local.get $3)
      )
     )
     (i32.store8 offset=2
      (local.get $0)
      (i32.load8_u offset=2
       (local.get $1)
      )
     )
     (local.set $3
      (i32.add
       (local.get $2)
       (i32.const -3)
      )
     )
     (local.set $4
      (i32.add
       (local.get $0)
       (i32.const 3)
      )
     )
     (br_if $label$1
      (i32.eqz
       (i32.and
        (local.tee $5
         (i32.add
          (local.get $1)
          (i32.const 3)
         )
        )
        (i32.const 3)
       )
      )
     )
     (br_if $label$1
      (i32.eqz
       (local.get $3)
      )
     )
     (i32.store8 offset=3
      (local.get $0)
      (i32.load8_u offset=3
       (local.get $1)
      )
     )
     (local.set $3
      (i32.add
       (local.get $2)
       (i32.const -4)
      )
     )
     (local.set $4
      (i32.add
       (local.get $0)
       (i32.const 4)
      )
     )
     (local.set $5
      (i32.add
       (local.get $1)
       (i32.const 4)
      )
     )
     (br $label$1)
    )
    (memory.copy
     (local.get $0)
     (local.get $1)
     (local.get $2)
    )
    (return
     (local.get $0)
    )
   )
   (local.set $3
    (local.get $2)
   )
   (local.set $4
    (local.get $0)
   )
   (local.set $5
    (local.get $1)
   )
  )
  (block $label$4
   (block $label$5
    (br_if $label$5
     (local.tee $2
      (i32.and
       (local.get $4)
       (i32.const 3)
      )
     )
    )
    (block $label$6
     (block $label$7
      (br_if $label$7
       (i32.ge_u
        (local.get $3)
        (i32.const 16)
       )
      )
      (local.set $2
       (local.get $3)
      )
      (br $label$6)
     )
     (block $label$8
      (br_if $label$8
       (i32.and
        (local.tee $2
         (i32.add
          (local.get $3)
          (i32.const -16)
         )
        )
        (i32.const 16)
       )
      )
      (i64.store align=4
       (local.get $4)
       (i64.load align=4
        (local.get $5)
       )
      )
      (i64.store offset=8 align=4
       (local.get $4)
       (i64.load offset=8 align=4
        (local.get $5)
       )
      )
      (local.set $4
       (i32.add
        (local.get $4)
        (i32.const 16)
       )
      )
      (local.set $5
       (i32.add
        (local.get $5)
        (i32.const 16)
       )
      )
      (local.set $3
       (local.get $2)
      )
     )
     (br_if $label$6
      (i32.lt_u
       (local.get $2)
       (i32.const 16)
      )
     )
     (local.set $2
      (local.get $3)
     )
     (loop $label$9
      (i64.store align=4
       (local.get $4)
       (i64.load align=4
        (local.get $5)
       )
      )
      (i64.store offset=8 align=4
       (local.get $4)
       (i64.load offset=8 align=4
        (local.get $5)
       )
      )
      (i64.store offset=16 align=4
       (local.get $4)
       (i64.load offset=16 align=4
        (local.get $5)
       )
      )
      (i64.store offset=24 align=4
       (local.get $4)
       (i64.load offset=24 align=4
        (local.get $5)
       )
      )
      (local.set $4
       (i32.add
        (local.get $4)
        (i32.const 32)
       )
      )
      (local.set $5
       (i32.add
        (local.get $5)
        (i32.const 32)
       )
      )
      (br_if $label$9
       (i32.gt_u
        (local.tee $2
         (i32.add
          (local.get $2)
          (i32.const -32)
         )
        )
        (i32.const 15)
       )
      )
     )
    )
    (block $label$10
     (br_if $label$10
      (i32.lt_u
       (local.get $2)
       (i32.const 8)
      )
     )
     (i64.store align=4
      (local.get $4)
      (i64.load align=4
       (local.get $5)
      )
     )
     (local.set $5
      (i32.add
       (local.get $5)
       (i32.const 8)
      )
     )
     (local.set $4
      (i32.add
       (local.get $4)
       (i32.const 8)
      )
     )
    )
    (block $label$11
     (br_if $label$11
      (i32.eqz
       (i32.and
        (local.get $2)
        (i32.const 4)
       )
      )
     )
     (i32.store
      (local.get $4)
      (i32.load
       (local.get $5)
      )
     )
     (local.set $5
      (i32.add
       (local.get $5)
       (i32.const 4)
      )
     )
     (local.set $4
      (i32.add
       (local.get $4)
       (i32.const 4)
      )
     )
    )
    (block $label$12
     (br_if $label$12
      (i32.eqz
       (i32.and
        (local.get $2)
        (i32.const 2)
       )
      )
     )
     (i32.store16 align=1
      (local.get $4)
      (i32.load16_u align=1
       (local.get $5)
      )
     )
     (local.set $4
      (i32.add
       (local.get $4)
       (i32.const 2)
      )
     )
     (local.set $5
      (i32.add
       (local.get $5)
       (i32.const 2)
      )
     )
    )
    (br_if $label$4
     (i32.eqz
      (i32.and
       (local.get $2)
       (i32.const 1)
      )
     )
    )
    (i32.store8
     (local.get $4)
     (i32.load8_u
      (local.get $5)
     )
    )
    (return
     (local.get $0)
    )
   )
   (block $label$13
    (block $label$14
     (block $label$15
      (block $label$16
       (block $label$17
        (br_if $label$17
         (i32.lt_u
          (local.get $3)
          (i32.const 32)
         )
        )
        (block $label$18
         (block $label$19
          (br_table $label$16 $label$19 $label$18 $label$4
           (i32.add
            (local.get $2)
            (i32.const -1)
           )
          )
         )
         (i32.store16 align=1
          (local.get $4)
          (i32.load
           (local.get $5)
          )
         )
         (i32.store offset=2
          (local.get $4)
          (i32.load align=2
           (i32.add
            (local.get $5)
            (i32.const 2)
           )
          )
         )
         (i64.store offset=6 align=4
          (local.get $4)
          (i64.load align=2
           (i32.add
            (local.get $5)
            (i32.const 6)
           )
          )
         )
         (local.set $2
          (i32.add
           (local.get $4)
           (i32.const 18)
          )
         )
         (local.set $1
          (i32.add
           (local.get $5)
           (i32.const 18)
          )
         )
         (local.set $6
          (i32.const 14)
         )
         (local.set $5
          (i32.load align=2
           (i32.add
            (local.get $5)
            (i32.const 14)
           )
          )
         )
         (local.set $3
          (i32.const 14)
         )
         (br $label$15)
        )
        (i32.store8
         (local.get $4)
         (i32.load
          (local.get $5)
         )
        )
        (i32.store offset=1
         (local.get $4)
         (i32.load align=1
          (i32.add
           (local.get $5)
           (i32.const 1)
          )
         )
        )
        (i64.store offset=5 align=4
         (local.get $4)
         (i64.load align=1
          (i32.add
           (local.get $5)
           (i32.const 5)
          )
         )
        )
        (local.set $2
         (i32.add
          (local.get $4)
          (i32.const 17)
         )
        )
        (local.set $1
         (i32.add
          (local.get $5)
          (i32.const 17)
         )
        )
        (local.set $6
         (i32.const 13)
        )
        (local.set $5
         (i32.load align=1
          (i32.add
           (local.get $5)
           (i32.const 13)
          )
         )
        )
        (local.set $3
         (i32.const 15)
        )
        (br $label$15)
       )
       (block $label$20
        (block $label$21
         (br_if $label$21
          (i32.ge_u
           (local.get $3)
           (i32.const 16)
          )
         )
         (local.set $2
          (local.get $4)
         )
         (local.set $1
          (local.get $5)
         )
         (br $label$20)
        )
        (i32.store8
         (local.get $4)
         (i32.load8_u
          (local.get $5)
         )
        )
        (i32.store offset=1 align=1
         (local.get $4)
         (i32.load offset=1 align=1
          (local.get $5)
         )
        )
        (i64.store offset=5 align=1
         (local.get $4)
         (i64.load offset=5 align=1
          (local.get $5)
         )
        )
        (i32.store16 offset=13 align=1
         (local.get $4)
         (i32.load16_u offset=13 align=1
          (local.get $5)
         )
        )
        (i32.store8 offset=15
         (local.get $4)
         (i32.load8_u offset=15
          (local.get $5)
         )
        )
        (local.set $2
         (i32.add
          (local.get $4)
          (i32.const 16)
         )
        )
        (local.set $1
         (i32.add
          (local.get $5)
          (i32.const 16)
         )
        )
       )
       (br_if $label$14
        (i32.and
         (local.get $3)
         (i32.const 8)
        )
       )
       (br $label$13)
      )
      (i32.store8
       (local.get $4)
       (local.tee $2
        (i32.load
         (local.get $5)
        )
       )
      )
      (i32.store8 offset=2
       (local.get $4)
       (i32.shr_u
        (local.get $2)
        (i32.const 16)
       )
      )
      (i32.store8 offset=1
       (local.get $4)
       (i32.shr_u
        (local.get $2)
        (i32.const 8)
       )
      )
      (i32.store offset=3
       (local.get $4)
       (i32.load align=1
        (i32.add
         (local.get $5)
         (i32.const 3)
        )
       )
      )
      (i64.store offset=7 align=4
       (local.get $4)
       (i64.load align=1
        (i32.add
         (local.get $5)
         (i32.const 7)
        )
       )
      )
      (local.set $2
       (i32.add
        (local.get $4)
        (i32.const 19)
       )
      )
      (local.set $1
       (i32.add
        (local.get $5)
        (i32.const 19)
       )
      )
      (local.set $6
       (i32.const 15)
      )
      (local.set $5
       (i32.load align=1
        (i32.add
         (local.get $5)
         (i32.const 15)
        )
       )
      )
      (local.set $3
       (i32.const 13)
      )
     )
     (i32.store
      (i32.add
       (local.get $4)
       (local.get $6)
      )
      (local.get $5)
     )
    )
    (i64.store align=1
     (local.get $2)
     (i64.load align=1
      (local.get $1)
     )
    )
    (local.set $2
     (i32.add
      (local.get $2)
      (i32.const 8)
     )
    )
    (local.set $1
     (i32.add
      (local.get $1)
      (i32.const 8)
     )
    )
   )
   (block $label$22
    (br_if $label$22
     (i32.eqz
      (i32.and
       (local.get $3)
       (i32.const 4)
      )
     )
    )
    (i32.store align=1
     (local.get $2)
     (i32.load align=1
      (local.get $1)
     )
    )
    (local.set $2
     (i32.add
      (local.get $2)
      (i32.const 4)
     )
    )
    (local.set $1
     (i32.add
      (local.get $1)
      (i32.const 4)
     )
    )
   )
   (block $label$23
    (br_if $label$23
     (i32.eqz
      (i32.and
       (local.get $3)
       (i32.const 2)
      )
     )
    )
    (i32.store16 align=1
     (local.get $2)
     (i32.load16_u align=1
      (local.get $1)
     )
    )
    (local.set $2
     (i32.add
      (local.get $2)
      (i32.const 2)
     )
    )
    (local.set $1
     (i32.add
      (local.get $1)
      (i32.const 2)
     )
    )
   )
   (br_if $label$4
    (i32.eqz
     (i32.and
      (local.get $3)
      (i32.const 1)
     )
    )
   )
   (i32.store8
    (local.get $2)
    (i32.load8_u
     (local.get $1)
    )
   )
  )
  (local.get $0)
 )
 (func $memset (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i64)
  (block $label$1
   (br_if $label$1
    (i32.lt_u
     (local.get $2)
     (i32.const 33)
    )
   )
   (memory.fill
    (local.get $0)
    (local.get $1)
    (local.get $2)
   )
   (return
    (local.get $0)
   )
  )
  (block $label$2
   (br_if $label$2
    (i32.eqz
     (local.get $2)
    )
   )
   (i32.store8
    (local.get $0)
    (local.get $1)
   )
   (i32.store8
    (i32.add
     (local.tee $3
      (i32.add
       (local.get $2)
       (local.get $0)
      )
     )
     (i32.const -1)
    )
    (local.get $1)
   )
   (br_if $label$2
    (i32.lt_u
     (local.get $2)
     (i32.const 3)
    )
   )
   (i32.store8 offset=2
    (local.get $0)
    (local.get $1)
   )
   (i32.store8 offset=1
    (local.get $0)
    (local.get $1)
   )
   (i32.store8
    (i32.add
     (local.get $3)
     (i32.const -3)
    )
    (local.get $1)
   )
   (i32.store8
    (i32.add
     (local.get $3)
     (i32.const -2)
    )
    (local.get $1)
   )
   (br_if $label$2
    (i32.lt_u
     (local.get $2)
     (i32.const 7)
    )
   )
   (i32.store8 offset=3
    (local.get $0)
    (local.get $1)
   )
   (i32.store8
    (i32.add
     (local.get $3)
     (i32.const -4)
    )
    (local.get $1)
   )
   (br_if $label$2
    (i32.lt_u
     (local.get $2)
     (i32.const 9)
    )
   )
   (i32.store
    (local.tee $5
     (i32.add
      (local.get $0)
      (local.tee $4
       (i32.and
        (i32.sub
         (i32.const 0)
         (local.get $0)
        )
        (i32.const 3)
       )
      )
     )
    )
    (local.tee $3
     (i32.mul
      (i32.and
       (local.get $1)
       (i32.const 255)
      )
      (i32.const 16843009)
     )
    )
   )
   (i32.store
    (i32.add
     (local.tee $2
      (i32.add
       (local.get $5)
       (local.tee $1
        (i32.and
         (i32.sub
          (local.get $2)
          (local.get $4)
         )
         (i32.const -4)
        )
       )
      )
     )
     (i32.const -4)
    )
    (local.get $3)
   )
   (br_if $label$2
    (i32.lt_u
     (local.get $1)
     (i32.const 9)
    )
   )
   (i32.store offset=8
    (local.get $5)
    (local.get $3)
   )
   (i32.store offset=4
    (local.get $5)
    (local.get $3)
   )
   (i32.store
    (i32.add
     (local.get $2)
     (i32.const -8)
    )
    (local.get $3)
   )
   (i32.store
    (i32.add
     (local.get $2)
     (i32.const -12)
    )
    (local.get $3)
   )
   (br_if $label$2
    (i32.lt_u
     (local.get $1)
     (i32.const 25)
    )
   )
   (i32.store offset=24
    (local.get $5)
    (local.get $3)
   )
   (i32.store offset=20
    (local.get $5)
    (local.get $3)
   )
   (i32.store offset=16
    (local.get $5)
    (local.get $3)
   )
   (i32.store offset=12
    (local.get $5)
    (local.get $3)
   )
   (i32.store
    (i32.add
     (local.get $2)
     (i32.const -16)
    )
    (local.get $3)
   )
   (i32.store
    (i32.add
     (local.get $2)
     (i32.const -20)
    )
    (local.get $3)
   )
   (i32.store
    (i32.add
     (local.get $2)
     (i32.const -24)
    )
    (local.get $3)
   )
   (i32.store
    (i32.add
     (local.get $2)
     (i32.const -28)
    )
    (local.get $3)
   )
   (br_if $label$2
    (i32.lt_u
     (local.tee $1
      (i32.sub
       (local.get $1)
       (local.tee $2
        (i32.or
         (i32.and
          (local.get $5)
          (i32.const 4)
         )
         (i32.const 24)
        )
       )
      )
     )
     (i32.const 32)
    )
   )
   (local.set $6
    (i64.mul
     (i64.extend_i32_u
      (local.get $3)
     )
     (i64.const 4294967297)
    )
   )
   (local.set $2
    (i32.add
     (local.get $5)
     (local.get $2)
    )
   )
   (loop $label$3
    (i64.store offset=24
     (local.get $2)
     (local.get $6)
    )
    (i64.store offset=16
     (local.get $2)
     (local.get $6)
    )
    (i64.store offset=8
     (local.get $2)
     (local.get $6)
    )
    (i64.store
     (local.get $2)
     (local.get $6)
    )
    (local.set $2
     (i32.add
      (local.get $2)
      (i32.const 32)
     )
    )
    (br_if $label$3
     (i32.gt_u
      (local.tee $1
       (i32.add
        (local.get $1)
        (i32.const -32)
       )
      )
      (i32.const 31)
     )
    )
   )
  )
  (local.get $0)
 )
 (func $strlen (param $0 i32) (result i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local.set $1
   (local.get $0)
  )
  (block $label$1
   (block $label$2
    (br_if $label$2
     (i32.eqz
      (i32.and
       (local.get $0)
       (i32.const 3)
      )
     )
    )
    (block $label$3
     (br_if $label$3
      (i32.load8_u
       (local.get $0)
      )
     )
     (return
      (i32.sub
       (local.get $0)
       (local.get $0)
      )
     )
    )
    (br_if $label$2
     (i32.eqz
      (i32.and
       (local.tee $1
        (i32.add
         (local.get $0)
         (i32.const 1)
        )
       )
       (i32.const 3)
      )
     )
    )
    (br_if $label$1
     (i32.eqz
      (i32.load8_u
       (local.get $1)
      )
     )
    )
    (br_if $label$2
     (i32.eqz
      (i32.and
       (local.tee $1
        (i32.add
         (local.get $0)
         (i32.const 2)
        )
       )
       (i32.const 3)
      )
     )
    )
    (br_if $label$1
     (i32.eqz
      (i32.load8_u
       (local.get $1)
      )
     )
    )
    (br_if $label$2
     (i32.eqz
      (i32.and
       (local.tee $1
        (i32.add
         (local.get $0)
         (i32.const 3)
        )
       )
       (i32.const 3)
      )
     )
    )
    (br_if $label$1
     (i32.eqz
      (i32.load8_u
       (local.get $1)
      )
     )
    )
    (br_if $label$1
     (i32.and
      (local.tee $1
       (i32.add
        (local.get $0)
        (i32.const 4)
       )
      )
      (i32.const 3)
     )
    )
   )
   (local.set $2
    (i32.add
     (local.get $1)
     (i32.const -4)
    )
   )
   (local.set $1
    (i32.add
     (local.get $1)
     (i32.const -5)
    )
   )
   (loop $label$4
    (local.set $1
     (i32.add
      (local.get $1)
      (i32.const 4)
     )
    )
    (br_if $label$4
     (i32.eqz
      (i32.and
       (i32.and
        (i32.xor
         (local.tee $3
          (i32.load
           (local.tee $2
            (i32.add
             (local.get $2)
             (i32.const 4)
            )
           )
          )
         )
         (i32.const -1)
        )
        (i32.add
         (local.get $3)
         (i32.const -16843009)
        )
       )
       (i32.const -2139062144)
      )
     )
    )
   )
   (loop $label$5
    (local.set $1
     (i32.add
      (local.get $1)
      (i32.const 1)
     )
    )
    (local.set $3
     (i32.load8_u
      (local.get $2)
     )
    )
    (local.set $2
     (i32.add
      (local.get $2)
      (i32.const 1)
     )
    )
    (br_if $label$5
     (local.get $3)
    )
   )
  )
  (i32.sub
   (local.get $1)
   (local.get $0)
  )
 )
 (func $memchr (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local.set $3
   (i32.ne
    (local.get $2)
    (i32.const 0)
   )
  )
  (block $label$1
   (block $label$2
    (block $label$3
     (block $label$4
      (br_if $label$4
       (i32.eqz
        (i32.and
         (local.get $0)
         (i32.const 3)
        )
       )
      )
      (br_if $label$4
       (i32.eqz
        (local.get $2)
       )
      )
      (block $label$5
       (br_if $label$5
        (i32.ne
         (i32.load8_u
          (local.get $0)
         )
         (i32.and
          (local.get $1)
          (i32.const 255)
         )
        )
       )
       (local.set $4
        (local.get $0)
       )
       (local.set $5
        (local.get $2)
       )
       (br $label$2)
      )
      (local.set $3
       (i32.ne
        (local.tee $5
         (i32.add
          (local.get $2)
          (i32.const -1)
         )
        )
        (i32.const 0)
       )
      )
      (br_if $label$3
       (i32.eqz
        (i32.and
         (local.tee $4
          (i32.add
           (local.get $0)
           (i32.const 1)
          )
         )
         (i32.const 3)
        )
       )
      )
      (br_if $label$3
       (i32.eqz
        (local.get $5)
       )
      )
      (br_if $label$2
       (i32.eq
        (i32.load8_u
         (local.get $4)
        )
        (i32.and
         (local.get $1)
         (i32.const 255)
        )
       )
      )
      (local.set $3
       (i32.ne
        (local.tee $5
         (i32.add
          (local.get $2)
          (i32.const -2)
         )
        )
        (i32.const 0)
       )
      )
      (br_if $label$3
       (i32.eqz
        (i32.and
         (local.tee $4
          (i32.add
           (local.get $0)
           (i32.const 2)
          )
         )
         (i32.const 3)
        )
       )
      )
      (br_if $label$3
       (i32.eqz
        (local.get $5)
       )
      )
      (br_if $label$2
       (i32.eq
        (i32.load8_u
         (local.get $4)
        )
        (i32.and
         (local.get $1)
         (i32.const 255)
        )
       )
      )
      (local.set $3
       (i32.ne
        (local.tee $5
         (i32.add
          (local.get $2)
          (i32.const -3)
         )
        )
        (i32.const 0)
       )
      )
      (br_if $label$3
       (i32.eqz
        (i32.and
         (local.tee $4
          (i32.add
           (local.get $0)
           (i32.const 3)
          )
         )
         (i32.const 3)
        )
       )
      )
      (br_if $label$3
       (i32.eqz
        (local.get $5)
       )
      )
      (br_if $label$2
       (i32.eq
        (i32.load8_u
         (local.get $4)
        )
        (i32.and
         (local.get $1)
         (i32.const 255)
        )
       )
      )
      (local.set $4
       (i32.add
        (local.get $0)
        (i32.const 4)
       )
      )
      (local.set $3
       (i32.ne
        (local.tee $5
         (i32.add
          (local.get $2)
          (i32.const -4)
         )
        )
        (i32.const 0)
       )
      )
      (br $label$3)
     )
     (local.set $5
      (local.get $2)
     )
     (local.set $4
      (local.get $0)
     )
    )
    (br_if $label$1
     (i32.eqz
      (local.get $3)
     )
    )
    (block $label$6
     (br_if $label$6
      (i32.eq
       (i32.load8_u
        (local.get $4)
       )
       (i32.and
        (local.get $1)
        (i32.const 255)
       )
      )
     )
     (br_if $label$6
      (i32.lt_u
       (local.get $5)
       (i32.const 4)
      )
     )
     (local.set $0
      (i32.mul
       (i32.and
        (local.get $1)
        (i32.const 255)
       )
       (i32.const 16843009)
      )
     )
     (loop $label$7
      (br_if $label$2
       (i32.and
        (i32.and
         (i32.xor
          (local.tee $2
           (i32.xor
            (i32.load
             (local.get $4)
            )
            (local.get $0)
           )
          )
          (i32.const -1)
         )
         (i32.add
          (local.get $2)
          (i32.const -16843009)
         )
        )
        (i32.const -2139062144)
       )
      )
      (local.set $4
       (i32.add
        (local.get $4)
        (i32.const 4)
       )
      )
      (br_if $label$7
       (i32.gt_u
        (local.tee $5
         (i32.add
          (local.get $5)
          (i32.const -4)
         )
        )
        (i32.const 3)
       )
      )
     )
    )
    (br_if $label$1
     (i32.eqz
      (local.get $5)
     )
    )
   )
   (local.set $2
    (i32.and
     (local.get $1)
     (i32.const 255)
    )
   )
   (loop $label$8
    (block $label$9
     (br_if $label$9
      (i32.ne
       (i32.load8_u
        (local.get $4)
       )
       (local.get $2)
      )
     )
     (return
      (local.get $4)
     )
    )
    (local.set $4
     (i32.add
      (local.get $4)
      (i32.const 1)
     )
    )
    (br_if $label$8
     (local.tee $5
      (i32.add
       (local.get $5)
       (i32.const -1)
      )
     )
    )
   )
  )
  (i32.const 0)
 )
 (func $strnlen (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (select
   (i32.sub
    (local.tee $2
     (call $memchr
      (local.get $0)
      (i32.const 0)
      (local.get $1)
     )
    )
    (local.get $0)
   )
   (local.get $1)
   (local.get $2)
  )
 )
 ;; custom section "producers", size 108
 ;; features section: mutable-globals, bulk-memory, sign-ext
)

