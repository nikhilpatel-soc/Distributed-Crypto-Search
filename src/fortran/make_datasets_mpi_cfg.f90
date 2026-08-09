module openssl_lib
    use, intrinsic :: iso_c_binding
    implicit none

    interface
        function c_sha256(d, n, md) bind(c, name="SHA256") result(ret)
            import :: c_ptr, c_size_t
            type(c_ptr) :: ret
            type(c_ptr), value :: d
            integer(c_size_t), value :: n
            type(c_ptr), value :: md
        end function c_sha256
    end interface
contains
    function get_sha256_hex(input_str) result(hex_str)
        character(len=*), intent(in) :: input_str
        character(len=64) :: hex_str
        character(kind=c_char), dimension(:), allocatable, target :: input_bytes
        integer(c_signed_char), dimension(32), target :: digest
        integer :: i, n, byte_val
        type(c_ptr) :: dummy

        n = len_trim(input_str)
        allocate(input_bytes(n))
        do i = 1, n
            input_bytes(i) = input_str(i:i)
        end do

        dummy = c_sha256(c_loc(input_bytes(1)), int(n, c_size_t), c_loc(digest(1)))
        do i = 1, 32
            byte_val = iand(int(digest(i), kind=4), int(z'FF', kind=4))
            write(hex_str(2*i-1:2*i), '(Z2.2)') byte_val
        end do
    end function get_sha256_hex
end module openssl_lib

module config_mod
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private
    public :: run_config, load_config

    type :: run_config
        character(len=256) :: alphabet = 'abcdefg'
        integer :: key_length = 9
        character(len=256) :: target_plaintext = 'deadfaced'
        integer :: prefix_hex_chars = 1
        integer(int64) :: progress_interval = 1000000_int64
        character(len=256) :: output_dir = '.'
        logical :: write_bucket_dataset = .true.
        logical :: write_all_hashes = .false.
    end type run_config
contains
    subroutine load_config(filename, cfg)
        character(len=*), intent(in) :: filename
        type(run_config), intent(inout) :: cfg
        integer :: unit, ios, p
        character(len=512) :: line, key, value

        open(newunit=unit, file=trim(filename), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write(*,'(A,1X,A)') 'ERROR: could not open cfg file:', trim(filename)
            stop 1
        end if

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line = adjustl(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#') cycle
            p = index(line, '=')
            if (p <= 0) cycle
            key = lower_str(trim(adjustl(line(:p-1))))
            value = trim(adjustl(line(p+1:)))
            call strip_inline_comment(value)
            call strip_quotes(value)
            call apply_kv(cfg, trim(key), trim(value))
        end do
        close(unit)

        if (cfg%key_length <= 0) then
            write(*,*) 'ERROR: key_length must be positive.'
            stop 1
        end if
        if (cfg%prefix_hex_chars <= 0 .or. cfg%prefix_hex_chars > 64) then
            write(*,*) 'ERROR: prefix_hex_chars must be between 1 and 64.'
            stop 1
        end if
        if (len_trim(cfg%alphabet) < 2) then
            write(*,*) 'ERROR: alphabet must contain at least two symbols.'
            stop 1
        end if
        if (len_trim(cfg%target_plaintext) /= cfg%key_length) then
            write(*,*) 'ERROR: target_plaintext length must match key_length.'
            stop 1
        end if
    end subroutine load_config

    subroutine apply_kv(cfg, key, value)
        type(run_config), intent(inout) :: cfg
        character(len=*), intent(in) :: key, value
        integer :: ios
        integer(int64) :: tmp64

        select case (trim(key))
        case ('alphabet')
            cfg%alphabet = trim(value)
        case ('key_length')
            read(value, *, iostat=ios) cfg%key_length
            if (ios /= 0) call bad_value(key, value)
        case ('target_plaintext')
            cfg%target_plaintext = trim(value)
        case ('prefix_hex_chars')
            read(value, *, iostat=ios) cfg%prefix_hex_chars
            if (ios /= 0) call bad_value(key, value)
        case ('progress_interval')
            read(value, *, iostat=ios) tmp64
            if (ios /= 0) call bad_value(key, value)
            cfg%progress_interval = tmp64
        case ('output_dir')
            cfg%output_dir = trim(value)
        case ('write_bucket_dataset')
            cfg%write_bucket_dataset = parse_logical(value)
        case ('write_all_hashes')
            cfg%write_all_hashes = parse_logical(value)
        case default
            write(*,'(A,1X,A)') 'WARNING: ignoring unknown cfg key:', trim(key)
        end select
    end subroutine apply_kv

    logical function parse_logical(value)
        character(len=*), intent(in) :: value
        character(len=256) :: v
        v = lower_str(trim(value))
        select case (trim(v))
        case ('true','t','yes','y','1','.true.')
            parse_logical = .true.
        case ('false','f','no','n','0','.false.')
            parse_logical = .false.
        case default
            write(*,'(A,1X,A)') 'ERROR: invalid logical value:', trim(value)
            stop 1
        end select
    end function parse_logical

    subroutine bad_value(key, value)
        character(len=*), intent(in) :: key, value
        write(*,'(A,1X,A,1X,A)') 'ERROR: invalid value for', trim(key), trim(value)
        stop 1
    end subroutine bad_value

    subroutine strip_inline_comment(s)
        character(len=*), intent(inout) :: s
        integer :: p
        p = index(s, '#')
        if (p > 0) s = trim(s(:p-1))
    end subroutine strip_inline_comment

    subroutine strip_quotes(s)
        character(len=*), intent(inout) :: s
        integer :: n
        s = trim(s)
        n = len_trim(s)
        if (n >= 2) then
            if ((s(1:1) == '"' .and. s(n:n) == '"') .or. (s(1:1) == "'" .and. s(n:n) == "'")) then
                s = s(2:n-1)
            end if
        end if
    end subroutine strip_quotes

    pure function lower_str(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i, c
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) then
                out(i:i) = achar(c + 32)
            else
                out(i:i) = s(i:i)
            end if
        end do
    end function lower_str
end module config_mod

program make_datasets_mpi_cfg
    use mpi_f08
    use openssl_lib
    use config_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64, output_unit
    implicit none

    type(run_config) :: cfg
    integer :: ierr, rank, nprocs, nargs
    character(len=256) :: cfg_file
    character(len=64) :: target_hash, hash_hex
    character(len=64) :: target_bucket_prefix
    character(len=:), allocatable :: alphabet, target_pt, output_dir, candidate
    integer(int64) :: total_space, start_idx, end_idx, idx
    integer(int64) :: local_total, local_bucket_hits, global_total, global_bucket_hits
    integer(int64) :: report_counter, next_report
    real(real64) :: t0, t1, elapsed_local, elapsed_global, rate, pct_local, eta
    logical :: found_exact_local, found_exact_global, do_write_bucket, do_write_hashes
    integer :: found_local_int, found_global_int
    integer :: unit_bucket, unit_hash, ios
    character(len=512) :: bucket_filename, hash_filename
    integer :: key_length, prefix_hex_chars

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)

    cfg_file = 'dataset.cfg'
    nargs = command_argument_count()
    if (nargs >= 1) call get_command_argument(1, cfg_file)

    if (rank == 0) call load_config(trim(cfg_file), cfg)
    call broadcast_config(cfg, rank)

    alphabet = trim(cfg%alphabet)
    key_length = cfg%key_length
    target_pt = trim(cfg%target_plaintext)
    prefix_hex_chars = cfg%prefix_hex_chars
    output_dir = trim(cfg%output_dir)
    do_write_bucket = cfg%write_bucket_dataset
    do_write_hashes = cfg%write_all_hashes

    total_space = int(len_trim(alphabet), int64) ** key_length
    call partition_range(total_space, int(nprocs, int64), int(rank, int64), start_idx, end_idx)

    target_hash = get_sha256_hex(target_pt)
    target_bucket_prefix = target_hash(1:prefix_hex_chars)

    local_total = 0_int64
    local_bucket_hits = 0_int64
    found_exact_local = .false.
    report_counter = 0_int64
    next_report = cfg%progress_interval

    unit_bucket = -1
    unit_hash = -1
    if (do_write_bucket) then
        write(bucket_filename,'(A,"/bucket_",A,"_rank_",I0.4,".txt")') trim(output_dir), trim(target_bucket_prefix), rank
        open(newunit=unit_bucket, file=trim(bucket_filename), status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(*,'(A,I0,2A)') 'ERROR rank ', rank, ' could not open ', trim(bucket_filename)
            call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
        end if
        write(unit_bucket,'(A)') '# candidate hash_prefix full_hash'
    end if
    if (do_write_hashes) then
        write(hash_filename,'(A,"/all_hashes_rank_",I0.4,".txt")') trim(output_dir), rank
        open(newunit=unit_hash, file=trim(hash_filename), status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(*,'(A,I0,2A)') 'ERROR rank ', rank, ' could not open ', trim(hash_filename)
            call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
        end if
        write(unit_hash,'(A)') '# candidate hash_prefix full_hash'
    end if

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    call cpu_time(t0)

    if (rank == 0) then
        print *, 'MPI dataset generation started.'
        print *, 'Cfg file           : ', trim(cfg_file)
        print *, 'Alphabet           : ', trim(alphabet)
        print *, 'Key length         : ', key_length
        print *, 'Total search size  : ', total_space
        print *, 'MPI ranks          : ', nprocs
        print *, 'Target plaintext   : ', trim(target_pt)
        print *, 'Target SHA-256     : ', target_hash
        print *, 'Prefix hex chars   : ', prefix_hex_chars
        print *, 'Target bucket      : ', trim(target_bucket_prefix)
        print *, 'Output dir         : ', trim(output_dir)
        print *, 'Write bucket file  : ', do_write_bucket
        print *, 'Write full hashes  : ', do_write_hashes
        print *, 'Progress interval  : ', cfg%progress_interval, ' per rank'
        call flush(output_unit)
    end if

    allocate(character(len=key_length) :: candidate)

    do idx = start_idx, end_idx
        call index_to_candidate(idx, alphabet, key_length, candidate)
        hash_hex = get_sha256_hex(candidate)

        local_total = local_total + 1_int64
        report_counter = report_counter + 1_int64

        if (hash_hex(1:prefix_hex_chars) == target_bucket_prefix(1:prefix_hex_chars)) then
            local_bucket_hits = local_bucket_hits + 1_int64
            if (do_write_bucket) then
                write(unit_bucket,'(A,1X,A,1X,A)') candidate, hash_hex(1:prefix_hex_chars), hash_hex
            end if
        end if

        if (do_write_hashes) then
            write(unit_hash,'(A,1X,A,1X,A)') candidate, hash_hex(1:prefix_hex_chars), hash_hex
        end if

        if (.not. found_exact_local) then
            if (candidate == target_pt) found_exact_local = .true.
        end if

        if (report_counter >= next_report) then
            call cpu_time(t1)
            elapsed_local = t1 - t0
            if (elapsed_local > 0.0_real64) then
                rate = real(local_total, real64) / elapsed_local
            else
                rate = 0.0_real64
            end if

            if (end_idx >= start_idx) then
                pct_local = 100.0_real64 * real(local_total, real64) / real(end_idx - start_idx + 1_int64, real64)
            else
                pct_local = 100.0_real64
            end if

            if (rate > 0.0_real64) then
                eta = real((end_idx - idx), real64) / rate
            else
                eta = -1.0_real64
            end if

            write(output_unit,'(A,I0)') 'Rank                         : ', rank
            write(output_unit,'(A,I0,A,I0)') 'Assigned global index range  : ', start_idx, ' to ', end_idx
            write(output_unit,'(A,I0)') 'Local candidates checked     : ', local_total
            write(output_unit,'(A,F8.3,A)') 'Local percent complete       : ', pct_local, '%'
            write(output_unit,'(A,F12.3)') 'Elapsed seconds              : ', elapsed_local
            write(output_unit,'(A,F12.3)') 'Local hash rate (cand/s)     : ', rate
            if (eta >= 0.0_real64) then
                write(output_unit,'(A,F12.3)') 'Estimated seconds remaining  : ', eta
            else
                write(output_unit,'(A)') 'Estimated seconds remaining  : undefined'
            end if
            write(output_unit,'(A,I0)') 'Current global index         : ', idx
            write(output_unit,'(A,A)') 'Current candidate            : ', candidate
            write(output_unit,'(A)') ''
            call flush(output_unit)
            next_report = next_report + cfg%progress_interval
        end if
    end do

    call cpu_time(t1)
    elapsed_local = t1 - t0

    call MPI_Reduce(local_total,       global_total,       1, MPI_INTEGER8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    call MPI_Reduce(local_bucket_hits, global_bucket_hits, 1, MPI_INTEGER8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    call MPI_Reduce(elapsed_local,     elapsed_global,     1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, ierr)

    found_local_int = 0
    if (found_exact_local) found_local_int = 1
    call MPI_Reduce(found_local_int, found_global_int, 1, MPI_INTEGER, MPI_MAX, 0, MPI_COMM_WORLD, ierr)
    found_exact_global = (found_global_int /= 0)

    if (unit_bucket > 0) close(unit_bucket)
    if (unit_hash > 0) close(unit_hash)

    if (rank == 0) then
        print *
        print *, '--- MPI Summary ---'
        print *, 'Global search size          : ', global_total
        print *, 'Target bucket hit count     : ', global_bucket_hits
        print *, 'Elapsed seconds (max rank)  : ', elapsed_global
        if (elapsed_global > 0.0_real64) then
            print *, 'Aggregate effective rate    : ', real(global_total, real64) / elapsed_global
        else
            print *, 'Aggregate effective rate    : undefined'
        end if
        if (global_bucket_hits > 0_int64) then
            print *, 'Reduction ratio             : ', real(global_total, real64) / real(global_bucket_hits, real64), 'x'
        else
            print *, 'Reduction ratio             : undefined (no matches in target bucket)'
        end if
        print *, 'Exact target in keyspace    : ', found_exact_global
        if (do_write_bucket) print *, 'Bucket dataset files        : ', 'bucket_'//trim(target_bucket_prefix)//'_rank_####.txt'
        if (do_write_hashes) print *, 'Full hash files             : ', 'all_hashes_rank_####.txt'
        call flush(output_unit)
    end if

    deallocate(candidate)
    call MPI_Finalize(ierr)
contains
    subroutine broadcast_config(cfg, rank)
        type(run_config), intent(inout) :: cfg
        integer, intent(in) :: rank
        integer :: ierr
        integer :: ints(2)
        integer(int64) :: pinterval
        logical :: logs(2)

        ints = 0
        pinterval = 0_int64
        logs = .false.

        if (rank == 0) then
            ints = [cfg%key_length, cfg%prefix_hex_chars]
            pinterval = cfg%progress_interval
            logs = [cfg%write_bucket_dataset, cfg%write_all_hashes]
        end if

        call MPI_Bcast(cfg%alphabet, len(cfg%alphabet), MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
        call MPI_Bcast(cfg%target_plaintext, len(cfg%target_plaintext), MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
        call MPI_Bcast(cfg%output_dir, len(cfg%output_dir), MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
        call MPI_Bcast(ints, 2, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
        call MPI_Bcast(pinterval, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr)
        call MPI_Bcast(logs, 2, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)

        if (rank /= 0) then
            cfg%key_length = ints(1)
            cfg%prefix_hex_chars = ints(2)
            cfg%progress_interval = pinterval
            cfg%write_bucket_dataset = logs(1)
            cfg%write_all_hashes = logs(2)
        end if
    end subroutine broadcast_config

    subroutine partition_range(total_n, nworkers, myrank, first_idx, last_idx)
        integer(int64), intent(in)  :: total_n, nworkers, myrank
        integer(int64), intent(out) :: first_idx, last_idx
        integer(int64) :: base, extra

        base = total_n / nworkers
        extra = mod(total_n, nworkers)

        if (myrank < extra) then
            first_idx = myrank * (base + 1_int64)
            last_idx  = first_idx + base
        else
            first_idx = extra * (base + 1_int64) + (myrank - extra) * base
            last_idx  = first_idx + base - 1_int64
        end if
    end subroutine partition_range

    subroutine index_to_candidate(idx, chars, out_len, out_str)
        integer(int64), intent(in) :: idx
        character(len=*), intent(in) :: chars
        integer, intent(in) :: out_len
        character(len=out_len), intent(out) :: out_str
        integer(int64) :: work_idx, base
        integer :: pos, digit

        work_idx = idx
        base = int(len_trim(chars), int64)
        out_str = repeat(chars(1:1), out_len)

        do pos = out_len, 1, -1
            digit = int(mod(work_idx, base)) + 1
            out_str(pos:pos) = chars(digit:digit)
            work_idx = work_idx / base
        end do
    end subroutine index_to_candidate
end program make_datasets_mpi_cfg
