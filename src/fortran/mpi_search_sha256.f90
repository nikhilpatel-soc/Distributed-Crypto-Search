
program mpi_search_sha256
    use mpi_f08
    use, intrinsic :: iso_c_binding, only: c_char, c_size_t
    implicit none

    interface
        subroutine c_sha256(d, n, md) bind(c, name="SHA256")
            import :: c_char, c_size_t
            character(kind=c_char), intent(in)  :: d(*)
            integer(c_size_t), value            :: n
            character(kind=c_char), intent(out) :: md(32)
        end subroutine c_sha256
    end interface

    integer, parameter :: TAG_WORK = 100
    integer, parameter :: TAG_RESULT = 200
    integer, parameter :: TAG_MATCH = 250
    integer, parameter :: TAG_FILE_TASK = 260
    integer, parameter :: TAG_STOP = 300
    integer, parameter :: MAX_LINE = 1024
    integer, parameter :: MAX_TEXT = 256
    integer, parameter :: MAX_PATH = 512
    integer, parameter :: I8 = selected_int_kind(18)

    type :: config_type
        character(len=:), allocatable :: runtype
        integer                       :: calib_n = 0
        integer                       :: calib_lmin = 0
        integer                       :: calib_lmax = 0
        integer(kind=I8)              :: calib_chunk_size = 0_I8

        character(len=:), allocatable :: production_mode
        character(len=:), allocatable :: alphabet
        integer                       :: length = 0
        character(len=:), allocatable :: target
        integer(kind=I8)              :: prod_chunk_size = 0_I8

        integer                       :: prefix_hex_chars = 1
        character(len=:), allocatable :: dataset_dir
        integer                       :: dataset_file_count = 0
    end type config_type

    integer :: ierr, rank, world_size
    character(len=MAX_TEXT) :: cfg_path

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, world_size, ierr)

    if (command_argument_count() /= 1) then
        if (rank == 0) print '(a)', 'Usage: mpirun -np <N> ./mpi_search_sha256 mpi_search_sha256.cfg'
        call MPI_Finalize(ierr)
        stop 1
    end if

    call get_command_argument(1, cfg_path)
    cfg_path = trim(cfg_path)

    if (rank == 0) then
        call manager(trim(cfg_path), world_size)
    else
        call worker(trim(cfg_path), rank)
    end if

    call MPI_Finalize(ierr)

contains

    subroutine manager(cfg_path, world_size)
        character(len=*), intent(in) :: cfg_path
        integer, intent(in)          :: world_size

        type(config_type) :: cfg
        integer :: worker_count
        integer :: lval, nruns, idx
        character(len=:), allocatable :: active_alphabet, active_target
        integer(kind=I8) :: total_candidates, tested_total, found_index
        integer, allocatable :: out_n(:), out_l(:)
        integer(kind=I8), allocatable :: out_keyspace(:)
        real(8), allocatable :: out_elapsed(:)
        real(8) :: elapsed
        logical :: found_any
        character(len=MAX_LINE) :: match_candidate, match_hash

        call load_config(cfg_path, cfg)
        call validate_config(cfg)

        worker_count = world_size - 1

        if (worker_count < 1) then
            print '(a)', '[manager] error: at least 2 MPI ranks are required'
            call send_stop_all(world_size)
            return
        end if

        if (cfg%runtype == 'calibration') then
            print '(a)', ''
            print '(a)', '[manager] runtype=calibration'
            print '(a,i0)', '[manager] workers=', worker_count
            print '(a,i0)', '[manager] N=', cfg%calib_n
            print '(a,i0,a,i0)', '[manager] L range=', cfg%calib_lmin, ' to ', cfg%calib_lmax
            print '(a,i0)', '[manager] chunk size=', cfg%calib_chunk_size

            nruns = cfg%calib_lmax - cfg%calib_lmin + 1
            allocate(out_n(nruns), out_l(nruns), out_keyspace(nruns), out_elapsed(nruns))

            active_alphabet = build_sequential_alphabet(cfg%calib_n)

            idx = 0
            do lval = cfg%calib_lmin, cfg%calib_lmax
                idx = idx + 1
                active_target = repeat(active_alphabet(cfg%calib_n:cfg%calib_n), lval)
                total_candidates = int(cfg%calib_n, kind=I8) ** lval

                call execute_bruteforce_search( &
                    world_size         = world_size, &
                    active_alphabet    = active_alphabet, &
                    active_length      = lval, &
                    active_target      = active_target, &
                    total_candidates   = total_candidates, &
                    chunk_size         = cfg%calib_chunk_size, &
                    calibration_base   = cfg%calib_n, &
                    tested_total       = tested_total, &
                    found_any          = found_any, &
                    found_index        = found_index, &
                    match_candidate    = match_candidate, &
                    match_hash         = match_hash, &
                    elapsed            = elapsed &
                )

                out_n(idx) = cfg%calib_n
                out_l(idx) = lval
                out_keyspace(idx) = total_candidates
                out_elapsed(idx) = elapsed
            end do

            print '(a)', ''
            print '(a)', 'N,L,keyspace,elapsed_seconds'
            do idx = 1, nruns
                write(*,'(i0,a,i0,a,i0,a,f12.6)') out_n(idx), ',', out_l(idx), ',', out_keyspace(idx), ',', out_elapsed(idx)
            end do

        else
            print '(a)', ''
            print '(a)', '[manager] runtype=production'
            print '(a,a)', '[manager] production_mode=', trim(cfg%production_mode)
            print '(a,a)', '[manager] target=', trim(cfg%target)
            print '(a,a)', '[manager] target hash=', trim(sha256_text(trim(cfg%target)))
            print '(a,i0)', '[manager] workers=', worker_count

            select case (cfg%production_mode)
            case ('bruteforce')
                print '(a,a)', '[manager] alphabet=', trim(cfg%alphabet)
                print '(a,i0)', '[manager] candidate length=', cfg%length
                print '(a,i0)', '[manager] chunk size=', cfg%prod_chunk_size
                total_candidates = int(len_trim(cfg%alphabet), kind=I8) ** cfg%length
                print '(a,i0)', '[manager] brute-force search space=', total_candidates

                call execute_bruteforce_search( &
                    world_size         = world_size, &
                    active_alphabet    = cfg%alphabet, &
                    active_length      = cfg%length, &
                    active_target      = cfg%target, &
                    total_candidates   = total_candidates, &
                    chunk_size         = cfg%prod_chunk_size, &
                    calibration_base   = -1, &
                    tested_total       = tested_total, &
                    found_any          = found_any, &
                    found_index        = found_index, &
                    match_candidate    = match_candidate, &
                    match_hash         = match_hash, &
                    elapsed            = elapsed &
                )

                print '(a)', ''
                print '(a,f8.3,a,1x,i0,a,i0,a)', '[manager] progress=', &
                    (100.0d0 * dble(tested_total) / dble(total_candidates)), '%', &
                    tested_total, '/', total_candidates, ' candidates completed'
                print '(a)', '[manager] search complete'
                print '(a,i0)', '[manager] total candidates tested=', tested_total
                print '(a,f12.6,a)', '[manager] elapsed time=', elapsed, ' seconds'

                if (found_any) then
                    print '(a)', '[manager] match found'
                    print '(a,a)', '[manager] candidate=', trim(match_candidate)
                    print '(a,a)', '[manager] candidate hash=', trim(match_hash)
                else
                    print '(a)', '[manager] no match found'
                end if

            case ('table_assisted')
                print '(a,i0)', '[manager] prefix_hex_chars=', cfg%prefix_hex_chars
                print '(a,a)', '[manager] dataset_dir=', trim(cfg%dataset_dir)
                print '(a,i0)', '[manager] dataset_file_count=', cfg%dataset_file_count

                call execute_table_assisted_search( &
                    world_size      = world_size, &
                    cfg             = cfg, &
                    tested_total    = tested_total, &
                    found_any       = found_any, &
                    match_candidate = match_candidate, &
                    match_hash      = match_hash, &
                    elapsed         = elapsed &
                )

                print '(a)', ''
                print '(a)', '[manager] table-assisted search complete'
                print '(a,i0)', '[manager] dataset records tested=', tested_total
                print '(a,f12.6,a)', '[manager] elapsed time=', elapsed, ' seconds'

                if (found_any) then
                    print '(a)', '[manager] match found'
                    print '(a,a)', '[manager] candidate=', trim(match_candidate)
                    print '(a,a)', '[manager] candidate hash=', trim(match_hash)
                else
                    print '(a)', '[manager] no match found in dataset bucket files'
                end if
            end select
        end if

        call send_stop_all(world_size)
    end subroutine manager

    subroutine execute_bruteforce_search(world_size, active_alphabet, active_length, active_target, total_candidates, &
                                         chunk_size, calibration_base, tested_total, found_any, found_index, &
                                         match_candidate, match_hash, elapsed)
        integer, intent(in)          :: world_size
        character(len=*), intent(in) :: active_alphabet
        integer, intent(in)          :: active_length
        character(len=*), intent(in) :: active_target
        integer(kind=I8), intent(in) :: total_candidates, chunk_size
        integer, intent(in)          :: calibration_base
        integer(kind=I8), intent(out):: tested_total, found_index
        logical, intent(out)         :: found_any
        character(len=*), intent(out):: match_candidate, match_hash
        real(8), intent(out)         :: elapsed

        integer(kind=I8) :: next_start, next_stop
        integer(kind=I8) :: tested, completed_last_report
        integer :: active_workers, worker, source_rank, status_code
        type(MPI_Status) :: probe_status
        logical :: result_waiting
        real(8) :: start_time, now_time, last_report_time
        integer :: ierr_local
        character(len=MAX_LINE) :: recv_candidate, recv_hash

        next_start = 0_I8
        tested_total = 0_I8
        completed_last_report = -1_I8
        active_workers = 0
        found_any = .false.
        found_index = -1_I8
        match_candidate = ''
        match_hash = ''
        start_time = MPI_Wtime()
        last_report_time = start_time

        do worker = 1, world_size - 1
            if (next_start < total_candidates) then
                next_stop = min(next_start + chunk_size, total_candidates)
                call send_work(worker, next_start, next_stop, calibration_base, active_length)
                next_start = next_stop
                active_workers = active_workers + 1
            end if
        end do

        do while (active_workers > 0)
            call MPI_Iprobe(MPI_ANY_SOURCE, TAG_RESULT, MPI_COMM_WORLD, result_waiting, probe_status, ierr_local)

            if (result_waiting) then
                call recv_result(source_rank, status_code, tested, found_index)
                active_workers = active_workers - 1
                tested_total = tested_total + tested

                if (status_code == 1 .and. .not. found_any) then
                    found_any = .true.
                    call recv_match(source_rank, recv_candidate, recv_hash)
                    match_candidate = trim(recv_candidate)
                    match_hash = trim(recv_hash)
                end if

                if (.not. found_any .and. next_start < total_candidates) then
                    next_stop = min(next_start + chunk_size, total_candidates)
                    call send_work(source_rank, next_start, next_stop, calibration_base, active_length)
                    next_start = next_stop
                    active_workers = active_workers + 1
                end if
            else
                now_time = MPI_Wtime()
                if (now_time - last_report_time >= 1.0d0) then
                    if (tested_total /= completed_last_report) then
                        write(*,'(a,f8.3,a,1x,i0,a,i0,a)') '[manager] progress=', &
                            (100.0d0 * dble(tested_total) / dble(total_candidates)), '%', &
                            tested_total, '/', total_candidates, ' candidates completed'
                        completed_last_report = tested_total
                    end if
                    last_report_time = now_time
                end if
            end if
        end do

        elapsed = MPI_Wtime() - start_time
    end subroutine execute_bruteforce_search

    subroutine execute_table_assisted_search(world_size, cfg, tested_total, found_any, match_candidate, match_hash, elapsed)
        integer, intent(in) :: world_size
        type(config_type), intent(in) :: cfg
        integer(kind=I8), intent(out) :: tested_total
        logical, intent(out) :: found_any
        character(len=*), intent(out) :: match_candidate, match_hash
        real(8), intent(out) :: elapsed

        integer :: worker, active_workers, source_rank, status_code
        integer :: next_task, total_tasks
        integer(kind=I8) :: tested, completed_last_report
        logical :: result_waiting, file_exists
        type(MPI_Status) :: probe_status
        real(8) :: start_time, now_time, last_report_time
        integer :: ierr_local
        character(len=64) :: target_hash, target_prefix
        character(len=MAX_PATH) :: filename
        character(len=MAX_LINE) :: recv_candidate, recv_hash
        logical :: sent_any
        integer(kind=I8) :: found_index_dummy

        target_hash = sha256_text(trim(cfg%target))
        target_prefix = target_hash(1:cfg%prefix_hex_chars)

        tested_total = 0_I8
        completed_last_report = -1_I8
        found_any = .false.
        match_candidate = ''
        match_hash = ''
        active_workers = 0
        next_task = 0
        total_tasks = cfg%dataset_file_count
        start_time = MPI_Wtime()
        last_report_time = start_time

        do worker = 1, world_size - 1
            sent_any = .false.
            do while (next_task < total_tasks .and. .not. sent_any)
                write(filename,'(A,"/bucket_",A,"_rank_",I4.4,".txt")') trim(cfg%dataset_dir), trim(target_prefix), next_task
                inquire(file=trim(filename), exist=file_exists)
                next_task = next_task + 1
                if (file_exists) then
                    call send_file_task(worker, trim(filename))
                    active_workers = active_workers + 1
                    sent_any = .true.
                end if
            end do
        end do

        do while (active_workers > 0)
            call MPI_Iprobe(MPI_ANY_SOURCE, TAG_RESULT, MPI_COMM_WORLD, result_waiting, probe_status, ierr_local)

            if (result_waiting) then
                call recv_result(source_rank, status_code, tested, found_index=found_index_dummy)
                active_workers = active_workers - 1
                tested_total = tested_total + tested

                if (status_code == 1 .and. .not. found_any) then
                    found_any = .true.
                    call recv_match(source_rank, recv_candidate, recv_hash)
                    match_candidate = trim(recv_candidate)
                    match_hash = trim(recv_hash)
                end if

                if (.not. found_any) then
                    sent_any = .false.
                    do while (next_task < total_tasks .and. .not. sent_any)
                        write(filename,'(A,"/bucket_",A,"_rank_",I4.4,".txt")') trim(cfg%dataset_dir), trim(target_prefix), next_task
                        inquire(file=trim(filename), exist=file_exists)
                        next_task = next_task + 1
                        if (file_exists) then
                            call send_file_task(source_rank, trim(filename))
                            active_workers = active_workers + 1
                            sent_any = .true.
                        end if
                    end do
                end if
            else
                now_time = MPI_Wtime()
                if (now_time - last_report_time >= 1.0d0) then
                    if (tested_total /= completed_last_report) then
                        write(*,'(a,1x,i0,a)') '[manager] table-assisted records completed=', tested_total, ''
                        completed_last_report = tested_total
                    end if
                    last_report_time = now_time
                end if
            end if
        end do

        elapsed = MPI_Wtime() - start_time
    end subroutine execute_table_assisted_search

    subroutine worker(cfg_path, rank)
        character(len=*), intent(in) :: cfg_path
        integer, intent(in)          :: rank

        type(config_type) :: cfg
        type(MPI_Status)  :: recv_status
        integer(kind=I8)  :: work_packet(4)
        integer(kind=I8)  :: start_index, stop_index, idx
        integer(kind=I8)  :: tested
        integer           :: base_n, active_length
        integer           :: ierr_local, ios
        logical           :: found
        character(len=:), allocatable :: active_alphabet, active_target, candidate
        character(len=64) :: target_hash, candidate_hash
        character(len=MAX_PATH) :: task_filename
        character(len=MAX_LINE) :: line, file_candidate, file_prefix, file_hash

        call load_config(cfg_path, cfg)
        call validate_config(cfg)

        do
            call MPI_Probe(0, MPI_ANY_TAG, MPI_COMM_WORLD, recv_status, ierr_local)

            select case (recv_status%MPI_TAG)
            case (TAG_STOP)
                call MPI_Recv(work_packet, 4, MPI_INTEGER8, 0, TAG_STOP, MPI_COMM_WORLD, recv_status, ierr_local)
                exit

            case (TAG_WORK)
                call MPI_Recv(work_packet, 4, MPI_INTEGER8, 0, TAG_WORK, MPI_COMM_WORLD, recv_status, ierr_local)

                start_index   = work_packet(1)
                stop_index    = work_packet(2)
                base_n        = int(work_packet(3))
                active_length = int(work_packet(4))

                if (base_n > 0) then
                    active_alphabet = build_sequential_alphabet(base_n)
                    active_target   = repeat(active_alphabet(base_n:base_n), active_length)
                else
                    active_alphabet = cfg%alphabet
                    active_target   = cfg%target
                    active_length   = cfg%length
                end if

                target_hash = sha256_text(trim(active_target))
                tested = 0_I8
                found = .false.

                do idx = start_index, stop_index - 1_I8
                    candidate = index_to_candidate(idx, active_alphabet, active_length)
                    candidate_hash = sha256_text(candidate)
                    tested = tested + 1_I8

                    if (candidate_hash == target_hash) then
                        call send_result(rank, 1, tested, idx)
                        call send_match(rank, trim(candidate), trim(candidate_hash))
                        found = .true.
                        exit
                    end if
                end do

                if (.not. found) call send_result(rank, 0, tested, -1_I8)

            case (TAG_FILE_TASK)
                call MPI_Recv(task_filename, MAX_PATH, MPI_CHARACTER, 0, TAG_FILE_TASK, MPI_COMM_WORLD, recv_status, ierr_local)

                target_hash = sha256_text(trim(cfg%target))
                tested = 0_I8
                found = .false.

                open(unit=77, file=trim(task_filename), status='old', action='read', iostat=ios)
                if (ios /= 0) then
                    write(*,'(a,i0,2a)') '[worker] rank ', rank, ' could not open dataset file ', trim(task_filename)
                    call send_result(rank, 0, tested, -1_I8)
                    cycle
                end if

                do
                    read(77, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    if (len_trim(line) == 0) cycle

                    call parse_dataset_line(trim(line), file_candidate, file_prefix, file_hash)
                    tested = tested + 1_I8

                    if (trim(file_hash) == trim(target_hash)) then
                        call send_result(rank, 1, tested, -1_I8)
                        call send_match(rank, trim(file_candidate), trim(file_hash))
                        found = .true.
                        exit
                    end if
                end do
                close(77)

                if (.not. found) call send_result(rank, 0, tested, -1_I8)
            end select
        end do
    end subroutine worker

    subroutine send_work(dest_rank, start_index, stop_index, calibration_base, active_length)
        integer, intent(in)           :: dest_rank, calibration_base, active_length
        integer(kind=I8), intent(in)  :: start_index, stop_index
        integer(kind=I8) :: packet(4)
        integer :: ierr_local

        packet = [start_index, stop_index, int(calibration_base,kind=I8), int(active_length,kind=I8)]
        call MPI_Send(packet, 4, MPI_INTEGER8, dest_rank, TAG_WORK, MPI_COMM_WORLD, ierr_local)
    end subroutine send_work

    subroutine send_file_task(dest_rank, filename)
        integer, intent(in) :: dest_rank
        character(len=*), intent(in) :: filename
        character(len=MAX_PATH) :: payload
        integer :: ierr_local

        payload = ''
        payload = trim(filename)
        call MPI_Send(payload, MAX_PATH, MPI_CHARACTER, dest_rank, TAG_FILE_TASK, MPI_COMM_WORLD, ierr_local)
    end subroutine send_file_task

    subroutine send_stop_all(world_size)
        integer, intent(in) :: world_size
        integer :: worker
        integer(kind=I8) :: dummy(4)
        integer :: ierr_local

        dummy = [0_I8, 0_I8, 0_I8, 0_I8]
        do worker = 1, world_size - 1
            call MPI_Send(dummy, 4, MPI_INTEGER8, worker, TAG_STOP, MPI_COMM_WORLD, ierr_local)
        end do
    end subroutine send_stop_all

    subroutine send_result(rank, status_code, tested, found_index)
        integer, intent(in)           :: rank, status_code
        integer(kind=I8), intent(in)  :: tested, found_index
        integer(kind=I8) :: payload(4)
        integer :: ierr_local

        payload = [int(status_code,kind=I8), int(rank,kind=I8), tested, found_index]
        call MPI_Send(payload, 4, MPI_INTEGER8, 0, TAG_RESULT, MPI_COMM_WORLD, ierr_local)
    end subroutine send_result

    subroutine recv_result(source_rank, status_code, tested, found_index, dummy_found_index)
        integer, intent(out)           :: source_rank, status_code
        integer(kind=I8), intent(out)  :: tested
        integer(kind=I8), intent(out), optional  :: found_index
        integer(kind=I8), intent(out), optional  :: dummy_found_index

        integer(kind=I8) :: payload(4)
        type(MPI_Status) :: recv_status
        integer :: ierr_local

        call MPI_Recv(payload, 4, MPI_INTEGER8, MPI_ANY_SOURCE, TAG_RESULT, MPI_COMM_WORLD, recv_status, ierr_local)
        source_rank = recv_status%MPI_SOURCE
        status_code = int(payload(1))
        tested      = payload(3)
        if (present(found_index)) found_index = payload(4)
        if (present(dummy_found_index)) dummy_found_index = payload(4)
    end subroutine recv_result

    subroutine send_match(rank, candidate, hash_text)
        integer, intent(in) :: rank
        character(len=*), intent(in) :: candidate, hash_text
        character(len=MAX_LINE) :: payload
        integer :: ierr_local

        payload = ''
        payload = trim(candidate)//'|'//trim(hash_text)
        call MPI_Send(payload, MAX_LINE, MPI_CHARACTER, 0, TAG_MATCH, MPI_COMM_WORLD, ierr_local)
    end subroutine send_match

    subroutine recv_match(source_rank, candidate, hash_text)
        integer, intent(in) :: source_rank
        character(len=*), intent(out) :: candidate, hash_text
        character(len=MAX_LINE) :: payload
        type(MPI_Status) :: recv_status
        integer :: ierr_local, p

        call MPI_Recv(payload, MAX_LINE, MPI_CHARACTER, source_rank, TAG_MATCH, MPI_COMM_WORLD, recv_status, ierr_local)
        p = index(payload, '|')
        if (p <= 0) then
            candidate = ''
            hash_text = ''
        else
            candidate = adjustl(payload(:p-1))
            hash_text = adjustl(payload(p+1:))
        end if
    end subroutine recv_match

    subroutine parse_dataset_line(line, candidate, prefix, full_hash)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: candidate, prefix, full_hash
        integer :: p1, p2
        character(len=MAX_LINE) :: tmp

        candidate = ''
        prefix = ''
        full_hash = ''

        tmp = adjustl(trim(line))
        p1 = index(tmp, ' ')
        if (p1 <= 0) return
        candidate = adjustl(tmp(:p1-1))
        tmp = adjustl(tmp(p1+1:))

        p2 = index(tmp, ' ')
        if (p2 <= 0) then
            prefix = adjustl(trim(tmp))
            return
        end if

        prefix = adjustl(tmp(:p2-1))
        full_hash = adjustl(trim(tmp(p2+1:)))
    end subroutine parse_dataset_line

    subroutine load_config(path, cfg)
        character(len=*), intent(in)   :: path
        type(config_type), intent(out) :: cfg

        integer :: unit, ios, eqpos
        character(len=MAX_LINE) :: line, key, value

        open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write(*,'(a,1x,a)') 'Could not open config file:', trim(path)
            stop 1
        end if

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            line = adjustl(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#') cycle

            eqpos = index(line, '=')
            if (eqpos <= 1) cycle

            key = lowercase(trim(adjustl(line(1:eqpos-1))))
            value = trim(adjustl(line(eqpos+1:)))
            call strip_inline_comment(value)
            call strip_quotes(value)

            select case (trim(key))
            case ('runtype')
                cfg%runtype = lowercase(trim(value))
            case ('n')
                read(value, *, iostat=ios) cfg%calib_n
                if (ios /= 0) stop 'Invalid value for N in config.'
            case ('l')
                call parse_l_range(trim(value), cfg%calib_lmin, cfg%calib_lmax)
            case ('calibration_chunk_size')
                read(value, *, iostat=ios) cfg%calib_chunk_size
                if (ios /= 0) stop 'Invalid value for calibration_chunk_size in config.'
            case ('production_mode')
                cfg%production_mode = lowercase(trim(value))
            case ('alphabet')
                cfg%alphabet = trim(value)
            case ('length')
                read(value, *, iostat=ios) cfg%length
                if (ios /= 0) stop 'Invalid value for length in config.'
            case ('target')
                cfg%target = trim(value)
            case ('production_chunk_size')
                read(value, *, iostat=ios) cfg%prod_chunk_size
                if (ios /= 0) stop 'Invalid value for production_chunk_size in config.'
            case ('prefix_hex_chars')
                read(value, *, iostat=ios) cfg%prefix_hex_chars
                if (ios /= 0) stop 'Invalid value for prefix_hex_chars in config.'
            case ('dataset_dir')
                cfg%dataset_dir = trim(value)
            case ('dataset_file_count')
                read(value, *, iostat=ios) cfg%dataset_file_count
                if (ios /= 0) stop 'Invalid value for dataset_file_count in config.'
            end select
        end do

        close(unit)
    end subroutine load_config

    subroutine validate_config(cfg)
        type(config_type), intent(inout) :: cfg
        integer :: base

        if (.not. allocated(cfg%runtype)) cfg%runtype = 'production'
        if (.not. allocated(cfg%production_mode)) cfg%production_mode = 'bruteforce'

        if (cfg%calib_chunk_size <= 0_I8) cfg%calib_chunk_size = 100000_I8
        if (cfg%prod_chunk_size <= 0_I8) cfg%prod_chunk_size = 100000_I8
        if (cfg%prefix_hex_chars <= 0) cfg%prefix_hex_chars = 1
        if (.not. allocated(cfg%dataset_dir)) cfg%dataset_dir = '.'

        select case (cfg%runtype)
        case ('calibration')
            if (cfg%calib_n < 1) stop 'Calibration N must be positive.'
            if (cfg%calib_lmin < 1 .or. cfg%calib_lmax < cfg%calib_lmin) stop 'Invalid calibration L range.'
            if (cfg%calib_n > 26) stop 'Calibration N currently supports at most 26 letters.'
        case ('production')
            if (.not. allocated(cfg%target)) stop 'Missing target in production config.'

            select case (cfg%production_mode)
            case ('bruteforce')
                if (.not. allocated(cfg%alphabet)) stop 'Missing alphabet in brute-force production config.'
                if (cfg%length <= 0) stop 'length must be positive in brute-force production config.'
                if (len_trim(cfg%target) /= cfg%length) stop 'target length must exactly match length.'
                base = len_trim(cfg%alphabet)
                if (base < 1) stop 'alphabet must not be empty.'
            case ('table_assisted')
                if (cfg%dataset_file_count <= 0) stop 'dataset_file_count must be positive in table-assisted production config.'
                if (cfg%prefix_hex_chars < 1 .or. cfg%prefix_hex_chars > 64) stop 'prefix_hex_chars must be between 1 and 64.'
            case default
                stop 'production_mode must be bruteforce or table_assisted.'
            end select
        case default
            stop 'runtype must be calibration or production.'
        end select
    end subroutine validate_config

    subroutine parse_l_range(text, lmin, lmax)
        character(len=*), intent(in) :: text
        integer, intent(out)         :: lmin, lmax
        integer :: comma_pos, ios
        character(len=MAX_LINE) :: left, right

        comma_pos = index(text, ',')
        if (comma_pos <= 1) stop 'L must be specified as lower,upper'

        left = adjustl(text(1:comma_pos-1))
        right = adjustl(text(comma_pos+1:))

        read(left, *, iostat=ios) lmin
        if (ios /= 0) stop 'Invalid lower bound for L.'

        read(right, *, iostat=ios) lmax
        if (ios /= 0) stop 'Invalid upper bound for L.'
    end subroutine parse_l_range

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

    function build_sequential_alphabet(n) result(alphabet)
        integer, intent(in) :: n
        character(len=:), allocatable :: alphabet
        integer :: i

        allocate(character(len=n) :: alphabet)
        do i = 1, n
            alphabet(i:i) = achar(iachar('a') + i - 1)
        end do
    end function build_sequential_alphabet

    function index_to_candidate(index, alphabet, length) result(candidate)
        integer(kind=I8), intent(in) :: index
        character(len=*), intent(in) :: alphabet
        integer, intent(in)          :: length
        character(len=:), allocatable :: candidate

        integer(kind=I8) :: value, base, remainder
        integer :: pos

        allocate(character(len=length) :: candidate)
        value = index
        base = int(len_trim(alphabet), kind=I8)
        candidate = repeat(' ', length)

        do pos = length, 1, -1
            remainder = mod(value, base)
            value = value / base
            candidate(pos:pos) = alphabet(int(remainder) + 1:int(remainder) + 1)
        end do
    end function index_to_candidate

    function sha256_text(text) result(hex_hash)
        character(len=*), intent(in) :: text
        character(len=64)            :: hex_hash

        character(kind=c_char), allocatable :: c_input(:)
        character(kind=c_char) :: digest(32)
        integer :: n, i, byte_value

        n = len_trim(text)
        allocate(c_input(n))

        do i = 1, n
            c_input(i) = text(i:i)
        end do

        call c_sha256(c_input, int(n, kind=c_size_t), digest)

        do i = 1, 32
            byte_value = iachar(digest(i))
            if (byte_value < 0) byte_value = byte_value + 256
            write(hex_hash(2*i-1:2*i), '(Z2.2)') byte_value
        end do
    end function sha256_text

    function lowercase(text) result(lowered)
        character(len=*), intent(in) :: text
        character(len=len(text))     :: lowered
        integer :: i, code

        lowered = text
        do i = 1, len(text)
            code = iachar(lowered(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) lowered(i:i) = achar(code + 32)
        end do
    end function lowercase

end program mpi_search_sha256
