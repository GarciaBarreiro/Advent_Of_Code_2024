program part1
    implicit none

    integer :: i, j
    integer :: length = 1000, width = 9
    integer, allocatable :: reports(:, :)
    integer :: safe = 0, prev = 0, desc, curr
    character(len=50) :: line

    allocate(reports(length, width))

    do i = 1, length
        read(*, '(A)') line
        j = count(transfer(line, 'a', len(line)) == " ")
        do j = 1, len_trim(line)
            if (line(j:j) == " ")  prev = prev + 1
        end do
        read(line, *) reports(i, 1:prev+1)
        prev = 0
    end do

    do i = 1, length
        prev = reports(i, 1)
        if (reports(i, 2) < prev) then
            desc = 1
        else
            desc = 0
        end if
        curr = 1
        do j = 2, width
            if (reports(i, j) == 0) then
                exit
            else if (reports(i,j) == prev) then
                curr = 0
                exit
            else if (reports(i, j) < prev) then
                if (desc == 0 .or. prev - reports(i, j) > 3) then
                    curr = 0
                    exit
                end if
            else if (reports(i, j) > prev) then
                if (desc == 1 .or. reports(i, j) - prev > 3) then
                    curr = 0
                    exit
                end if
            end if
            prev = reports(i, j)
        end do
        if (curr == 1) safe = safe + 1
    end do

    write(*, *) safe
end program part1
