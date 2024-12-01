recursive subroutine qsort(a, first, last)
    implicit none
    integer :: a(*)
    integer :: i, j, x, temp, first, last

    x = a((first + last) / 2)
    i = first
    j = last

    do
        do while (a(i) < x)
            i = i + 1
        end do
        do while (x < a(j))
            j = j - 1
        end do
        if (i >= j) exit
        temp = a(i)
        a(i) = a(j)
        a(j) = temp
        i = i + 1
        j = j - 1
    end do
    
    if (first < i - 1) call qsort(a, first, i - 1)
    if (j + 1 < last) call qsort(a, j + 1, last)
end subroutine qsort

program part1
    implicit none

    integer :: i, sum = 0
    integer :: list1(1000), list2(1000)

    do i = 1, 1000
        read(*,*) list1(i), list2(i)
    end do

    call qsort(list1, 1, 1000)
    call qsort(list2, 1, 1000)

    do i = 1, 1000
        sum = sum + abs(list1(i) - list2(i))
    end do

    print *, sum
end program part1
