program part2
    implicit none

    integer :: i, j, sim = 0, count
    integer :: list1(1000), list2(1000)

    do i = 1, 1000
        read(*,*) list1(i), list2(i)
    end do

    do i = 1, 1000
        count = 0
        do j = 1, 1000
            if (list1(i) == list2(j)) count = count + 1
        end do
        sim = sim + list1(i) * count
    end do

    print *, sim
end program part2
