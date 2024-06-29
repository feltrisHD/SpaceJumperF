program SpaceJumperFortran
    implicit none
    integer :: lo, mov, lol, sum, lop, qu
    integer :: cou
    real :: rand_value
    integer :: end

    cou = 0
    sum = 0
    lop = 1

    print *, "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15"
    do
        read(*,*) mov
        cou = cou + 1

        if (qu == mov) then
            print *, "Verloren! You survived: ", sum - 1, " rounds"
            print *, "~"
            print *, "."
            print *, "."
            print *, "."
            print *, "."
            print *, "^"
            read(*,*) lo
            stop
        end if

        select case(mov)
            case (1)
                print *, "~ 2 3 4 5 6 7 8 9 10 11 12 13 14 15"
                sum = sum + lop
            case (2)
                print *, "1 ~ 3 4 5 6 7 8 9 10 11 12 13 14 15"
                sum = sum + lop
            case (3)
                print *, "1 2 ~ 4 5 6 7 8 9 10 11 12 13 14 15"
                sum = sum + lop
            case (4)
                print *, "1 2 3 ~ 5 6 7 8 9 10 11 12 13 14 15"
                sum = sum + lop
            case (5)
                print *, "1 2 3 4 ~ 6 7 8 9 10 11 12 13 14 15"
                sum = sum + lop
            case (6)
                print *, "1 2 3 4 5 ~ 7 8 9 10 11 12 13 14 15"
                sum = sum + lop
            case (7)
                print *, "1 2 3 4 5 6 ~ 8 9 10 11 12 13 14 15"
                sum = sum + lop
            case (8)
                print *, "1 2 3 4 5 6 7 ~ 9 10 11 12 13 14 15"
                sum = sum + lop
            case (9)
                print *, "1 2 3 4 5 6 7 8 ~ 10 11 12 13 14 15"
                sum = sum + lop
            case (10)
                print *, "1 2 3 4 5 6 7 8 9 ~ 11 12 13 14 15"
                sum = sum + lop
            case (11)
                print *, "1 2 3 4 5 6 7 8 9 10 ~ 12 13 14 15"
                sum = sum + lop
            case (12)
                print *, "1 2 3 4 5 6 7 8 9 10 11 ~ 13 14 15"
                sum = sum + lop
            case (13)
                print *, "1 2 3 4 5 6 7 8 9 10 11 12 ~ 14 15"
                sum = sum + lop
            case (14)
                print *, "1 2 3 4 5 6 7 8 9 10 11 12 13 ~ 15"
                sum = sum + lop
            case (15)
                print *, "1 2 3 4 5 6 7 8 9 10 11 12 13 14 ~"
                sum = sum + lop
            
            case default
                print *, "Invalid move!"
        end select

        call random_number(rand_value)
        lol = int(rand_value * 15) + 1

        select case(lol)
            case (1)
                print *, "^ 2 3 4 5 6 7 8 9 10 11 12 13 14 15"
                qu = 1
            case (2)
                print *, "1 ^ 3 4 5 6 7 8 9 10 11 12 13 14 15"
                qu = 2
            case (3)
                print *, "1 2 ^ 4 5 6 7 8 9 10 11 12 13 14 15"
                qu = 3
            case (4)
                print *, "1 2 3 ^ 5 6 7 8 9 10 11 12 13 14 15"
                qu = 4
            case (5)
                print *, "1 2 3 4 ^ 6 7 8 9 10 11 12 13 14 15"
                qu = 5
            case (6)
                print *, "1 2 3 4 5 ^ 7 8 9 10 11 12 13 14 15"
                qu = 6
            case (7)
                print *, "1 2 3 4 5 6 ^ 8 9 10 11 12 13 14 15"
                qu = 7
            case (8)
                print *, "1 2 3 4 5 6 7 ^ 9 10 11 12 13 14 15"
                qu = 8
            case (9)
                print *, "1 2 3 4 5 6 7 8 ^ 10 11 12 13 14 15"
                qu = 9
            case (10)
                print *, "1 2 3 4 5 6 7 8 9 ^ 11 12 13 14 15"
                qu = 10
            case (11)
                print *, "1 2 3 4 5 6 7 8 9 10 ^ 12 13 14 15"
                qu = 11
            case (12)
                print *, "1 2 3 4 5 6 7 8 9 10 11 ^ 13 14 15"
                qu = 12
            case (13)
                print *, "1 2 3 4 5 6 7 8 9 10 11 12 ^ 14 15"
                qu = 13
            case (14)
                print *, "1 2 3 4 5 6 7 8 9 10 11 12 13 ^ 15"
                qu = 14
            case (15)
                print *, "1 2 3 4 5 6 7 8 9 10 11 12 13 14 ^"
                qu = 15
            case default
                print *, "Invalid random number!"
        end select

        if (lol == mov) then
            print *, "Verloren! You survived: ", sum - 1, " rounds"
            print *, "~"
            print *, "."
            print *, "."
            print *, "."
            print *, "."
            print *, "^"
            read(*,*) lo
            stop
        end if
    end do
end program SpaceJumperFortran
