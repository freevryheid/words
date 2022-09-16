module words

	use stdlib_error
	use stdlib_string_type

	implicit none

	private
	public :: get_word, permutate, word2ichar, ichar2word

	interface copy    ! generic
		module procedure copy_a2s, copy_s2a
	end interface copy

	contains

		pure function copy_a2s(a) result (s)
			! copy char array to string
			character, intent(in) :: a(:)
			character(size(a)) :: s
			integer :: i
			do i = 1, size(a)
				s(i:i) = a(i)
			end do
		end function copy_a2s

		pure function copy_s2a(s) result (a)
			! copy s(1:clen(s)) to char array
			character(*),intent(in) :: s
			character :: a(len(s))
			integer :: i
			do i = 1, len(s)
				a(i) = s(i:i)
			end do
		end function copy_s2a

		! function get_word() result(word)
		!   type(string_type) :: word
		! 	character(len=:), allocatable :: string
		! 	integer :: nargs, arglen
		! 	nargs = command_argument_count()
		! 	if (nargs.ne.1) call error_stop("usage: alias word")
		! 		call get_command_argument(1, length=arglen)
		! 		if (allocated(string)) deallocate(string)
		! 		allocate(character(len=arglen) :: string)
		! 		call get_command_argument(1, string)
		! 		word = string_type(string)
		! end function get_word

		function get_word() result(word)
			character(len=:), allocatable :: word
			integer :: nargs, arglen
			nargs = command_argument_count()
			if (nargs.ne.1) call error_stop("usage: alias word")
				call get_command_argument(1, length=arglen)
				if (allocated(word)) deallocate(word)
				allocate(character(len=arglen) :: word)
				call get_command_argument(1, word)
		end function get_word

		recursive subroutine permutate(e, p)
			integer, intent(in) :: e(:)       ! array of objects
			integer, intent(out) :: p(:,:)    ! permutations of e
			integer  :: n, nfac, i, k, s(size(p,1)/size(e), size(e)-1)
			n = size(e)
			nfac = size(p,1)
			do i=1,n                          ! cases with e(i) in front
				if( n>1 ) call permutate((/e(:i-1), e(i+1:)/), s)
				forall(k=1:nfac/n) p((i-1)*nfac/n+k,:) = (/e(i), s(k,:)/)
			end do
		end subroutine permutate

		! function word2ichar(word) result(res)
		! 	type(string_type), intent(in) :: word
		! 	integer, allocatable :: res(:)
		! 	integer :: i
		! 	allocate(res(len(word)))
		! 	do i = 1, len(word)
		! 		res(i) = ichar(char(word, i))
		! 	end do
		! end function word2ichar

		function word2ichar(word) result(res)
			character(len=:), allocatable, intent(in) :: word
			character :: a(len(word))
			integer :: res(len(word))
			integer :: i
			do i = 1, len(word)
				res(i) = ichar(word(i:i))
			end do
		end function word2ichar

		function ichar2word(ia) result(word)
			integer, intent(in) :: ia(:)
			integer :: i
			character :: a(size(ia))
			character(len=:), allocatable :: word
			do i = 1, size(ia)
				a(i) = char(ia(i))
			end do
			word = copy(a)
		end function ichar2word

end module words
