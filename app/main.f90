program main

	use words
	use stdlib_io
	! use stdlib_strings
	use stdlib_string_type
	! use stdlib_stringlist_type
	use stdlib_hashmaps
	use stdlib_hashmap_wrappers

	implicit none

	! integer :: i
	integer :: u
	integer :: iostat

	! type(string_type) :: word
	! character(len=:), allocatable :: word
	integer :: i, j, n
	character(len=:), allocatable :: word
	type(string_type) :: tmp
	! type(stringlist_type)       :: wordlist
	! type(stringlist_index_type) :: idx
	! integer                     :: u, iostat
	! idx = bidx(1)
	! call getline(word, iostat)
	integer, allocatable :: e(:) ! allocatable integer array
	integer, allocatable :: p(:,:) ! permutations of e

	type(chaining_hashmap_type) :: map
	! type(open_hashmap_type) :: map
	type(key_type) :: key

	logical :: mapped

	word = get_word()
	e = word2ichar(word)

	n = len(word)
	allocate(p(product((/(i, i = 1, n)/)), n))
	call permutate(e, p)





	! print *, e
	! do i = 1, len(word)
	! 	print *, char(word, i)
	! end do

	! write(*,'(a)') word
	! u = open("wordlist", iostat=iostat)
	! do while (iostat.eq.0)
	! 	call getline(u, word, iostat)
	! 	if (starts_with(word, "z")) then
	! 		call wordlist%insert_at(idx, word)
	! 	end if
	! end do
	! close(u)
	! print *, wordlist%len()



	call map%init(fnv_1_hasher)

  ! add wordlist to hash map
  ! i = 0
	u = open("wordlist", iostat=iostat)
	do while (iostat.eq.0)
		call getline(u, tmp, iostat)
		word = char(tmp) ! stdlib hash not support string_type
		call set(key, word)
		call map%map_entry(key)
		! i = i + 1
		! print *, i, tmp
	end do
	close(u)

	! test
	! call set(key, "zorn")
	! call map%key_test(key, mapped)
	! print *, "mapped: ", mapped



	do j = 1, size(p, 1)
		word = ichar2word(p(j, :))
		call set(key, word)
		call map%key_test(key, mapped)
		if (mapped) print *, word
	end do





end program main
