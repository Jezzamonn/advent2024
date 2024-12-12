package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Scan()

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	line := scanner.Text()

	words := strings.Fields(line)
	nums := make([]int, len(words))
	for i, word := range words {
		num, err := strconv.Atoi(word)
		if err != nil {
			panic(err)
		}
		nums[i] = num
	}

	fmt.Println(nums)

	numSteps := 25
	for i := 0; i < numSteps; i++ {
		nums = step(nums)
		fmt.Println(nums)
	}
	fmt.Println(nums)
	fmt.Println(len(nums))
}

func step(nums []int) []int {
	next := make([]int, 0, 2*len(nums))
	for _, num := range nums {
		if num == 0 {
			next = append(next, 1)
			continue
		}
		digits := numDigits(num)
		if digits%2 == 0 {
			first, second := splitDigits(num, digits/2)
			next = append(next, first, second)
		} else {
			next = append(next, 2024*num)
		}
	}
	return next
}

func numDigits(num int) int {
	if num == 0 {
		return 1
	}
	count := 0
	for num != 0 {
		num /= 10
		count++
	}
	return count
}

func splitDigits(num int, splitAt int) (int, int) {
	power := 1
	for i := 0; i < splitAt; i++ {
		power *= 10
	}
	return num / power, num % power
}
