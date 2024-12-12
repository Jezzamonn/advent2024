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

	numSteps := 75
	sum := 0
	for _, num := range nums {
		sum += numStones(num, numSteps)
	}

	fmt.Println(sum)
}

type CacheKey struct {
	num      int
	numSteps int
}

var cache = make(map[CacheKey]int)

func numStones(num int, numSteps int) int {
	if numSteps == 0 {
		return 1
	}

	key := CacheKey{num, numSteps}
	if result, ok := cache[key]; ok {
		return result
	}

	if num == 0 {
		cache[key] = numStones(1, numSteps-1)
	} else {
		digits := numDigits(num)
		if digits%2 == 0 {
			first, second := splitDigits(num, digits/2)
			cache[key] = numStones(first, numSteps-1) + numStones(second, numSteps-1)
		} else {
			cache[key] = numStones(2024*num, numSteps-1)
		}
	}

	return cache[key]
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
