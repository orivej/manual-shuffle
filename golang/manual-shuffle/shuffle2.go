package main

import (
	"fmt"
	"math"
	"math/rand"
	"strings"
)

// v2result actions encoding:
// - 1..M :: put one card onto specified deck
// - -M..-1 :: put specified deck onto the deck from the previous move
type v2result struct {
	Perm       []int
	Actions    []int
	NHeaps     int
	SquareSide int
}

func v2shuffle(n int) v2result {
	perm, actions, nheaps := rand.Perm(n), make([]int, 0, n), 0
	top := make([]int, n)   // "On top of which heap (1..nheaps) is the card (0..N-1)?"
	bot := make([]int, n)   // "At the bottom of which is it?"
	above := make([]int, n) // "What card is at the top of this bottom of the heap?"
	below := make([]int, n) // "What card is at the bottom of this top of the heap?"
	empty := []int{}        // Empty heap list (1..nheaps).
	for _, target := range perm {
		next, prev := target+1, target-1
		// Select a heap for the card.
		var heap int
		if next < n && top[next] != 0 {
			heap = top[next]
			below[target] = below[next]
		} else {
			if len(empty) > 0 {
				last := len(empty) - 1
				heap, empty = empty[last], empty[:last]
			} else {
				heap = nheaps + 1
				nheaps++
			}
			bot[target] = heap
			below[target] = target
		}
		top[target] = heap
		above[below[target]] = target
		actions = append(actions, heap)
		// Put preceding heap (if exists) on top of this card.
		if prev >= 0 && bot[prev] != 0 {
			top[above[prev]] = heap
			above[below[target]] = above[prev]
			below[above[prev]] = below[target]
			empty = append(empty, bot[prev])
			actions = append(actions, -bot[prev])
		}
	}
	side := 1 + int(math.Sqrt(float64(nheaps-1)))
	return v2result{perm, actions, nheaps, side}
}

func (r v2result) String() string {
	actions := r.Actions
	if r.SquareSide <= 9 {
		actions = make([]int, len(actions))
		for i, action := range r.Actions {
			sign := 1
			if action < 0 {
				sign, action = -1, -action
			}
			y := 1 + (action-1)/r.SquareSide
			x := 1 + (action-1)%r.SquareSide
			actions[i] = sign * (10*x + y)
		}
	}
	s := fmt.Sprint(actions)
	s = strings.Trim(s, "[]")
	s = strings.Replace(s, " -", "&", -1)
	return s
}
