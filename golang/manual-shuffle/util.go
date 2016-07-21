package main

import (
	"fmt"

	"github.com/bep/inflect"
)

func plural(qty int, singular string) string {
	s := singular
	if qty != 1 {
		s = inflect.Pluralize(s)
	}
	return fmt.Sprintf("%d %s", qty, s)
}
