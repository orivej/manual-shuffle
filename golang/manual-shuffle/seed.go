package main

import (
	cryptorand "crypto/rand"
	"math/big"
	"math/rand"

	"github.com/orivej/e"
)

func seed() {
	n, err := cryptorand.Int(cryptorand.Reader, big.NewInt(1<<31-1))
	e.Exit(err)
	rand.Seed(n.Int64())
}
