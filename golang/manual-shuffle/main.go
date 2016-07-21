package main

import (
	"fmt"
	"strconv"

	"github.com/gotk3/gotk3/gtk"
	"github.com/orivej/e"
)

const (
	title  = "Manual shuffle"
	bigCSS = `
* {
; font-family: 'Open Sans Condensed Light'
; font-size: 22pt
}`
)

var w = struct {
	count           *gtk.SpinButton
	fieldBuf        *gtk.TextBuffer
	showPermutation *gtk.CheckButton
}{}

var state = struct {
	v2result
}{}

func main() {
	gtk.Init(nil)
	uiSetup()
	gtk.Main()
}

func uiSetup() {
	var err error
	w.count, err = gtk.SpinButtonNewWithRange(1, 999, 1)
	e.Exit(err)
	w.count.SetValue(1)

	shuffle, err := gtk.ButtonNewWithLabel("Shuffle")
	e.Exit(err)

	v2, err := gtk.CheckButtonNewWithLabel("v2")
	e.Exit(err)
	v2.SetActive(true)

	w.showPermutation, err = gtk.CheckButtonNewWithMnemonic("show _permutation")
	e.Exit(err)

	field, err := gtk.TextViewNew()
	e.Exit(err)
	field.SetAcceptsTab(false)
	field.SetWrapMode(gtk.WRAP_WORD)
	w.fieldBuf, err = field.GetBuffer()
	e.Exit(err)

	style, err := gtk.CssProviderNew()
	e.Exit(err)
	err = style.LoadFromData(bigCSS)
	e.Exit(err)
	fieldStyleCtx, err := field.GetStyleContext()
	e.Exit(err)
	fieldStyleCtx.AddProvider(style, gtk.STYLE_PROVIDER_PRIORITY_USER+1)

	w.count.Connect("changed", uiShuffle)
	w.count.Connect("activate", uiShuffle)
	shuffle.Connect("clicked", uiShuffle)
	w.showPermutation.Connect("toggled", uiReset)
	uiShuffle()

	panel, err := gtk.BoxNew(gtk.ORIENTATION_HORIZONTAL, 0)
	e.Exit(err)
	panel.Add(w.count)
	panel.Add(shuffle)
	// panel.Add(v2)
	panel.Add(w.showPermutation)

	fieldWindow, err := gtk.ScrolledWindowNew(nil, nil)
	e.Exit(err)
	fieldWindow.Add(field)

	layout, err := gtk.BoxNew(gtk.ORIENTATION_VERTICAL, 0)
	e.Exit(err)
	layout.Add(panel)
	layout.PackStart(fieldWindow, true, true, 0)

	window, err := gtk.WindowNew(gtk.WINDOW_TOPLEVEL)
	e.Exit(err)
	window.SetTitle(title)
	window.Add(layout)
	window.Connect("destroy", gtk.MainQuit)
	window.ShowAll()
}

func uiShuffle() {
	text, err := w.count.GetText()
	e.Exit(err)
	n, err := strconv.Atoi(text)
	if err != nil || n < 1 {
		return
	}
	state.v2result = v2shuffle(n)
	uiReset()
}

func uiReset() {
	r := state.v2result
	s := plural(r.NHeaps, "heap")
	if r.SquareSide != 0 {
		s += fmt.Sprintf(" in a %d×%d layout", r.SquareSide, r.SquareSide)
	}
	s += fmt.Sprintf(". %s:\n%s.", plural(len(r.Actions), "action"), r)
	if w.showPermutation.GetActive() {
		s += fmt.Sprintf("\nPermutation: %v.", r.Perm)
	}
	w.fieldBuf.SetText(s)
}
