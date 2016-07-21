package main

import (
	"fmt"
	"strconv"

	"github.com/gotk3/gotk3/gtk"
	"github.com/orivej/e"
)

const (
	title  = "Manual shuffle"
	bigCSS = "* {font-family: 'Open Sans Condensed Light'; font-size: 22pt}"
)

func main() {
	gtk.Init(nil)
	uiSetup()
	gtk.Main()
}

func uiSetup() {
	count, err := gtk.SpinButtonNewWithRange(2, 999, 1)
	e.Exit(err)
	count.SetValue(2)

	shuffle, err := gtk.ButtonNewWithLabel("Shuffle")
	e.Exit(err)

	v2, err := gtk.CheckButtonNewWithLabel("v2")
	e.Exit(err)
	v2.SetActive(true)

	field, err := gtk.TextViewNew()
	e.Exit(err)
	field.SetAcceptsTab(false)
	field.SetWrapMode(gtk.WRAP_WORD)
	fieldBuf, err := field.GetBuffer()
	e.Exit(err)

	style, err := gtk.CssProviderNew()
	e.Exit(err)
	err = style.LoadFromData(bigCSS)
	e.Exit(err)
	fieldStyleCtx, err := field.GetStyleContext()
	e.Exit(err)
	fieldStyleCtx.AddProvider(style, gtk.STYLE_PROVIDER_PRIORITY_USER+1)

	cb := func() { uiShuffle(count, fieldBuf) }
	count.Connect("changed", cb)
	count.Connect("activate", cb)
	shuffle.Connect("clicked", cb)
	cb()

	panel, err := gtk.BoxNew(gtk.ORIENTATION_HORIZONTAL, 0)
	e.Exit(err)
	panel.Add(count)
	panel.Add(shuffle)
	// panel.Add(v2)

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

func uiShuffle(count *gtk.SpinButton, fieldBuf *gtk.TextBuffer) {
	text, err := count.GetText()
	e.Exit(err)
	n, err := strconv.Atoi(text)
	if err != nil || n < 2 {
		return
	}
	r := v2shuffle(n)
	sides := ""
	if r.SquareSide != 0 {
		sides = fmt.Sprintf(" in a %d×%d layout", r.SquareSide, r.SquareSide)
	}
	fieldBuf.SetText(fmt.Sprintf("%d heaps%s. %d actions:\n%s.\nPermutation: %v.",
		r.NHeaps, sides, len(r.Actions), r, r.Perm))
}
