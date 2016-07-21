package main

import (
	"fmt"

	"github.com/gotk3/gotk3/gtk"
	"github.com/orivej/e"
)

const title = "Manual shuffle"

func main() {
	gtk.Init(nil)
	uiSetup()
	gtk.Main()
}

func uiSetup() {
	count, err := gtk.SpinButtonNewWithRange(2, 999, 1)
	e.Exit(err)
	count.SetValue(2)

	shuffle, err := gtk.ButtonNewWithLabel("Manual shuffle")
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
	err = style.LoadFromData("* {font-family: Anivers; font-size: 22pt}")
	e.Exit(err)
	fieldStyleCtx, err := field.GetStyleContext()
	e.Exit(err)
	fieldStyleCtx.AddProvider(style, gtk.STYLE_PROVIDER_PRIORITY_USER+1)

	cb := func() { uiShuffle(count, fieldBuf) }
	count.Connect("activate", cb)
	shuffle.Connect("clicked", cb)

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
	r := v2shuffle(count.GetValueAsInt())
	sides := ""
	if r.SquareSide != 0 {
		sides = fmt.Sprintf(" in a %d×%d layout", r.SquareSide, r.SquareSide)
	}
	fieldBuf.SetText(fmt.Sprintf("%d heaps%s.\n%d actions: %s.\nPermutation: %v.",
		r.NHeaps, sides, len(r.Actions), r, r.Perm))
}
