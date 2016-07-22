package main

import (
	"fmt"
	"strconv"

	"github.com/gotk3/gotk3/gtk"
	"github.com/orivej/e"
)

type Coord struct {
	X, Y int
}

const (
	title = "Manual shuffle"
)

const css = `
.fontBig {
; font-family: 'Open Sans Condensed Light'
; font-size: 22pt
}

.borderSolid {
; border-width: 1px
; border-style: solid
; border-color: lightgrey
}

.bgRed { background-color: red }
.bgBlue { background-color: blue }
.bgGrey { background-color: grey }
.bgLightGrey { background-color: lightgrey }
`

var cssProvider *gtk.CssProvider

var w = struct {
	count           *gtk.SpinButton
	field           *gtk.TextView
	fieldBuf        *gtk.TextBuffer
	fieldWindow     *gtk.ScrolledWindow
	showActionsText *gtk.CheckButton
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

	cssProvider, err = gtk.CssProviderNew()
	e.Exit(err)
	err = cssProvider.LoadFromData(css)
	e.Exit(err)

	w.count, err = gtk.SpinButtonNewWithRange(1, 999, 1)
	e.Exit(err)
	w.count.SetValue(1)

	shuffle, err := gtk.ButtonNewWithLabel("Shuffle")
	e.Exit(err)

	v2, err := gtk.CheckButtonNewWithLabel("v2")
	e.Exit(err)
	v2.SetActive(true)

	w.showActionsText, err = gtk.CheckButtonNewWithMnemonic("show actions _text")
	e.Exit(err)
	w.showPermutation, err = gtk.CheckButtonNewWithMnemonic("show _permutation")
	e.Exit(err)

	w.count.Connect("changed", uiShuffle)
	w.count.Connect("activate", uiShuffle)
	shuffle.Connect("clicked", uiShuffle)
	w.showActionsText.Connect("toggled", uiReset)
	w.showPermutation.Connect("toggled", uiReset)

	panel, err := gtk.BoxNew(gtk.ORIENTATION_HORIZONTAL, 0)
	e.Exit(err)
	panel.Add(w.count)
	panel.Add(shuffle)
	// panel.Add(v2)
	panel.Add(w.showActionsText)
	panel.Add(w.showPermutation)

	w.fieldWindow, err = gtk.ScrolledWindowNew(nil, nil)
	e.Exit(err)

	layout, err := gtk.BoxNew(gtk.ORIENTATION_VERTICAL, 0)
	e.Exit(err)
	layout.Add(panel)
	layout.PackStart(w.fieldWindow, true, true, 0)

	window, err := gtk.WindowNew(gtk.WINDOW_TOPLEVEL)
	e.Exit(err)
	window.SetTitle(title)
	window.Add(layout)
	window.Connect("destroy", gtk.MainQuit)
	uiShuffle()
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
	if w.field != nil {
		w.field.Destroy()
	}

	var err error
	w.field, err = gtk.TextViewNew()
	e.Exit(err)
	w.field.SetAcceptsTab(false)
	w.field.SetWrapMode(gtk.WRAP_WORD)
	w.fieldBuf, err = w.field.GetBuffer()
	e.Exit(err)
	setStyle(w.field, "fontBig")
	w.fieldWindow.Add(w.field)

	r := state.v2result
	textual := w.showActionsText.GetActive() || r.SquareSide == 0
	s := plural(r.NHeaps, "heap")
	if r.SquareSide != 0 {
		s += fmt.Sprintf(" in a %d×%d layout", r.SquareSide, r.SquareSide)
	}
	s += fmt.Sprintf(". %s:\n", plural(len(r.Actions), "action"))
	if textual {
		s += fmt.Sprintf("%s.\n", r.String())
	}
	if w.showPermutation.GetActive() {
		s += fmt.Sprintf("Permutation: %v.\n", r.Perm)
	}
	w.fieldBuf.SetText(s)
	if !textual {
		addGraphicalActions()
	}
	w.field.ShowAll()
}

func addGraphicalActions() {
	r := state.v2result
	newCell := func(class string) *gtk.DrawingArea {
		cell, err := gtk.DrawingAreaNew()
		e.Exit(err)
		cell.SetSizeRequest(20, 20)
		if class != "" {
			setStyle(cell, class)
		}
		return cell
	}
	actionXY := func(action int) Coord {
		return Coord{(action - 1) / r.SquareSide, (action - 1) % r.SquareSide}
	}
	occupied := map[Coord]bool{}
	prevAction := 0
	tail := w.fieldBuf.GetEndIter()
	for i := 0; i < len(r.Actions); i++ {
		table, err := gtk.GridNew()
		e.Exit(err)
		table.SetBorderWidth(1)
		setStyle(table, "borderSolid")
		addCell := func(class string, c Coord) {
			if _, err := table.GetChildAt(c.X, c.Y); err != nil {
				table.Attach(newCell(class), c.X, c.Y, 1, 1)
			}
		}

		action := r.Actions[i]
		coord := actionXY(action)
		addCell("bgRed", coord)
		occupied[coord] = true

		if i+1 < len(r.Actions) && r.Actions[i+1] < 0 {
			i++
			coord := actionXY(-r.Actions[i])
			addCell("bgBlue", coord)
			delete(occupied, coord)
		}

		if prevAction != 0 {
			addCell("bgGrey", actionXY(prevAction))
		}
		prevAction = action

		for coord := range occupied {
			addCell("bgLightGrey", coord)
		}

		for k := 0; k < r.SquareSide; k++ {
			addCell("", Coord{k, k})
		}

		w.field.AddChildAtAnchor(table, w.fieldBuf.CreateChildAnchor(tail))
	}
}
