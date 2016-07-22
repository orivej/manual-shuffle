package main

import (
	"fmt"
	"strconv"

	"github.com/gotk3/gotk3/cairo"
	"github.com/gotk3/gotk3/gtk"
	"github.com/orivej/e"
)

const (
	title    = "Manual shuffle"
	cellSize = 22
)

const css = `
.fontBig {
; font-family: 'Open Sans Condensed Light'
; font-size: 22pt
}
`

const (
	cellEmpty = iota
	cellMerged
	cellOccupied
	cellPrevious
	cellCurrent
)

var cellColor = [][3]float64{
	{1, 1, 1},
	{0, 0.2, 0.7},
	{0.7, 0.8, 0.8},
	{0.4, 0.5, 0.5},
	{1, 0.1, 0.1},
}

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
	side := r.SquareSide
	cells := make([]int, 1+side*side)
	tail := w.fieldBuf.GetEndIter()
	for i := 0; i < len(r.Actions); i++ {
		for k, cell := range cells {
			if cell != cellEmpty && cell != cellOccupied {
				cells[k]--
			}
		}

		cells[r.Actions[i]] = cellCurrent

		if i+1 < len(r.Actions) && r.Actions[i+1] < 0 {
			i++
			cells[-r.Actions[i]] = cellMerged
		}

		area, err := gtk.DrawingAreaNew()
		e.Exit(err)
		areaSize := 4 + side*cellSize
		area.SetSizeRequest(areaSize, areaSize)
		setStyle(area, "borderSolid")
		areaCells := make([]int, len(cells))
		copy(areaCells, cells)
		area.Connect("draw", func(area *gtk.DrawingArea, cr *cairo.Context) {
			drawTable(area, cr, side, areaCells)
		})
		w.field.AddChildAtAnchor(area, w.fieldBuf.CreateChildAnchor(tail))
	}
}

func drawTable(area *gtk.DrawingArea, cr *cairo.Context, side int, cells []int) {
	u := float64(cellSize)
	w := area.GetAllocatedWidth()
	h := area.GetAllocatedHeight()
	bg := cellColor[cellPrevious]
	cr.SetSourceRGB(bg[0], bg[1], bg[2])
	cr.Rectangle(1, 1, float64(w-2), float64(h-2))
	cr.Fill()
	for k, cell := range cells[1:] {
		color := cellColor[cell]
		x, y := float64(k/side), float64(k%side)
		cr.SetSourceRGB(color[0], color[1], color[2])
		cr.Rectangle(2+x*u, 2+y*u, u, u)
		cr.Fill()
	}
}
