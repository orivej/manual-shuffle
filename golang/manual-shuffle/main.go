package main

import (
	"fmt"
	"strconv"

	"github.com/gotk3/gotk3/cairo"
	"github.com/gotk3/gotk3/gtk"
	"github.com/orivej/e"
)

const (
	title = "Manual shuffle"
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
	cellCurrentToMerge
)

var cellColor = [][3]float64{
	{1, 1, 1},
	{0, 0.2, 0.7},
	{0.7, 0.8, 0.8},
	{0.4, 0.5, 0.5},
	{1, 0.4, 0.4},
	{1, 0.1, 0.1},
}

var cssProvider *gtk.CssProvider

var w = struct {
	count, cellSize *gtk.SpinButton
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
	seed()
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

	w.showActionsText, err = gtk.CheckButtonNewWithMnemonic("Show actions _text")
	e.Exit(err)
	w.showPermutation, err = gtk.CheckButtonNewWithMnemonic("Show _permutation")
	e.Exit(err)

	w.cellSize, err = gtk.SpinButtonNewWithRange(1, 999, 1)
	e.Exit(err)
	w.cellSize.SetValue(32)

	cellSizeLabel, err := gtk.LabelNew("Cell size:")
	e.Exit(err)

	w.count.Connect("changed", uiShuffle)
	w.count.Connect("activate", uiShuffle)
	shuffle.Connect("clicked", uiShuffle)
	w.showActionsText.Connect("toggled", uiReset)
	w.showPermutation.Connect("toggled", uiReset)
	w.cellSize.Connect("changed", uiReset)

	panel, err := gtk.BoxNew(gtk.ORIENTATION_HORIZONTAL, 0)
	e.Exit(err)
	panel.SetSpacing(5)
	panel.Add(w.count)
	panel.Add(shuffle)
	panel.Add(w.showActionsText)
	panel.Add(w.showPermutation)
	panel.Add(cellSizeLabel)
	panel.Add(w.cellSize)

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
	window.Maximize()
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
	textual := w.showActionsText.GetActive() || r.LongSide == 0
	s := plural(r.NHeaps, "heap")
	if !textual || r.LongSide <= 9 {
		s += fmt.Sprintf(" in a %d×%d layout", r.LongSide, r.ShortSide)
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
	cells := make([]int, 1+r.LongSide*r.ShortSide)
	areaW := 4 + r.ShortSide*w.cellSize.GetValueAsInt()
	areaH := 4 + r.LongSide*w.cellSize.GetValueAsInt()
	for i := 0; i < len(r.Actions); i++ {
		for k, cell := range cells {
			if cell != cellEmpty && cell != cellOccupied {
				cells[k]--
			}
		}

		cells[r.Actions[i]] = cellCurrent

		merged := false
		if i+1 < len(r.Actions) && r.Actions[i+1] < 0 {
			merged = true
			i++
			cells[-r.Actions[i]] = cellMerged
		}

		areaCells := make([]int, len(cells))
		copy(areaCells, cells)
		if merged {
			areaCells[r.Actions[i-1]] = cellCurrentToMerge
		}

		area, err := gtk.DrawingAreaNew()
		e.Exit(err)
		area.SetSizeRequest(areaW, areaH)
		setStyle(area, "borderSolid")
		area.Connect("draw", func(area *gtk.DrawingArea, cr *cairo.Context) {
			drawTable(area, cr, r.LongSide, areaCells)
		})
		tail, err := w.fieldBuf.CreateChildAnchor(w.fieldBuf.GetEndIter())
		e.Exit(err)
		w.field.AddChildAtAnchor(area, tail)
	}
}

func drawTable(area *gtk.DrawingArea, cr *cairo.Context, side int, cells []int) {
	u := w.cellSize.GetValue()
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
