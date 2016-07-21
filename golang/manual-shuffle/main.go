package main

import (
	"fmt"
	"strconv"

	"github.com/andlabs/ui"
	"github.com/orivej/e"
)

func main() {
	e.Exit(ui.Main(uiMain))
}

func uiMain() {
	count := ui.NewEntry()
	count.SetText("2")
	shuffle := ui.NewButton("Manual shuffle")
	v2 := ui.NewCheckbox("v2")
	v2.SetChecked(true)
	field := ui.NewMultilineEntry()

	shuffle.OnClicked(func(*ui.Button) {
		text := count.Text()
		n, err := strconv.Atoi(text)
		if err != nil {
			field.SetText(err.Error())
			return
		}
		if n < 1 {
			field.SetText("The number must be positive.")
			return
		}
		actions, perm, nheaps := v2shuffle(n)
		field.SetText(fmt.Sprintf("%d heaps.\nActions: %s.\nPermutation: %v.",
			nheaps, v2actionsString(actions), perm))
	})

	panel := ui.NewHorizontalBox()
	panel.Append(count, false)
	panel.Append(shuffle, false)
	// panel.Append(v2, false)

	layout := ui.NewVerticalBox()
	layout.Append(panel, false)
	layout.Append(field, true)

	window := ui.NewWindow("Manual shuffle", 800, 600, false)
	window.SetChild(layout)
	window.OnClosing(uiQuit)
	window.Show()
}

func uiQuit(*ui.Window) bool {
	ui.Quit()
	return true
}
