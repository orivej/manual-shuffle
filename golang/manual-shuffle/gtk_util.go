package main

import (
	"github.com/gotk3/gotk3/gtk"
	"github.com/orivej/e"
)

type IGetStyleContext interface {
	GetStyleContext() (*gtk.StyleContext, error)
}

func setStyle(widget IGetStyleContext, class string) {
	styleCtx, err := widget.GetStyleContext()
	e.Exit(err)
	styleCtx.AddProvider(cssProvider, gtk.STYLE_PROVIDER_PRIORITY_USER+1)
	styleCtx.AddClass(class)
}
