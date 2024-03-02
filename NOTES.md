### Syntax highlighting

Treesitter highlighting should probably be done with two passes.

One that just performs syntax highlighting on the buffer of text that is visible.
That result should then be displayed, whilst the complete parsing 
be performed in the background.
There is the real chance that the buffer isn't valid syntax therefore the
visible buffer should be updated at a later time.
