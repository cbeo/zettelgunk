

# Using Zettelgunk

You need to set the value of `zettel-directory` to someplace writable
by emacs, it is set to `~/zettel/` by default.  You need to include
the trailing slash.

Notes are plain text files.  You can make links to
other notes |that-look-like-this|. You can make tags #like-this.

Here are the commands that you'll want to bind to suit your needs:

- `zettel` jumps you into your root zettel directory
- `zettel-spanking-new-note` makes a new note
- `zettel-browse-tags` search for a tag (using emacs'
  `completing-read` then displays all notes with that tag
- `zettel-search-notes` opens a browse buffer displaying links to
  notes containing a search term
- `zettel-browse-notes-linking-here` shows you "reverse links" to the
  note you are currently looking at.
- `zettel-jump-back` jumps back to the last note you were looking at
- `zettel-browse-notes` search for a note by title, using emacs' `completing-read`
- `zettel-browse-notes-days-ago` shows notes that were touched in the
  last few days, you supply the period of time. Good for reviewing
  your week of thought.

I have each of the above bound to suit my needs in my
`~/.emacs.d/init.el` file.

In zettel mode, when looking at a zettel file, you can press Enter on
any note or tag. Pressing enter on a tag will dislplay a tag browse
buffer.  Pressing enter on a note will jump to that note.

Tags an Note Names should autocomplete as you type them.

I have bound `M-n` to `zettel-next-thing-in-buffer` and `M-p` to
`zettel-prev-thing-in-buffer`.  Each of these will advance the cursor
to the next or previous link in the buffer, respectively.



