---
name: There is no generic for this
about: You wanted to change something and found nothing to specialize
labels: extension-surface
---

**What you wanted the window manager to do differently**

**What you found when you looked**

```
latticewm --extension-surface | grep -i <the thing>
latticewm --list-options | grep -i <the thing>
```

<!-- If a generic exists and does not go far enough, say which one and where
     it stops.  If nothing exists, say what you had to patch instead. -->

**What you had to do instead**

<!-- A copy of the core function you redefined, a fork, a shell script, giving
     up.  All four are useful answers and the fourth is the most useful. -->

---

This is the most valuable kind of issue this project takes. `capture-wanted-p`,
`border-state` and `entry-address` all exist because something that should have
been a method was a hardcoded decision, and each was found by somebody trying
to change it. A report that says "there is no generic for X" beats a patch that
special-cases X.
