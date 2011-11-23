# TPFS

TPFS (the Tagged Plain File System) is a revolutionary new file system
developed by ΩF:∅ that aims to move beyond the rut of hierarchical
file systems through a purely tag based model. There are only two
objects in TPFS: files and tags. They share a many-to-many
relationship; files can have many different tags, and tags can have
many different files. It is designed to be quick to look up in both
directions.

TPFS is simple, but powerful. Tags can store any string data and are
not restricted in size. They can be looked up quickly, through SHA-256
hashing and a B-tree index. While TPFS is not designed to be the
fastest or the most reliable, it should be enough to satisfy most
users. The loss is a small price to pay for such an innovative
filesystem.

The reference implementation of TPFS is written in Haskell. Lots of
help is currently needed, but please note that the challenges are
often not for the faint of heart. It is recommended to be quite
comfortable with Haskell and the barebones basics of filesystems
before attempting this project.

That said, if you're interested, here's a rough idea of what needs to
be done:

- A B-Tree implementation in Haskell using the
  [Device](http://h.devyn.tk/docs/tpfs/System-TPFS-Device.html)
  API. This will be used for the file and tag indexes.
- Implementation of the low level part of
  [`System.TPFS.Objects`](http://h.devyn.tk/docs/tpfs/System-TPFS-Objects.html). This,
  of course, is dependent on the B-Tree implementation.
- Design of the high level access API for the Haskell implementation.
- A FUSE driver, probably using
  [HFuse](http://hackage.haskell.org/package/HFuse) if it works. The
  FUSE driver will need to be very carefully planned; mapping TPFS to
  the conventional POSIX system in an obvious manner won't be easy.
