# zig-devicetree

Zig wrapper around libfdt.

## Installation

Add the dependency to `build.zig.zon`:

```sh
zig fetch --save git+https://github.com/CascadeOS/zig-devicetree
```

Then add the following to `build.zig`:

```zig
const devicetree_dep = b.dependency("devicetree", .{});
exe.root_module.addImport("devicetree", devicetree_dep.module("devicetree"));
```
