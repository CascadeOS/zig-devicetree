# zig-devicetree

A read-only Flattened Devicetree (DTB) API.

None of the API requires allocation except the various list builders in `Property.Value` which are completely optional.

Compatible with [Devicetree Specification v0.4](https://github.com/devicetree-org/devicetree-specification/releases/tag/v0.4).

[Auto-generated docs](https://cascadeos.github.io/zig-devicetree/)

This started as a wrapper around [libfdt](https://github.com/dgibson/dtc/tree/main/libfdt) but is now a fresh implementation.

## Installation

Add the dependency to `build.zig.zon`:

```sh
zig fetch --save git+https://github.com/CascadeOS/zig-devicetree
```

Then add the following to `build.zig`:

```zig
const devicetree_dep = b.dependency("devicetree", .{});
exe.root_module.addImport("DeviceTree", devicetree_dep.module("DeviceTree"));
```
