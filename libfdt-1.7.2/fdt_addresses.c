// SPDX-License-Identifier: (BSD-2-Clause)
/*
 * libfdt - Flat Device Tree manipulation
 * Copyright (C) 2014 David Gibson <david@gibson.dropbear.id.au>
 * Copyright (C) 2018 embedded brains GmbH
 */
#include "libfdt_env.h"

#include <fdt.h>
#include <libfdt.h>

#include "libfdt_internal.h"

static int fdt_cells(const void *fdt, int nodeoffset, const char *name)
{
	const fdt32_t *c;
	uint32_t val;
	int len;

	c = fdt_getprop(fdt, nodeoffset, name, &len);
	if (!c)
		return len;

	if (len != sizeof(*c))
		return -FDT_ERR_BADNCELLS;

	val = fdt32_to_cpu(*c);
	if (val > FDT_MAX_NCELLS)
		return -FDT_ERR_BADNCELLS;

	return (int)val;
}

int fdt_address_cells(const void *fdt, int nodeoffset)
{
	int val;

	val = fdt_cells(fdt, nodeoffset, "#address-cells");
	if (val == 0)
		return -FDT_ERR_BADNCELLS;
	if (val == -FDT_ERR_NOTFOUND)
		return 2;
	return val;
}

int fdt_size_cells(const void *fdt, int nodeoffset)
{
	int val;

	val = fdt_cells(fdt, nodeoffset, "#size-cells");
	if (val == -FDT_ERR_NOTFOUND)
		return 1;
	return val;
}
