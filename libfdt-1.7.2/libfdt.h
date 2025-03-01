/* SPDX-License-Identifier: (BSD-2-Clause) */
#ifndef LIBFDT_H
#define LIBFDT_H
/*
 * libfdt - Flat Device Tree manipulation
 * Copyright (C) 2006 David Gibson, IBM Corporation.
 */

#include <libfdt_env.h>
#include <fdt.h>

#ifdef __cplusplus
extern "C" {
#endif

#define FDT_FIRST_SUPPORTED_VERSION	0x02
#define FDT_LAST_COMPATIBLE_VERSION	0x10
#define FDT_LAST_SUPPORTED_VERSION	0x11

/* Error codes: informative error codes */
#define FDT_ERR_NOTFOUND	1
	/* FDT_ERR_NOTFOUND: The requested node or property does not exist */
#define FDT_ERR_EXISTS		2
	/* FDT_ERR_EXISTS: Attempted to create a node or property which
	 * already exists */
#define FDT_ERR_NOSPACE		3
	/* FDT_ERR_NOSPACE: Operation needed to expand the device
	 * tree, but its buffer did not have sufficient space to
	 * contain the expanded tree. Use fdt_open_into() to move the
	 * device tree to a buffer with more space. */

/* Error codes: codes for bad parameters */
#define FDT_ERR_BADOFFSET	4
	/* FDT_ERR_BADOFFSET: Function was passed a structure block
	 * offset which is out-of-bounds, or which points to an
	 * unsuitable part of the structure for the operation. */
#define FDT_ERR_BADPATH		5
	/* FDT_ERR_BADPATH: Function was passed a badly formatted path
	 * (e.g. missing a leading / for a function which requires an
	 * absolute path) */
#define FDT_ERR_BADPHANDLE	6
	/* FDT_ERR_BADPHANDLE: Function was passed an invalid phandle.
	 * This can be caused either by an invalid phandle property
	 * length, or the phandle value was either 0 or -1, which are
	 * not permitted. */
#define FDT_ERR_BADSTATE	7
	/* FDT_ERR_BADSTATE: Function was passed an incomplete device
	 * tree created by the sequential-write functions, which is
	 * not sufficiently complete for the requested operation. */

/* Error codes: codes for bad device tree blobs */
#define FDT_ERR_TRUNCATED	8
	/* FDT_ERR_TRUNCATED: FDT or a sub-block is improperly
	 * terminated (overflows, goes outside allowed bounds, or
	 * isn't properly terminated).  */
#define FDT_ERR_BADMAGIC	9
	/* FDT_ERR_BADMAGIC: Given "device tree" appears not to be a
	 * device tree at all - it is missing the flattened device
	 * tree magic number. */
#define FDT_ERR_BADVERSION	10
	/* FDT_ERR_BADVERSION: Given device tree has a version which
	 * can't be handled by the requested operation.  For
	 * read-write functions, this may mean that fdt_open_into() is
	 * required to convert the tree to the expected version. */
#define FDT_ERR_BADSTRUCTURE	11
	/* FDT_ERR_BADSTRUCTURE: Given device tree has a corrupt
	 * structure block or other serious error (e.g. misnested
	 * nodes, or subnodes preceding properties). */
#define FDT_ERR_BADLAYOUT	12
	/* FDT_ERR_BADLAYOUT: For read-write functions, the given
	 * device tree has it's sub-blocks in an order that the
	 * function can't handle (memory reserve map, then structure,
	 * then strings).  Use fdt_open_into() to reorganize the tree
	 * into a form suitable for the read-write operations. */

/* "Can't happen" error indicating a bug in libfdt */
#define FDT_ERR_INTERNAL	13
	/* FDT_ERR_INTERNAL: libfdt has failed an internal assertion.
	 * Should never be returned, if it is, it indicates a bug in
	 * libfdt itself. */

/* Errors in device tree content */
#define FDT_ERR_BADNCELLS	14
	/* FDT_ERR_BADNCELLS: Device tree has a #address-cells, #size-cells
	 * or similar property with a bad format or value */

#define FDT_ERR_BADVALUE	15
	/* FDT_ERR_BADVALUE: Device tree has a property with an unexpected
	 * value. For example: a property expected to contain a string list
	 * is not NUL-terminated within the length of its value. */

#define FDT_ERR_BADOVERLAY	16
	/* FDT_ERR_BADOVERLAY: The device tree overlay, while
	 * correctly structured, cannot be applied due to some
	 * unexpected or missing value, property or node. */

#define FDT_ERR_NOPHANDLES	17
	/* FDT_ERR_NOPHANDLES: The device tree doesn't have any
	 * phandle available anymore without causing an overflow */

#define FDT_ERR_BADFLAGS	18
	/* FDT_ERR_BADFLAGS: The function was passed a flags field that
	 * contains invalid flags or an invalid combination of flags. */

#define FDT_ERR_ALIGNMENT	19
	/* FDT_ERR_ALIGNMENT: The device tree base address is not 8-byte
	 * aligned. */

/**********************************************************************/
/* Low-level functions (you probably don't need these)                */
/**********************************************************************/

const void *fdt_offset_ptr(const void *fdt, int offset, unsigned int checklen);

static inline void *fdt_offset_ptr_w(void *fdt, int offset, int checklen)
{
	return (void *)(uintptr_t)fdt_offset_ptr(fdt, offset, checklen);
}

uint32_t fdt_next_tag(const void *fdt, int offset, int *nextoffset);

/*
 * External helpers to access words from a device tree blob. They're built
 * to work even with unaligned pointers on platforms (such as ARMv5) that don't
 * like unaligned loads and stores.
 */
static inline uint16_t fdt16_ld(const fdt16_t *p)
{
	const uint8_t *bp = (const uint8_t *)p;

	return ((uint16_t)bp[0] << 8) | bp[1];
}

static inline uint32_t fdt32_ld(const fdt32_t *p)
{
	const uint8_t *bp = (const uint8_t *)p;

	return ((uint32_t)bp[0] << 24)
		| ((uint32_t)bp[1] << 16)
		| ((uint32_t)bp[2] << 8)
		| bp[3];
}

static inline void fdt32_st(void *property, uint32_t value)
{
	uint8_t *bp = (uint8_t *)property;

	bp[0] = value >> 24;
	bp[1] = (value >> 16) & 0xff;
	bp[2] = (value >> 8) & 0xff;
	bp[3] = value & 0xff;
}

static inline uint64_t fdt64_ld(const fdt64_t *p)
{
	const uint8_t *bp = (const uint8_t *)p;

	return ((uint64_t)bp[0] << 56)
		| ((uint64_t)bp[1] << 48)
		| ((uint64_t)bp[2] << 40)
		| ((uint64_t)bp[3] << 32)
		| ((uint64_t)bp[4] << 24)
		| ((uint64_t)bp[5] << 16)
		| ((uint64_t)bp[6] << 8)
		| bp[7];
}

static inline void fdt64_st(void *property, uint64_t value)
{
	uint8_t *bp = (uint8_t *)property;

	bp[0] = value >> 56;
	bp[1] = (value >> 48) & 0xff;
	bp[2] = (value >> 40) & 0xff;
	bp[3] = (value >> 32) & 0xff;
	bp[4] = (value >> 24) & 0xff;
	bp[5] = (value >> 16) & 0xff;
	bp[6] = (value >> 8) & 0xff;
	bp[7] = value & 0xff;
}

/**********************************************************************/
/* Traversal functions                                                */
/**********************************************************************/

int fdt_next_node(const void *fdt, int offset, int *depth);

/**
 * fdt_first_subnode() - get offset of first direct subnode
 * @fdt:	FDT blob
 * @offset:	Offset of node to check
 *
 * Return: offset of first subnode, or -FDT_ERR_NOTFOUND if there is none
 */
int fdt_first_subnode(const void *fdt, int offset);

/**
 * fdt_next_subnode() - get offset of next direct subnode
 * @fdt:	FDT blob
 * @offset:	Offset of previous subnode
 *
 * After first calling fdt_first_subnode(), call this function repeatedly to
 * get direct subnodes of a parent node.
 *
 * Return: offset of next subnode, or -FDT_ERR_NOTFOUND if there are no more
 *         subnodes
 */
int fdt_next_subnode(const void *fdt, int offset);

/**********************************************************************/
/* General functions                                                  */
/**********************************************************************/
#define fdt_get_header(fdt, field) \
	(fdt32_ld(&((const struct fdt_header *)(fdt))->field))
#define fdt_magic(fdt)			(fdt_get_header(fdt, magic))
#define fdt_totalsize(fdt)		(fdt_get_header(fdt, totalsize))
#define fdt_off_dt_struct(fdt)		(fdt_get_header(fdt, off_dt_struct))
#define fdt_off_dt_strings(fdt)		(fdt_get_header(fdt, off_dt_strings))
#define fdt_off_mem_rsvmap(fdt)		(fdt_get_header(fdt, off_mem_rsvmap))
#define fdt_version(fdt)		(fdt_get_header(fdt, version))
#define fdt_last_comp_version(fdt)	(fdt_get_header(fdt, last_comp_version))
#define fdt_boot_cpuid_phys(fdt)	(fdt_get_header(fdt, boot_cpuid_phys))
#define fdt_size_dt_strings(fdt)	(fdt_get_header(fdt, size_dt_strings))
#define fdt_size_dt_struct(fdt)		(fdt_get_header(fdt, size_dt_struct))

/**
 * fdt_header_size - return the size of the tree's header
 * @fdt: pointer to a flattened device tree
 *
 * Return: size of DTB header in bytes
 */
size_t fdt_header_size(const void *fdt);

/**
 * fdt_header_size_ - internal function to get header size from a version number
 * @version: device tree version number
 *
 * Return: size of DTB header in bytes
 */
size_t fdt_header_size_(uint32_t version);

/**
 * fdt_check_header - sanity check a device tree header
 * @fdt: pointer to data which might be a flattened device tree
 *
 * fdt_check_header() checks that the given buffer contains what
 * appears to be a flattened device tree, and that the header contains
 * valid information (to the extent that can be determined from the
 * header alone).
 *
 * returns:
 *     0, if the buffer appears to contain a valid device tree
 *     -FDT_ERR_BADMAGIC,
 *     -FDT_ERR_BADVERSION,
 *     -FDT_ERR_BADSTATE,
 *     -FDT_ERR_TRUNCATED, standard meanings, as above
 */
int fdt_check_header(const void *fdt);

/**
 * fdt_move - move a device tree around in memory
 * @fdt: pointer to the device tree to move
 * @buf: pointer to memory where the device is to be moved
 * @bufsize: size of the memory space at buf
 *
 * fdt_move() relocates, if possible, the device tree blob located at
 * fdt to the buffer at buf of size bufsize.  The buffer may overlap
 * with the existing device tree blob at fdt.  Therefore,
 *     fdt_move(fdt, fdt, fdt_totalsize(fdt))
 * should always succeed.
 *
 * returns:
 *     0, on success
 *     -FDT_ERR_NOSPACE, bufsize is insufficient to contain the device tree
 *     -FDT_ERR_BADMAGIC,
 *     -FDT_ERR_BADVERSION,
 *     -FDT_ERR_BADSTATE, standard meanings
 */
int fdt_move(const void *fdt, void *buf, int bufsize);

/**********************************************************************/
/* Read-only functions                                                */
/**********************************************************************/

int fdt_check_full(const void *fdt, size_t bufsize);

/**
 * fdt_get_string - retrieve a string from the strings block of a device tree
 * @fdt: pointer to the device tree blob
 * @stroffset: offset of the string within the strings block (native endian)
 * @lenp: optional pointer to return the string's length
 *
 * fdt_get_string() retrieves a pointer to a single string from the
 * strings block of the device tree blob at fdt, and optionally also
 * returns the string's length in *lenp.
 *
 * returns:
 *     a pointer to the string, on success
 *     NULL, if stroffset is out of bounds, or doesn't point to a valid string
 */
const char *fdt_get_string(const void *fdt, int stroffset, int *lenp);

/**
 * fdt_string - retrieve a string from the strings block of a device tree
 * @fdt: pointer to the device tree blob
 * @stroffset: offset of the string within the strings block (native endian)
 *
 * fdt_string() retrieves a pointer to a single string from the
 * strings block of the device tree blob at fdt.
 *
 * returns:
 *     a pointer to the string, on success
 *     NULL, if stroffset is out of bounds, or doesn't point to a valid string
 */
const char *fdt_string(const void *fdt, int stroffset);

/**
 * fdt_num_mem_rsv - retrieve the number of memory reserve map entries
 * @fdt: pointer to the device tree blob
 *
 * Returns the number of entries in the device tree blob's memory
 * reservation map.  This does not include the terminating 0,0 entry
 * or any other (0,0) entries reserved for expansion.
 *
 * returns:
 *     the number of entries
 */
int fdt_num_mem_rsv(const void *fdt);

/**
 * fdt_get_mem_rsv - retrieve one memory reserve map entry
 * @fdt: pointer to the device tree blob
 * @n: index of reserve map entry
 * @address: pointer to 64-bit variable to hold the start address
 * @size: pointer to 64-bit variable to hold the size of the entry
 *
 * On success, @address and @size will contain the address and size of
 * the n-th reserve map entry from the device tree blob, in
 * native-endian format.
 *
 * returns:
 *     0, on success
 *     -FDT_ERR_BADMAGIC,
 *     -FDT_ERR_BADVERSION,
 *     -FDT_ERR_BADSTATE, standard meanings
 */
int fdt_get_mem_rsv(const void *fdt, int n, uint64_t *address, uint64_t *size);

/**
 * fdt_subnode_offset_namelen - find a subnode based on substring
 * @fdt: pointer to the device tree blob
 * @parentoffset: structure block offset of a node
 * @name: name of the subnode to locate
 * @namelen: number of characters of name to consider
 *
 * Identical to fdt_subnode_offset(), but only examine the first
 * namelen characters of name for matching the subnode name.  This is
 * useful for finding subnodes based on a portion of a larger string,
 * such as a full path.
 *
 * Return: offset of the subnode or -FDT_ERR_NOTFOUND if name not found.
 */
int fdt_subnode_offset_namelen(const void *fdt, int parentoffset,
			       const char *name, int namelen);

/**
 * fdt_path_offset_namelen - find a tree node by its full path
 * @fdt: pointer to the device tree blob
 * @path: full path of the node to locate
 * @namelen: number of characters of path to consider
 *
 * Identical to fdt_path_offset(), but only consider the first namelen
 * characters of path as the path name.
 *
 * Return: offset of the node or negative libfdt error value otherwise
 */
int fdt_path_offset_namelen(const void *fdt, const char *path, int namelen);

/**
 * fdt_path_offset - find a tree node by its full path
 * @fdt: pointer to the device tree blob
 * @path: full path of the node to locate
 *
 * fdt_path_offset() finds a node of a given path in the device tree.
 * Each path component may omit the unit address portion, but the
 * results of this are undefined if any such path component is
 * ambiguous (that is if there are multiple nodes at the relevant
 * level matching the given component, differentiated only by unit
 * address).
 *
 * If the path is not absolute (i.e. does not begin with '/'), the
 * first component is treated as an alias.  That is, the property by
 * that name is looked up in the /aliases node, and the value of that
 * property used in place of that first component.
 *
 * For example, for this small fragment
 *
 * / {
 *     aliases {
 *         i2c2 = &foo; // RHS compiles to "/soc@0/i2c@30a40000/eeprom@52"
 *     };
 *     soc@0 {
 *         foo: i2c@30a40000 {
 *             bar: eeprom@52 {
 *             };
 *         };
 *     };
 * };
 *
 * these would be equivalent:
 *
 *   /soc@0/i2c@30a40000/eeprom@52
 *   i2c2/eeprom@52
 *
 * returns:
 *	structure block offset of the node with the requested path (>=0), on
 *		success
 *	-FDT_ERR_BADPATH, given path does not begin with '/' and the first
 *		component is not a valid alias
 *	-FDT_ERR_NOTFOUND, if the requested node does not exist
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE,
 *	-FDT_ERR_TRUNCATED, standard meanings.
 */
int fdt_path_offset(const void *fdt, const char *path);

/**
 * fdt_get_name - retrieve the name of a given node
 * @fdt: pointer to the device tree blob
 * @nodeoffset: structure block offset of the starting node
 * @lenp: pointer to an integer variable (will be overwritten) or NULL
 *
 * fdt_get_name() retrieves the name (including unit address) of the
 * device tree node at structure block offset nodeoffset.  If lenp is
 * non-NULL, the length of this name is also returned, in the integer
 * pointed to by lenp.
 *
 * returns:
 *	pointer to the node's name, on success
 *		If lenp is non-NULL, *lenp contains the length of that name
 *			(>=0)
 *	NULL, on error
 *		if lenp is non-NULL *lenp contains an error code (<0):
 *		-FDT_ERR_BADOFFSET, nodeoffset did not point to FDT_BEGIN_NODE
 *			tag
 *		-FDT_ERR_BADMAGIC,
 *		-FDT_ERR_BADVERSION,
 *		-FDT_ERR_BADSTATE, standard meanings
 */
const char *fdt_get_name(const void *fdt, int nodeoffset, int *lenp);

/**
 * fdt_first_property_offset - find the offset of a node's first property
 * @fdt: pointer to the device tree blob
 * @nodeoffset: structure block offset of a node
 *
 * fdt_first_property_offset() finds the first property of the node at
 * the given structure block offset.
 *
 * returns:
 *	structure block offset of the property (>=0), on success
 *	-FDT_ERR_NOTFOUND, if the requested node has no properties
 *	-FDT_ERR_BADOFFSET, if nodeoffset did not point to an FDT_BEGIN_NODE tag
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE,
 *	-FDT_ERR_TRUNCATED, standard meanings.
 */
int fdt_first_property_offset(const void *fdt, int nodeoffset);

/**
 * fdt_next_property_offset - step through a node's properties
 * @fdt: pointer to the device tree blob
 * @offset: structure block offset of a property
 *
 * fdt_next_property_offset() finds the property immediately after the
 * one at the given structure block offset.  This will be a property
 * of the same node as the given property.
 *
 * returns:
 *	structure block offset of the next property (>=0), on success
 *	-FDT_ERR_NOTFOUND, if the given property is the last in its node
 *	-FDT_ERR_BADOFFSET, if nodeoffset did not point to an FDT_PROP tag
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE,
 *	-FDT_ERR_TRUNCATED, standard meanings.
 */
int fdt_next_property_offset(const void *fdt, int offset);

/**
 * fdt_getprop_by_offset - retrieve the value of a property at a given offset
 * @fdt: pointer to the device tree blob
 * @offset: offset of the property to read
 * @namep: pointer to a string variable (will be overwritten) or NULL
 * @lenp: pointer to an integer variable (will be overwritten) or NULL
 *
 * fdt_getprop_by_offset() retrieves a pointer to the value of the
 * property at structure block offset 'offset' (this will be a pointer
 * to within the device blob itself, not a copy of the value).  If
 * lenp is non-NULL, the length of the property value is also
 * returned, in the integer pointed to by lenp.  If namep is non-NULL,
 * the property's name will also be returned in the char * pointed to
 * by namep (this will be a pointer to within the device tree's string
 * block, not a new copy of the name).
 *
 * returns:
 *	pointer to the property's value
 *		if lenp is non-NULL, *lenp contains the length of the property
 *		value (>=0)
 *		if namep is non-NULL *namep contains a pointer to the property
 *		name.
 *	NULL, on error
 *		if lenp is non-NULL, *lenp contains an error code (<0):
 *		-FDT_ERR_BADOFFSET, nodeoffset did not point to FDT_PROP tag
 *		-FDT_ERR_BADMAGIC,
 *		-FDT_ERR_BADVERSION,
 *		-FDT_ERR_BADSTATE,
 *		-FDT_ERR_BADSTRUCTURE,
 *		-FDT_ERR_TRUNCATED, standard meanings
 */
const void *fdt_getprop_by_offset(const void *fdt, int offset,
				  const char **namep, int *lenp);

/**
 * fdt_getprop_namelen - get property value based on substring
 * @fdt: pointer to the device tree blob
 * @nodeoffset: offset of the node whose property to find
 * @name: name of the property to find
 * @namelen: number of characters of name to consider
 * @lenp: pointer to an integer variable (will be overwritten) or NULL
 *
 * Identical to fdt_getprop(), but only examine the first namelen
 * characters of name for matching the property name.
 *
 * Return: pointer to the property's value or NULL on error
 */
const void *fdt_getprop_namelen(const void *fdt, int nodeoffset,
				const char *name, int namelen, int *lenp);

/**
 * fdt_getprop - retrieve the value of a given property
 * @fdt: pointer to the device tree blob
 * @nodeoffset: offset of the node whose property to find
 * @name: name of the property to find
 * @lenp: pointer to an integer variable (will be overwritten) or NULL
 *
 * fdt_getprop() retrieves a pointer to the value of the property
 * named @name of the node at offset @nodeoffset (this will be a
 * pointer to within the device blob itself, not a copy of the value).
 * If @lenp is non-NULL, the length of the property value is also
 * returned, in the integer pointed to by @lenp.
 *
 * returns:
 *	pointer to the property's value
 *		if lenp is non-NULL, *lenp contains the length of the property
 *		value (>=0)
 *	NULL, on error
 *		if lenp is non-NULL, *lenp contains an error code (<0):
 *		-FDT_ERR_NOTFOUND, node does not have named property
 *		-FDT_ERR_BADOFFSET, nodeoffset did not point to FDT_BEGIN_NODE
 *			tag
 *		-FDT_ERR_BADMAGIC,
 *		-FDT_ERR_BADVERSION,
 *		-FDT_ERR_BADSTATE,
 *		-FDT_ERR_BADSTRUCTURE,
 *		-FDT_ERR_TRUNCATED, standard meanings
 */
const void *fdt_getprop(const void *fdt, int nodeoffset,
			const char *name, int *lenp);

/**
 * fdt_get_phandle - retrieve the phandle of a given node
 * @fdt: pointer to the device tree blob
 * @nodeoffset: structure block offset of the node
 *
 * fdt_get_phandle() retrieves the phandle of the device tree node at
 * structure block offset nodeoffset.
 *
 * returns:
 *	the phandle of the node at nodeoffset, on success (!= 0, != -1)
 *	0, if the node has no phandle, or another error occurs
 */
uint32_t fdt_get_phandle(const void *fdt, int nodeoffset);

/**
 * fdt_get_alias_namelen - get alias based on substring
 * @fdt: pointer to the device tree blob
 * @name: name of the alias to look up
 * @namelen: number of characters of name to consider
 *
 * Identical to fdt_get_alias(), but only examine the first @namelen
 * characters of @name for matching the alias name.
 *
 * Return: a pointer to the expansion of the alias named @name, if it exists,
 *	   NULL otherwise
 */
const char *fdt_get_alias_namelen(const void *fdt,
				  const char *name, int namelen);

/**
 * fdt_get_symbol_namelen - get symbol based on substring
 * @fdt: pointer to the device tree blob
 * @name: name of the symbol to look up
 * @namelen: number of characters of name to consider
 *
 * Identical to fdt_get_symbol(), but only examine the first @namelen
 * characters of @name for matching the symbol name.
 *
 * Return: a pointer to the expansion of the symbol named @name, if it exists,
 *	   NULL otherwise
 */
const char *fdt_get_symbol_namelen(const void *fdt,
				   const char *name, int namelen);

/**
 * fdt_get_path - determine the full path of a node
 * @fdt: pointer to the device tree blob
 * @nodeoffset: offset of the node whose path to find
 * @buf: character buffer to contain the returned path (will be overwritten)
 * @buflen: size of the character buffer at buf
 *
 * fdt_get_path() computes the full path of the node at offset
 * nodeoffset, and records that path in the buffer at buf.
 *
 * NOTE: This function is expensive, as it must scan the device tree
 * structure from the start to nodeoffset.
 *
 * returns:
 *	0, on success
 *		buf contains the absolute path of the node at
 *		nodeoffset, as a NUL-terminated string.
 *	-FDT_ERR_BADOFFSET, nodeoffset does not refer to a BEGIN_NODE tag
 *	-FDT_ERR_NOSPACE, the path of the given node is longer than (bufsize-1)
 *		characters and will not fit in the given buffer.
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE, standard meanings
 */
int fdt_get_path(const void *fdt, int nodeoffset, char *buf, int buflen);

/**
 * fdt_supernode_atdepth_offset - find a specific ancestor of a node
 * @fdt: pointer to the device tree blob
 * @nodeoffset: offset of the node whose parent to find
 * @supernodedepth: depth of the ancestor to find
 * @nodedepth: pointer to an integer variable (will be overwritten) or NULL
 *
 * fdt_supernode_atdepth_offset() finds an ancestor of the given node
 * at a specific depth from the root (where the root itself has depth
 * 0, its immediate subnodes depth 1 and so forth).  So
 *	fdt_supernode_atdepth_offset(fdt, nodeoffset, 0, NULL);
 * will always return 0, the offset of the root node.  If the node at
 * nodeoffset has depth D, then:
 *	fdt_supernode_atdepth_offset(fdt, nodeoffset, D, NULL);
 * will return nodeoffset itself.
 *
 * NOTE: This function is expensive, as it must scan the device tree
 * structure from the start to nodeoffset.
 *
 * returns:
 *	structure block offset of the node at node offset's ancestor
 *		of depth supernodedepth (>=0), on success
 *	-FDT_ERR_BADOFFSET, nodeoffset does not refer to a BEGIN_NODE tag
 *	-FDT_ERR_NOTFOUND, supernodedepth was greater than the depth of
 *		nodeoffset
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE, standard meanings
 */
int fdt_supernode_atdepth_offset(const void *fdt, int nodeoffset,
				 int supernodedepth, int *nodedepth);

/**
 * fdt_node_depth - find the depth of a given node
 * @fdt: pointer to the device tree blob
 * @nodeoffset: offset of the node whose parent to find
 *
 * fdt_node_depth() finds the depth of a given node.  The root node
 * has depth 0, its immediate subnodes depth 1 and so forth.
 *
 * NOTE: This function is expensive, as it must scan the device tree
 * structure from the start to nodeoffset.
 *
 * returns:
 *	depth of the node at nodeoffset (>=0), on success
 *	-FDT_ERR_BADOFFSET, nodeoffset does not refer to a BEGIN_NODE tag
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE, standard meanings
 */
int fdt_node_depth(const void *fdt, int nodeoffset);

/**
 * fdt_parent_offset - find the parent of a given node
 * @fdt: pointer to the device tree blob
 * @nodeoffset: offset of the node whose parent to find
 *
 * fdt_parent_offset() locates the parent node of a given node (that
 * is, it finds the offset of the node which contains the node at
 * nodeoffset as a subnode).
 *
 * NOTE: This function is expensive, as it must scan the device tree
 * structure from the start to nodeoffset, *twice*.
 *
 * returns:
 *	structure block offset of the parent of the node at nodeoffset
 *		(>=0), on success
 *	-FDT_ERR_BADOFFSET, nodeoffset does not refer to a BEGIN_NODE tag
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE, standard meanings
 */
int fdt_parent_offset(const void *fdt, int nodeoffset);

/**
 * fdt_node_offset_by_prop_value - find nodes with a given property value
 * @fdt: pointer to the device tree blob
 * @startoffset: only find nodes after this offset
 * @propname: property name to check
 * @propval: property value to search for
 * @proplen: length of the value in propval
 *
 * fdt_node_offset_by_prop_value() returns the offset of the first
 * node after startoffset, which has a property named propname whose
 * value is of length proplen and has value equal to propval; or if
 * startoffset is -1, the very first such node in the tree.
 *
 * To iterate through all nodes matching the criterion, the following
 * idiom can be used:
 *	offset = fdt_node_offset_by_prop_value(fdt, -1, propname,
 *					       propval, proplen);
 *	while (offset != -FDT_ERR_NOTFOUND) {
 *		// other code here
 *		offset = fdt_node_offset_by_prop_value(fdt, offset, propname,
 *						       propval, proplen);
 *	}
 *
 * Note the -1 in the first call to the function, if 0 is used here
 * instead, the function will never locate the root node, even if it
 * matches the criterion.
 *
 * returns:
 *	structure block offset of the located node (>= 0, >startoffset),
 *		 on success
 *	-FDT_ERR_NOTFOUND, no node matching the criterion exists in the
 *		tree after startoffset
 *	-FDT_ERR_BADOFFSET, nodeoffset does not refer to a BEGIN_NODE tag
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE, standard meanings
 */
int fdt_node_offset_by_prop_value(const void *fdt, int startoffset,
				  const char *propname,
				  const void *propval, int proplen);

/**
 * fdt_node_offset_by_phandle - find the node with a given phandle
 * @fdt: pointer to the device tree blob
 * @phandle: phandle value
 *
 * fdt_node_offset_by_phandle() returns the offset of the node
 * which has the given phandle value.  If there is more than one node
 * in the tree with the given phandle (an invalid tree), results are
 * undefined.
 *
 * returns:
 *	structure block offset of the located node (>= 0), on success
 *	-FDT_ERR_NOTFOUND, no node with that phandle exists
 *	-FDT_ERR_BADPHANDLE, given phandle value was invalid (0 or -1)
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE, standard meanings
 */
int fdt_node_offset_by_phandle(const void *fdt, uint32_t phandle);

/**
 * fdt_node_check_compatible - check a node's compatible property
 * @fdt: pointer to the device tree blob
 * @nodeoffset: offset of a tree node
 * @compatible: string to match against
 *
 * fdt_node_check_compatible() returns 0 if the given node contains a
 * @compatible property with the given string as one of its elements,
 * it returns non-zero otherwise, or on error.
 *
 * returns:
 *	0, if the node has a 'compatible' property listing the given string
 *	1, if the node has a 'compatible' property, but it does not list
 *		the given string
 *	-FDT_ERR_NOTFOUND, if the given node has no 'compatible' property
 *	-FDT_ERR_BADOFFSET, if nodeoffset does not refer to a BEGIN_NODE tag
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE, standard meanings
 */
int fdt_node_check_compatible(const void *fdt, int nodeoffset,
			      const char *compatible);

/**
 * fdt_node_offset_by_compatible - find nodes with a given 'compatible' value
 * @fdt: pointer to the device tree blob
 * @startoffset: only find nodes after this offset
 * @compatible: 'compatible' string to match against
 *
 * fdt_node_offset_by_compatible() returns the offset of the first
 * node after startoffset, which has a 'compatible' property which
 * lists the given compatible string; or if startoffset is -1, the
 * very first such node in the tree.
 *
 * To iterate through all nodes matching the criterion, the following
 * idiom can be used:
 *	offset = fdt_node_offset_by_compatible(fdt, -1, compatible);
 *	while (offset != -FDT_ERR_NOTFOUND) {
 *		// other code here
 *		offset = fdt_node_offset_by_compatible(fdt, offset, compatible);
 *	}
 *
 * Note the -1 in the first call to the function, if 0 is used here
 * instead, the function will never locate the root node, even if it
 * matches the criterion.
 *
 * returns:
 *	structure block offset of the located node (>= 0, >startoffset),
 *		 on success
 *	-FDT_ERR_NOTFOUND, no node matching the criterion exists in the
 *		tree after startoffset
 *	-FDT_ERR_BADOFFSET, nodeoffset does not refer to a BEGIN_NODE tag
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE, standard meanings
 */
int fdt_node_offset_by_compatible(const void *fdt, int startoffset,
				  const char *compatible);

/**
 * fdt_stringlist_contains - check a string list property for a string
 * @strlist: Property containing a list of strings to check
 * @listlen: Length of property
 * @str: String to search for
 *
 * This is a utility function provided for convenience. The list contains
 * one or more strings, each terminated by \0, as is found in a device tree
 * "compatible" property.
 *
 * Return: 1 if the string is found in the list, 0 not found, or invalid list
 */
int fdt_stringlist_contains(const char *strlist, int listlen, const char *str);

/**********************************************************************/
/* Read-only functions (addressing related)                           */
/**********************************************************************/

/**
 * FDT_MAX_NCELLS - maximum value for #address-cells and #size-cells
 *
 * This is the maximum value for #address-cells, #size-cells and
 * similar properties that will be processed by libfdt.  IEE1275
 * requires that OF implementations handle values up to 4.
 * Implementations may support larger values, but in practice higher
 * values aren't used.
 */
#define FDT_MAX_NCELLS		4

/**
 * fdt_address_cells - retrieve address size for a bus represented in the tree
 * @fdt: pointer to the device tree blob
 * @nodeoffset: offset of the node to find the address size for
 *
 * When the node has a valid #address-cells property, returns its value.
 *
 * returns:
 *	0 <= n < FDT_MAX_NCELLS, on success
 *	2, if the node has no #address-cells property
 *	-FDT_ERR_BADNCELLS, if the node has a badly formatted or invalid
 *		#address-cells property
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE,
 *	-FDT_ERR_TRUNCATED, standard meanings
 */
int fdt_address_cells(const void *fdt, int nodeoffset);

/**
 * fdt_size_cells - retrieve address range size for a bus represented in the
 *                  tree
 * @fdt: pointer to the device tree blob
 * @nodeoffset: offset of the node to find the address range size for
 *
 * When the node has a valid #size-cells property, returns its value.
 *
 * returns:
 *	0 <= n < FDT_MAX_NCELLS, on success
 *	1, if the node has no #size-cells property
 *	-FDT_ERR_BADNCELLS, if the node has a badly formatted or invalid
 *		#size-cells property
 *	-FDT_ERR_BADMAGIC,
 *	-FDT_ERR_BADVERSION,
 *	-FDT_ERR_BADSTATE,
 *	-FDT_ERR_BADSTRUCTURE,
 *	-FDT_ERR_TRUNCATED, standard meanings
 */
int fdt_size_cells(const void *fdt, int nodeoffset);

#ifdef __cplusplus
}
#endif

#endif /* LIBFDT_H */
