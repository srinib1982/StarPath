/**
 * pugixml parser - version 1.8
 * --------------------------------------------------------
 * Copyright (C) 2006-2017, by Arseny Kapoulkine (arseny.kapoulkine@gmail.com)
 * Report bugs and download new versions at http://pugixml.org/
 *
 * This library is distributed under the MIT License. See notice at the end
 * of this file.
 *
 * This work is based on the pugxml parser, which is:
 * Copyright (C) 2003, by Kristen Wegner (kristen@tima.net)
 */

#ifndef SOURCE_PUGIXML_CPP
#define SOURCE_PUGIXML_CPP

#include "pugixml.hpp"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#ifdef PUGIXML_WCHAR_MODE
#	include <wchar.h>
#endif

#ifndef PUGIXML_NO_XPATH
#	include <math.h>
#	include <float.h>
#endif

#ifndef PUGIXML_NO_STL
#	include <istream>
#	include <ostream>
#	include <string>
#endif

// For placement new
#include <new>

#ifdef _MSC_VER
#	pragma warning(push)
#	pragma warning(disable: 4127) // conditional expression is constant
#	pragma warning(disable: 4324) // structure was padded due to __declspec(align())
#	pragma warning(disable: 4702) // unreachable code
#	pragma warning(disable: 4996) // this function or variable may be unsafe
#endif

#if defined(_MSC_VER) && defined(__c2__)
#	pragma clang diagnostic push
#	pragma clang diagnostic ignored "-Wdeprecated" // this function or variable may be unsafe
#endif

#ifdef __INTEL_COMPILER
#	pragma warning(disable: 177) // function was declared but never referenced
#	pragma warning(disable: 279) // controlling expression is constant
#	pragma warning(disable: 1478 1786) // function was declared "deprecated"
#	pragma warning(disable: 1684) // conversion from pointer to same-sized integral type
#endif

#if defined(__BORLANDC__) && defined(PUGIXML_HEADER_ONLY)
#	pragma warn -8080 // symbol is declared but never used; disabling this inside push/pop bracket does not make the warning go away
#endif

#ifdef __BORLANDC__
#	pragma option push
#	pragma warn -8008 // condition is always false
#	pragma warn -8066 // unreachable code
#endif

#ifdef __SNC__
// Using diag_push/diag_pop does not disable the warnings inside templates due to a compiler bug
#	pragma diag_suppress=178 // function was declared but never referenced
#	pragma diag_suppress=237 // controlling expression is constant
#endif

// Inlining controls
#if defined(_MSC_VER) && _MSC_VER >= 1300
#	define PUGI__NO_INLINE __declspec(noinline)
#elif defined(__GNUC__)
#	define PUGI__NO_INLINE __attribute__((noinline))
#else
#	define PUGI__NO_INLINE
#endif

// Branch weight controls
#if defined(__GNUC__) && !defined(__c2__)
#	define PUGI__UNLIKELY(cond) __builtin_expect(cond, 0)
#else
#	define PUGI__UNLIKELY(cond) (cond)
#endif

// Simple static assertion
#define PUGI__STATIC_ASSERT(cond) { static const char condition_failed[(cond) ? 1 : -1] = {0}; (void)condition_failed[0]; }

// Digital Mars C++ bug workaround for passing char loaded from memory via stack
#ifdef __DMC__
#	define PUGI__DMC_VOLATILE volatile
#else
#	define PUGI__DMC_VOLATILE
#endif

// Integer sanitizer workaround
#ifdef __has_attribute
#	if __has_attribute(no_sanitize)
#		define PUGI__UNSIGNED_OVERFLOW __attribute__((no_sanitize("unsigned-integer-overflow")))
#	else
#		define PUGI__UNSIGNED_OVERFLOW
#	endif
#else
#	define PUGI__UNSIGNED_OVERFLOW
#endif

// Borland C++ bug workaround for not defining ::memcpy depending on header include order (can't always use std::memcpy because some compilers don't have it at all)
#if defined(__BORLANDC__) && !defined(__MEM_H_USING_LIST)
using std::memcpy;
using std::memmove;
using std::memset;
#endif

// Some MinGW versions have headers that erroneously omit LLONG_MIN/LLONG_MAX/ULLONG_MAX definitions in strict ANSI mode
#if defined(PUGIXML_HAS_LONG_LONG) && defined(__MINGW32__) && defined(__STRICT_ANSI__) && !defined(LLONG_MAX) && !defined(LLONG_MIN) && !defined(ULLONG_MAX)
#	define LLONG_MAX 9223372036854775807LL
#	define LLONG_MIN (-LLONG_MAX-1)
#	define ULLONG_MAX (2ULL*LLONG_MAX+1)
#endif

// In some environments MSVC is a compiler but the CRT lacks certain MSVC-specific features
#if defined(_MSC_VER) && !defined(__S3E__)
#	define PUGI__MSVC_CRT_VERSION _MSC_VER
#endif

// Not all platforms have snprintf; we define a wrapper that uses snprintf if possible. This only works with buffers with a known size.
#if __cplusplus >= 201103
#	define PUGI__SNPRINTF(buf, ...) snprintf(buf, sizeof(buf), __VA_ARGS__)
#elif defined(PUGI__MSVC_CRT_VERSION) && PUGI__MSVC_CRT_VERSION >= 1400
#	define PUGI__SNPRINTF(buf, ...) _snprintf_s(buf, _countof(buf), _TRUNCATE, __VA_ARGS__)
#else
#	define PUGI__SNPRINTF sprintf
#endif

// We put implementation details into an anonymous namespace in source mode, but have to keep it in non-anonymous namespace in header-only mode to prevent binary bloat.
#ifndef PUGIXML_HEADER_ONLY
#	define PUGI__NS_BEGIN namespace pugi { namespace impl {
#	define PUGI__NS_END } }
#	define PUGI__FN inline
#	define PUGI__FN_NO_INLINE inline
#else
#	if defined(_MSC_VER) && _MSC_VER < 1300 // MSVC6 seems to have an amusing bug with anonymous namespaces inside namespaces
#		define PUGI__NS_BEGIN namespace pugi { namespace impl {
#		define PUGI__NS_END } }
#	else
#		define PUGI__NS_BEGIN namespace pugi { namespace impl { namespace {
#		define PUGI__NS_END } } }
#	endif
#	define PUGI__FN
#	define PUGI__FN_NO_INLINE PUGI__NO_INLINE
#endif

// uintptr_t
#if (defined(_MSC_VER) && _MSC_VER < 1600) || (defined(__BORLANDC__) && __BORLANDC__ < 0x561)
namespace pugi
{
#	ifndef _UINTPTR_T_DEFINED
	typedef size_t uintptr_t;
#	endif

	typedef unsigned __int8 uint8_t;
	typedef unsigned __int16 uint16_t;
	typedef unsigned __int32 uint32_t;
}
#else
#	include <stdint.h>
#endif

// Memory allocation
PUGI__NS_BEGIN
	PUGI__FN void* default_allocate(size_t size)
	{
		return malloc(size);
	}

	PUGI__FN void default_deallocate(void* ptr)
	{
		free(ptr);
	}

	template <typename T>
	struct xml_memory_management_function_storage
	{
		static allocation_function allocate;
		static deallocation_function deallocate;
	};

	// Global allocation functions are stored in class statics so that in header mode linker deduplicates them
	// Without a template<> we'll get multiple definitions of the same static
	template <typename T> allocation_function xml_memory_management_function_storage<T>::allocate = default_allocate;
	template <typename T> deallocation_function xml_memory_management_function_storage<T>::deallocate = default_deallocate;

	typedef xml_memory_management_function_storage<int> xml_memory;
PUGI__NS_END

// String utilities
PUGI__NS_BEGIN
	// Get string length
	PUGI__FN size_t strlength(const char_t* s)
	{
		assert(s);

	#ifdef PUGIXML_WCHAR_MODE
		return wcslen(s);
	#else
		return strlen(s);
	#endif
	}

	// Compare two strings
	PUGI__FN bool strequal(const char_t* src, const char_t* dst)
	{
		assert(src && dst);

	#ifdef PUGIXML_WCHAR_MODE
		return wcscmp(src, dst) == 0;
	#else
		return strcmp(src, dst) == 0;
	#endif
	}

	// Compare lhs with [rhs_begin, rhs_end)
	PUGI__FN bool strequalrange(const char_t* lhs, const char_t* rhs, size_t count)
	{
		for (size_t i = 0; i < count; ++i)
			if (lhs[i] != rhs[i])
				return false;

		return lhs[count] == 0;
	}

	// Get length of wide string, even if CRT lacks wide character support
	PUGI__FN size_t strlength_wide(const wchar_t* s)
	{
		assert(s);

	#ifdef PUGIXML_WCHAR_MODE
		return wcslen(s);
	#else
		const wchar_t* end = s;
		while (*end) end++;
		return static_cast<size_t>(end - s);
	#endif
	}
PUGI__NS_END

// auto_ptr-like object for exception recovery
PUGI__NS_BEGIN
	template <typename T> struct auto_deleter
	{
		typedef void (*D)(T*);

		T* data;
		D deleter;

		auto_deleter(T* data_, D deleter_): data(data_), deleter(deleter_)
		{
		}

		~auto_deleter()
		{
			if (data) deleter(data);
		}

		T* release()
		{
			T* result = data;
			data = 0;
			return result;
		}
	};
PUGI__NS_END

PUGI__NS_BEGIN

	struct xml_allocator;

	struct xml_memory_page
	{
		static xml_memory_page* construct(void* memory)
		{
			xml_memory_page* result = static_cast<xml_memory_page*>(memory);

			result->allocator = 0;
			result->prev = 0;
			result->next = 0;
			result->busy_size = 0;
			result->freed_size = 0;

			return result;
		}

		xml_allocator* allocator;

		xml_memory_page* prev;
		xml_memory_page* next;

		size_t busy_size;
		size_t freed_size;

	};

	static const size_t xml_memory_page_size =
	#ifdef PUGIXML_MEMORY_PAGE_SIZE
		(PUGIXML_MEMORY_PAGE_SIZE)
	#else
		32768
	#endif
		- sizeof(xml_memory_page);

	struct xml_memory_string_header
	{
		uint16_t page_offset; // offset from page->data
		uint16_t full_size; // 0 if string occupies whole page
	};

	struct xml_allocator
	{
		xml_allocator(xml_memory_page* root): _root(root), _busy_size(root->busy_size)
		{
		}

		xml_memory_page* allocate_page(size_t data_size)
		{
			size_t size = sizeof(xml_memory_page) + data_size;

			// allocate block with some alignment, leaving memory for worst-case padding
			void* memory = xml_memory::allocate(size);
			if (!memory) return 0;

			// prepare page structure
			xml_memory_page* page = xml_memory_page::construct(memory);
			assert(page);

			page->allocator = _root->allocator;

			return page;
		}

		static void deallocate_page(xml_memory_page* page)
		{
			xml_memory::deallocate(page);
		}

		void* allocate_memory_oob(size_t size, xml_memory_page*& out_page);

		void* allocate_memory(size_t size, xml_memory_page*& out_page)
		{
			if (PUGI__UNLIKELY(_busy_size + size > xml_memory_page_size))
				return allocate_memory_oob(size, out_page);

			void* buf = reinterpret_cast<char*>(_root) + sizeof(xml_memory_page) + _busy_size;

			_busy_size += size;

			out_page = _root;

			return buf;
		}

		void* allocate_object(size_t size, xml_memory_page*& out_page)
		{
			return allocate_memory(size, out_page);
		}

		void deallocate_memory(void* ptr, size_t size, xml_memory_page* page)
		{
			if (page == _root) page->busy_size = _busy_size;

			assert(ptr >= reinterpret_cast<char*>(page) + sizeof(xml_memory_page) && ptr < reinterpret_cast<char*>(page) + sizeof(xml_memory_page) + page->busy_size);
			(void)!ptr;

			page->freed_size += size;
			assert(page->freed_size <= page->busy_size);

			if (page->freed_size == page->busy_size)
			{
				if (page->next == 0)
				{
					assert(_root == page);

					// top page freed, just reset sizes
					page->busy_size = 0;
					page->freed_size = 0;

					_busy_size = 0;
				}
				else
				{
					assert(_root != page);
					assert(page->prev);

					// remove from the list
					page->prev->next = page->next;
					page->next->prev = page->prev;

					// deallocate
					deallocate_page(page);
				}
			}
		}

		char_t* allocate_string(size_t length)
		{
			static const size_t max_encoded_offset = (1 << 16) * xml_memory_block_alignment;

			PUGI__STATIC_ASSERT(xml_memory_page_size <= max_encoded_offset);

			// allocate memory for string and header block
			size_t size = sizeof(xml_memory_string_header) + length * sizeof(char_t);

			// round size up to block alignment boundary
			size_t full_size = (size + (xml_memory_block_alignment - 1)) & ~(xml_memory_block_alignment - 1);

			xml_memory_page* page;
			xml_memory_string_header* header = static_cast<xml_memory_string_header*>(allocate_memory(full_size, page));

			if (!header) return 0;

			// setup header
			ptrdiff_t page_offset = reinterpret_cast<char*>(header) - reinterpret_cast<char*>(page) - sizeof(xml_memory_page);

			assert(page_offset % xml_memory_block_alignment == 0);
			assert(page_offset >= 0 && static_cast<size_t>(page_offset) < max_encoded_offset);
			header->page_offset = static_cast<uint16_t>(static_cast<size_t>(page_offset) / xml_memory_block_alignment);

			// full_size == 0 for large strings that occupy the whole page
			assert(full_size % xml_memory_block_alignment == 0);
			assert(full_size < max_encoded_offset || (page->busy_size == full_size && page_offset == 0));
			header->full_size = static_cast<uint16_t>(full_size < max_encoded_offset ? full_size / xml_memory_block_alignment : 0);

			// round-trip through void* to avoid 'cast increases required alignment of target type' warning
			// header is guaranteed a pointer-sized alignment, which should be enough for char_t
			return static_cast<char_t*>(static_cast<void*>(header + 1));
		}

		void deallocate_string(char_t* string)
		{
			// this function casts pointers through void* to avoid 'cast increases required alignment of target type' warnings
			// we're guaranteed the proper (pointer-sized) alignment on the input string if it was allocated via allocate_string

			// get header
			xml_memory_string_header* header = static_cast<xml_memory_string_header*>(static_cast<void*>(string)) - 1;
			assert(header);

			// deallocate
			size_t page_offset = sizeof(xml_memory_page) + header->page_offset * xml_memory_block_alignment;
			xml_memory_page* page = reinterpret_cast<xml_memory_page*>(static_cast<void*>(reinterpret_cast<char*>(header) - page_offset));

			// if full_size == 0 then this string occupies the whole page
			size_t full_size = header->full_size == 0 ? page->busy_size : header->full_size * xml_memory_block_alignment;

			deallocate_memory(header, full_size, page);
		}

		bool reserve()
		{
			return true;
		}

		xml_memory_page* _root;
		size_t _busy_size;
	};

	PUGI__FN_NO_INLINE void* xml_allocator::allocate_memory_oob(size_t size, xml_memory_page*& out_page)
	{
		const size_t large_allocation_threshold = xml_memory_page_size / 4;

		xml_memory_page* page = allocate_page(size <= large_allocation_threshold ? xml_memory_page_size : size);
		out_page = page;

		if (!page) return 0;

		if (size <= large_allocation_threshold)
		{
			_root->busy_size = _busy_size;

			// insert page at the end of linked list
			page->prev = _root;
			_root->next = page;
			_root = page;

			_busy_size = size;
		}
		else
		{
			// insert page before the end of linked list, so that it is deleted as soon as possible
			// the last page is not deleted even if it's empty (see deallocate_memory)
			assert(_root->prev);

			page->prev = _root->prev;
			page->next = _root;

			_root->prev->next = page;
			_root->prev = page;

			page->busy_size = size;
		}

		return reinterpret_cast<char*>(page) + sizeof(xml_memory_page);
	}
PUGI__NS_END


PUGI__NS_BEGIN
	struct xml_extra_buffer
	{
		char_t* buffer;
		xml_extra_buffer* next;
	};

	struct xml_document_struct: public xml_node_struct, public xml_allocator
	{
		xml_document_struct(xml_memory_page* page): xml_node_struct(page, node_document), xml_allocator(page), buffer(0), extra_buffers(0)
		{
		}

		const char_t* buffer;

		xml_extra_buffer* extra_buffers;
	};

	template <typename Object> inline xml_allocator& get_allocator(const Object* object)
	{
		assert(object);

		return *PUGI__GETPAGE(object)->allocator;
	}

	template <typename Object> inline xml_document_struct& get_document(const Object* object)
	{
		assert(object);

		return *static_cast<xml_document_struct*>(PUGI__GETPAGE(object)->allocator);
	}
PUGI__NS_END

namespace pugi {
  bool xml_node_struct_ref::is_page_contents_shared() {
    return (impl::get_document(Node).header & impl::xml_memory_page_contents_shared_mask) != 0;
  }

  bool xml_attribute_struct_ref::is_page_contents_shared() {
    return (impl::get_document(Node).header & impl::xml_memory_page_contents_shared_mask) != 0;
  }
}

PUGI__NS_BEGIN
	enum chartype_t
	{
		ct_parse_pcdata = 1,	// \0, &, \r, <
		ct_parse_attr = 2,		// \0, &, \r, ', "
		ct_parse_attr_ws = 4,	// \0, &, \r, ', ", \n, tab
		ct_space = 8,			// \r, \n, space, tab
		ct_parse_cdata = 16,	// \0, ], >, \r
		ct_parse_comment = 32,	// \0, -, >, \r
		ct_symbol = 64,			// Any symbol > 127, a-z, A-Z, 0-9, _, :, -, .
		ct_start_symbol = 128	// Any symbol > 127, a-z, A-Z, _, :
	};

	static const unsigned char chartype_table[256] =
	{
		55,  0,   0,   0,   0,   0,   0,   0,      0,   12,  12,  0,   0,   63,  0,   0,   // 0-15
		0,   0,   0,   0,   0,   0,   0,   0,      0,   0,   0,   0,   0,   0,   0,   0,   // 16-31
		8,   0,   6,   0,   0,   0,   7,   6,      0,   0,   0,   0,   0,   96,  64,  0,   // 32-47
		64,  64,  64,  64,  64,  64,  64,  64,     64,  64,  192, 0,   1,   0,   48,  0,   // 48-63
		0,   192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 192, 192, 192, 192, 192, // 64-79
		192, 192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 0,   0,   16,  0,   192, // 80-95
		0,   192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 192, 192, 192, 192, 192, // 96-111
		192, 192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 0, 0, 0, 0, 0,           // 112-127

		192, 192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 192, 192, 192, 192, 192, // 128+
		192, 192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 192, 192, 192, 192, 192,
		192, 192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 192, 192, 192, 192, 192,
		192, 192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 192, 192, 192, 192, 192,
		192, 192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 192, 192, 192, 192, 192,
		192, 192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 192, 192, 192, 192, 192,
		192, 192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 192, 192, 192, 192, 192,
		192, 192, 192, 192, 192, 192, 192, 192,    192, 192, 192, 192, 192, 192, 192, 192
	};

	enum chartypex_t
	{
		ctx_special_pcdata = 1,   // Any symbol >= 0 and < 32 (except \t, \r, \n), &, <, >
		ctx_special_attr = 2,     // Any symbol >= 0 and < 32 (except \t), &, <, >, "
		ctx_start_symbol = 4,	  // Any symbol > 127, a-z, A-Z, _
		ctx_digit = 8,			  // 0-9
		ctx_symbol = 16			  // Any symbol > 127, a-z, A-Z, 0-9, _, -, .
	};

	static const unsigned char chartypex_table[256] =
	{
		3,  3,  3,  3,  3,  3,  3,  3,     3,  0,  2,  3,  3,  2,  3,  3,     // 0-15
		3,  3,  3,  3,  3,  3,  3,  3,     3,  3,  3,  3,  3,  3,  3,  3,     // 16-31
		0,  0,  2,  0,  0,  0,  3,  0,     0,  0,  0,  0,  0, 16, 16,  0,     // 32-47
		24, 24, 24, 24, 24, 24, 24, 24,    24, 24, 0,  0,  3,  0,  3,  0,     // 48-63

		0,  20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 20, 20, 20, 20, 20,    // 64-79
		20, 20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 0,  0,  0,  0,  20,    // 80-95
		0,  20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 20, 20, 20, 20, 20,    // 96-111
		20, 20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 0,  0,  0,  0,  0,     // 112-127

		20, 20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 20, 20, 20, 20, 20,    // 128+
		20, 20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 20, 20, 20, 20, 20,
		20, 20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 20, 20, 20, 20, 20,
		20, 20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 20, 20, 20, 20, 20,
		20, 20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 20, 20, 20, 20, 20,
		20, 20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 20, 20, 20, 20, 20,
		20, 20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 20, 20, 20, 20, 20,
		20, 20, 20, 20, 20, 20, 20, 20,    20, 20, 20, 20, 20, 20, 20, 20
	};

#ifdef PUGIXML_WCHAR_MODE
	#define PUGI__IS_CHARTYPE_IMPL(c, ct, table) ((static_cast<unsigned int>(c) < 128 ? table[static_cast<unsigned int>(c)] : table[128]) & (ct))
#else
	#define PUGI__IS_CHARTYPE_IMPL(c, ct, table) (table[static_cast<unsigned char>(c)] & (ct))
#endif

	#define PUGI__IS_CHARTYPE(c, ct) PUGI__IS_CHARTYPE_IMPL(c, ct, chartype_table)
	#define PUGI__IS_CHARTYPEX(c, ct) PUGI__IS_CHARTYPE_IMPL(c, ct, chartypex_table)

PUGI__NS_END

#ifndef PUGIXML_NO_XPATH
// STL replacements
PUGI__NS_BEGIN
	struct equal_to
	{
		template <typename T> bool operator()(const T& lhs, const T& rhs) const
		{
			return lhs == rhs;
		}
	};

	struct not_equal_to
	{
		template <typename T> bool operator()(const T& lhs, const T& rhs) const
		{
			return lhs != rhs;
		}
	};

	struct less
	{
		template <typename T> bool operator()(const T& lhs, const T& rhs) const
		{
			return lhs < rhs;
		}
	};

	struct less_equal
	{
		template <typename T> bool operator()(const T& lhs, const T& rhs) const
		{
			return lhs <= rhs;
		}
	};

	template <typename T> void swap(T& lhs, T& rhs)
	{
		T temp = lhs;
		lhs = rhs;
		rhs = temp;
	}

	template <typename I, typename Pred> I min_element(I begin, I end, const Pred& pred)
	{
		I result = begin;

		for (I it = begin + 1; it != end; ++it)
			if (pred(*it, *result))
				result = it;

		return result;
	}

	template <typename I> void reverse(I begin, I end)
	{
		while (end - begin > 1) swap(*begin++, *--end);
	}

	template <typename I> I unique(I begin, I end)
	{
		// fast skip head
		while (end - begin > 1 && *begin != *(begin + 1)) begin++;

		if (begin == end) return begin;

		// last written element
		I write = begin++;

		// merge unique elements
		while (begin != end)
		{
			if (*begin != *write)
				*++write = *begin++;
			else
				begin++;
		}

		// past-the-end (write points to live element)
		return write + 1;
	}

	template <typename T, typename Pred> void insertion_sort(T* begin, T* end, const Pred& pred)
	{
		if (begin == end)
			return;

		for (T* it = begin + 1; it != end; ++it)
		{
			T val = *it;
			T* hole = it;

			// move hole backwards
			while (hole > begin && pred(val, *(hole - 1)))
			{
				*hole = *(hole - 1);
				hole--;
			}

			// fill hole with element
			*hole = val;
		}
	}

	template <typename I, typename Pred> I median3(I first, I middle, I last, const Pred& pred)
	{
		if (pred(*middle, *first)) swap(middle, first);
		if (pred(*last, *middle)) swap(last, middle);
		if (pred(*middle, *first)) swap(middle, first);

		return middle;
	}

	template <typename T, typename Pred> void partition3(T* begin, T* end, T pivot, const Pred& pred, T** out_eqbeg, T** out_eqend)
	{
		// invariant: array is split into 4 groups: = < ? > (each variable denotes the boundary between the groups)
		T* eq = begin;
		T* lt = begin;
		T* gt = end;

		while (lt < gt)
		{
			if (pred(*lt, pivot))
				lt++;
			else if (*lt == pivot)
				swap(*eq++, *lt++);
			else
				swap(*lt, *--gt);
		}

		// we now have just 4 groups: = < >; move equal elements to the middle
		T* eqbeg = gt;

		for (T* it = begin; it != eq; ++it)
			swap(*it, *--eqbeg);

		*out_eqbeg = eqbeg;
		*out_eqend = gt;
	}

	template <typename I, typename Pred> void sort(I begin, I end, const Pred& pred)
	{
		// sort large chunks
		while (end - begin > 16)
		{
			// find median element
			I middle = begin + (end - begin) / 2;
			I median = median3(begin, middle, end - 1, pred);

			// partition in three chunks (< = >)
			I eqbeg, eqend;
			partition3(begin, end, *median, pred, &eqbeg, &eqend);

			// loop on larger half
			if (eqbeg - begin > end - eqend)
			{
				sort(eqend, end, pred);
				end = eqbeg;
			}
			else
			{
				sort(begin, eqbeg, pred);
				begin = eqend;
			}
		}

		// insertion sort small chunk
		insertion_sort(begin, end, pred);
	}
PUGI__NS_END

// Allocator used for AST and evaluation stacks
PUGI__NS_BEGIN
	static const size_t xpath_memory_page_size =
	#ifdef PUGIXML_MEMORY_XPATH_PAGE_SIZE
		PUGIXML_MEMORY_XPATH_PAGE_SIZE
	#else
		4096
	#endif
		;

	static const uintptr_t xpath_memory_block_alignment = sizeof(double) > sizeof(void*) ? sizeof(double) : sizeof(void*);

	struct xpath_memory_block
	{
		xpath_memory_block* next;
		size_t capacity;

		union
		{
			char data[xpath_memory_page_size];
			double alignment;
		};
	};

	struct xpath_allocator
	{
		xpath_memory_block* _root;
		size_t _root_size;
		bool* _error;

		xpath_allocator(xpath_memory_block* root, bool* error = 0): _root(root), _root_size(0), _error(error)
		{
		}

		void* allocate(size_t size)
		{
			// round size up to block alignment boundary
			size = (size + xpath_memory_block_alignment - 1) & ~(xpath_memory_block_alignment - 1);

			if (_root_size + size <= _root->capacity)
			{
				void* buf = &_root->data[0] + _root_size;
				_root_size += size;
				return buf;
			}
			else
			{
				// make sure we have at least 1/4th of the page free after allocation to satisfy subsequent allocation requests
				size_t block_capacity_base = sizeof(_root->data);
				size_t block_capacity_req = size + block_capacity_base / 4;
				size_t block_capacity = (block_capacity_base > block_capacity_req) ? block_capacity_base : block_capacity_req;

				size_t block_size = block_capacity + offsetof(xpath_memory_block, data);

				xpath_memory_block* block = static_cast<xpath_memory_block*>(xml_memory::allocate(block_size));
				if (!block)
				{
					if (_error) *_error = true;
					return 0;
				}

				block->next = _root;
				block->capacity = block_capacity;

				_root = block;
				_root_size = size;

				return block->data;
			}
		}

		void* reallocate(void* ptr, size_t old_size, size_t new_size)
		{
			// round size up to block alignment boundary
			old_size = (old_size + xpath_memory_block_alignment - 1) & ~(xpath_memory_block_alignment - 1);
			new_size = (new_size + xpath_memory_block_alignment - 1) & ~(xpath_memory_block_alignment - 1);

			// we can only reallocate the last object
			assert(ptr == 0 || static_cast<char*>(ptr) + old_size == &_root->data[0] + _root_size);

			// try to reallocate the object inplace
			if (ptr && _root_size - old_size + new_size <= _root->capacity)
			{
				_root_size = _root_size - old_size + new_size;
				return ptr;
			}

			// allocate a new block
			void* result = allocate(new_size);
			if (!result) return 0;

			// we have a new block
			if (ptr)
			{
				// copy old data (we only support growing)
				assert(new_size >= old_size);
				memcpy(result, ptr, old_size);

				// free the previous page if it had no other objects
				assert(_root->data == result);
				assert(_root->next);

				if (_root->next->data == ptr)
				{
					// deallocate the whole page, unless it was the first one
					xpath_memory_block* next = _root->next->next;

					if (next)
					{
						xml_memory::deallocate(_root->next);
						_root->next = next;
					}
				}
			}

			return result;
		}

		void revert(const xpath_allocator& state)
		{
			// free all new pages
			xpath_memory_block* cur = _root;

			while (cur != state._root)
			{
				xpath_memory_block* next = cur->next;

				xml_memory::deallocate(cur);

				cur = next;
			}

			// restore state
			_root = state._root;
			_root_size = state._root_size;
		}

		void release()
		{
			xpath_memory_block* cur = _root;
			assert(cur);

			while (cur->next)
			{
				xpath_memory_block* next = cur->next;

				xml_memory::deallocate(cur);

				cur = next;
			}
		}
	};

	struct xpath_allocator_capture
	{
		xpath_allocator_capture(xpath_allocator* alloc): _target(alloc), _state(*alloc)
		{
		}

		~xpath_allocator_capture()
		{
			_target->revert(_state);
		}

		xpath_allocator* _target;
		xpath_allocator _state;
	};

	struct xpath_stack
	{
		xpath_allocator* result;
		xpath_allocator* temp;
	};

	struct xpath_stack_data
	{
		xpath_memory_block blocks[2];
		xpath_allocator result;
		xpath_allocator temp;
		xpath_stack stack;
		bool oom;

		xpath_stack_data(): result(blocks + 0, &oom), temp(blocks + 1, &oom), oom(false)
		{
			blocks[0].next = blocks[1].next = 0;
			blocks[0].capacity = blocks[1].capacity = sizeof(blocks[0].data);

			stack.result = &result;
			stack.temp = &temp;
		}

		~xpath_stack_data()
		{
			result.release();
			temp.release();
		}
	};
PUGI__NS_END

// String class
PUGI__NS_BEGIN
	class xpath_string
	{
		const char_t* _buffer;
		bool _uses_heap;
		size_t _length_heap;

		static char_t* duplicate_string(const char_t* string, size_t length, xpath_allocator* alloc)
		{
			char_t* result = static_cast<char_t*>(alloc->allocate((length + 1) * sizeof(char_t)));
			if (!result) return 0;

			memcpy(result, string, length * sizeof(char_t));
			result[length] = 0;

			return result;
		}

		xpath_string(const char_t* buffer, bool uses_heap_, size_t length_heap): _buffer(buffer), _uses_heap(uses_heap_), _length_heap(length_heap)
		{
		}

	public:
		static xpath_string from_const(const char_t* str)
		{
			return xpath_string(str, false, 0);
		}

		static xpath_string from_heap_preallocated(const char_t* begin, const char_t* end)
		{
			assert(begin <= end && *end == 0);

			return xpath_string(begin, true, static_cast<size_t>(end - begin));
		}

		static xpath_string from_heap(const char_t* begin, const char_t* end, xpath_allocator* alloc)
		{
			assert(begin <= end);

			if (begin == end)
				return xpath_string();

			size_t length = static_cast<size_t>(end - begin);
			const char_t* data = duplicate_string(begin, length, alloc);

			return data ? xpath_string(data, true, length) : xpath_string();
		}

		xpath_string(): _buffer(PUGIXML_TEXT("")), _uses_heap(false), _length_heap(0)
		{
		}

		void append(const xpath_string& o, xpath_allocator* alloc)
		{
			// skip empty sources
			if (!*o._buffer) return;

			// fast append for constant empty target and constant source
			if (!*_buffer && !_uses_heap && !o._uses_heap)
			{
				_buffer = o._buffer;
			}
			else
			{
				// need to make heap copy
				size_t target_length = length();
				size_t source_length = o.length();
				size_t result_length = target_length + source_length;

				// allocate new buffer
				char_t* result = static_cast<char_t*>(alloc->reallocate(_uses_heap ? const_cast<char_t*>(_buffer) : 0, (target_length + 1) * sizeof(char_t), (result_length + 1) * sizeof(char_t)));
				if (!result) return;

				// append first string to the new buffer in case there was no reallocation
				if (!_uses_heap) memcpy(result, _buffer, target_length * sizeof(char_t));

				// append second string to the new buffer
				memcpy(result + target_length, o._buffer, source_length * sizeof(char_t));
				result[result_length] = 0;

				// finalize
				_buffer = result;
				_uses_heap = true;
				_length_heap = result_length;
			}
		}

		const char_t* c_str() const
		{
			return _buffer;
		}

		size_t length() const
		{
			return _uses_heap ? _length_heap : strlength(_buffer);
		}

		char_t* data(xpath_allocator* alloc)
		{
			// make private heap copy
			if (!_uses_heap)
			{
				size_t length_ = strlength(_buffer);
				const char_t* data_ = duplicate_string(_buffer, length_, alloc);

				if (!data_) return 0;

				_buffer = data_;
				_uses_heap = true;
				_length_heap = length_;
			}

			return const_cast<char_t*>(_buffer);
		}

		bool empty() const
		{
			return *_buffer == 0;
		}

		bool operator==(const xpath_string& o) const
		{
			return strequal(_buffer, o._buffer);
		}

		bool operator!=(const xpath_string& o) const
		{
			return !strequal(_buffer, o._buffer);
		}

		bool uses_heap() const
		{
			return _uses_heap;
		}
	};
PUGI__NS_END

PUGI__NS_BEGIN
	PUGI__FN bool starts_with(const char_t* string, const char_t* pattern)
	{
		while (*pattern && *string == *pattern)
		{
			string++;
			pattern++;
		}

		return *pattern == 0;
	}

	PUGI__FN const char_t* find_char(const char_t* s, char_t c)
	{
	#ifdef PUGIXML_WCHAR_MODE
		return wcschr(s, c);
	#else
		return strchr(s, c);
	#endif
	}

	PUGI__FN const char_t* find_substring(const char_t* s, const char_t* p)
	{
	#ifdef PUGIXML_WCHAR_MODE
		// MSVC6 wcsstr bug workaround (if s is empty it always returns 0)
		return (*p == 0) ? s : wcsstr(s, p);
	#else
		return strstr(s, p);
	#endif
	}

	// Converts symbol to lower case, if it is an ASCII one
	PUGI__FN char_t tolower_ascii(char_t ch)
	{
		return static_cast<unsigned int>(ch - 'A') < 26 ? static_cast<char_t>(ch | ' ') : ch;
	}

	PUGI__FN xpath_string string_value(const xpath_node& na, xpath_allocator* alloc)
	{
		if (na.attribute())
			return xpath_string::from_const(na.attribute().value());
		else
		{
			xml_node n = na.node();

			switch (n.type())
			{
			case node_pcdata:
			case node_cdata:
			case node_comment:
			case node_pi:
				return xpath_string::from_const(n.value());

			case node_document:
			case node_element:
			{
				xpath_string result;

				// element nodes can have value if parse_embed_pcdata was used
				if (n.value()[0])
					result.append(xpath_string::from_const(n.value()), alloc);

				xml_node cur = n.first_child();

				while (cur && cur != n)
				{
					if (cur.type() == node_pcdata || cur.type() == node_cdata)
						result.append(xpath_string::from_const(cur.value()), alloc);

					if (cur.first_child())
						cur = cur.first_child();
					else if (cur.next_sibling())
						cur = cur.next_sibling();
					else
					{
						while (!cur.next_sibling() && cur != n)
							cur = cur.parent();

						if (cur != n) cur = cur.next_sibling();
					}
				}

				return result;
			}

			default:
				return xpath_string();
			}
		}
	}

	PUGI__FN bool node_is_before_sibling_lol(xml_node_struct_ref ln, xml_node_struct_ref rn)
	{
        assert(ln.parent() == rn.parent());

		// there is no common ancestor (the shared parent is null), nodes are from different documents
		if (!ln.parent()) return ln < rn;

		// determine sibling order
		xml_node_struct_ref ls = ln;
		xml_node_struct_ref rs = rn;

		while (ls && rs)
		{
			if (ls == rn) return true;
			if (rs == ln) return false;

			ls = ls.next_sibling();
			rs = rs.next_sibling();
		}

		// if rn sibling chain ended ln must be before rn
		return !rs;
	}

	PUGI__FN bool node_is_before_sibling(xml_node_struct* ln, xml_node_struct* rn)
	{
		assert(ln->parent == rn->parent);

		// there is no common ancestor (the shared parent is null), nodes are from different documents
		if (!ln->parent) return ln < rn;

		// determine sibling order
		xml_node_struct* ls = ln;
		xml_node_struct* rs = rn;

		while (ls && rs)
		{
			if (ls == rn) return true;
			if (rs == ln) return false;

			ls = ls->next_sibling;
			rs = rs->next_sibling;
		}

		// if rn sibling chain ended ln must be before rn
		return !rs;
	}

	PUGI__FN bool node_is_before_lol(xml_node_struct_ref ln, xml_node_struct_ref rn)
	{
		// find common ancestor at the same depth, if any
		xml_node_struct_ref lp = ln;
		xml_node_struct_ref rp = rn;

		while (lp && rp && lp.parent() != rp.parent())
		{
			lp = lp.parent();
			rp = rp.parent();
		}

		// parents are the same!
		if (lp && rp) return node_is_before_sibling_lol(lp, rp);

		// nodes are at different depths, need to normalize heights
		bool left_higher = !lp;

		while (lp)
		{
			lp = lp.parent();
			ln = ln.parent();
		}

		while (rp)
		{
			rp = rp.parent();
			rn = rn.parent();
		}

		// one node is the ancestor of the other
		if (ln == rn) return left_higher;

		// find common ancestor... again
		while (ln.parent() != rn.parent())
		{
			ln = ln.parent();
			rn = rn.parent();
		}

		return node_is_before_sibling_lol(ln, rn);
	}


	PUGI__FN bool node_is_before(xml_node_struct* ln, xml_node_struct* rn)
	{
		// find common ancestor at the same depth, if any
		xml_node_struct* lp = ln;
		xml_node_struct* rp = rn;

		while (lp && rp && lp->parent != rp->parent)
		{
			lp = lp->parent;
			rp = rp->parent;
		}

		// parents are the same!
		if (lp && rp) return node_is_before_sibling(lp, rp);

		// nodes are at different depths, need to normalize heights
		bool left_higher = !lp;

		while (lp)
		{
			lp = lp->parent;
			ln = ln->parent;
		}

		while (rp)
		{
			rp = rp->parent;
			rn = rn->parent;
		}

		// one node is the ancestor of the other
		if (ln == rn) return left_higher;

		// find common ancestor... again
		while (ln->parent != rn->parent)
		{
			ln = ln->parent;
			rn = rn->parent;
		}

		return node_is_before_sibling(ln, rn);
	}

	PUGI__FN bool node_is_ancestor_lol(xml_node_struct_ref parent, xml_node_struct_ref node)
	{
		while (node && node != parent) node = node.parent();

		return parent && node == parent;
	}

	PUGI__FN bool node_is_ancestor(xml_node_struct* parent, xml_node_struct* node)
	{
		while (node && node != parent) node = node->parent;

		return parent && node == parent;
	}

	PUGI__FN const void* document_buffer_order_lol(const xpath_node& xnode)
	{
		xml_node_struct_ref node = xnode.node().internal_object_lol();

		if (node)
		{
          if (!node.is_page_contents_shared())
			{
              if (node.name() && !node.is_page_name_allocated_or_shared()) return node.name();
              if (node.value() && !node.is_page_value_allocated_or_shared()) return node.value();
			}

			return 0;
		}

		xml_attribute_struct_ref attr = xnode.attribute().internal_object_lol();

		if (attr)
		{
            if (!attr.is_page_contents_shared())
			{
              if (attr.name() && !attr.is_page_name_allocated_or_shared()) return attr.name();
              if (attr.value() && !attr.is_page_value_allocated_or_shared()) return attr.value();
			}

			return 0;
		}

		return 0;
	}

	PUGI__FN const void* document_buffer_order(const xpath_node& xnode)
	{
		xml_node_struct* node = xnode.node().internal_object();

		if (node)
		{
			if ((get_document(node).header & xml_memory_page_contents_shared_mask) == 0)
			{
				if (node->name && (node->header & impl::xml_memory_page_name_allocated_or_shared_mask) == 0) return node->name;
				if (node->value && (node->header & impl::xml_memory_page_value_allocated_or_shared_mask) == 0) return node->value;
			}

			return 0;
		}

		xml_attribute_struct* attr = xnode.attribute().internal_object();

		if (attr)
		{
			if ((get_document(attr).header & xml_memory_page_contents_shared_mask) == 0)
			{
				if ((attr->header & impl::xml_memory_page_name_allocated_or_shared_mask) == 0) return attr->name;
				if ((attr->header & impl::xml_memory_page_value_allocated_or_shared_mask) == 0) return attr->value;
			}

			return 0;
		}

		return 0;
	}

	struct document_order_comparator
	{
		bool operator()(const xpath_node& lhs, const xpath_node& rhs) const
		{
			// optimized document order based check
			const void* lo = document_buffer_order(lhs);
			const void* ro = document_buffer_order(rhs);

			if (lo && ro) return lo < ro;

			// slow comparison
			xml_node ln = lhs.node(), rn = rhs.node();

			// compare attributes
			if (lhs.attribute() && rhs.attribute())
			{
				// shared parent
				if (lhs.parent() == rhs.parent())
				{
					// determine sibling order
					for (xml_attribute a = lhs.attribute(); a; a = a.next_attribute())
						if (a == rhs.attribute())
							return true;

					return false;
				}

				// compare attribute parents
				ln = lhs.parent();
				rn = rhs.parent();
			}
			else if (lhs.attribute())
			{
				// attributes go after the parent element
				if (lhs.parent() == rhs.node()) return false;

				ln = lhs.parent();
			}
			else if (rhs.attribute())
			{
				// attributes go after the parent element
				if (rhs.parent() == lhs.node()) return true;

				rn = rhs.parent();
			}

			if (ln == rn) return false;

			if (!ln || !rn) return ln < rn;

			return node_is_before_lol(ln.internal_object_lol(), rn.internal_object_lol());
		}
	};

	struct duplicate_comparator
	{
		bool operator()(const xpath_node& lhs, const xpath_node& rhs) const
		{
			if (lhs.attribute()) return rhs.attribute() ? lhs.attribute() < rhs.attribute() : true;
			else return rhs.attribute() ? false : lhs.node() < rhs.node();
		}
	};

	PUGI__FN double gen_nan()
	{
	#if defined(__STDC_IEC_559__) || ((FLT_RADIX - 0 == 2) && (FLT_MAX_EXP - 0 == 128) && (FLT_MANT_DIG - 0 == 24))
		PUGI__STATIC_ASSERT(sizeof(float) == sizeof(uint32_t));
		typedef uint32_t UI; // BCC5 workaround
		union { float f; UI i; } u;
		u.i = 0x7fc00000;
		return u.f;
	#else
		// fallback
		const volatile double zero = 0.0;
		return zero / zero;
	#endif
	}

	PUGI__FN bool is_nan(double value)
	{
	#if defined(PUGI__MSVC_CRT_VERSION) || defined(__BORLANDC__)
		return !!_isnan(value);
	#elif defined(fpclassify) && defined(FP_NAN)
		return fpclassify(value) == FP_NAN;
	#else
		// fallback
		const volatile double v = value;
		return v != v;
	#endif
	}

	PUGI__FN const char_t* convert_number_to_string_special(double value)
	{
	#if defined(PUGI__MSVC_CRT_VERSION) || defined(__BORLANDC__)
		if (_finite(value)) return (value == 0) ? PUGIXML_TEXT("0") : 0;
		if (_isnan(value)) return PUGIXML_TEXT("NaN");
		return value > 0 ? PUGIXML_TEXT("Infinity") : PUGIXML_TEXT("-Infinity");
	#elif defined(fpclassify) && defined(FP_NAN) && defined(FP_INFINITE) && defined(FP_ZERO)
		switch (fpclassify(value))
		{
		case FP_NAN:
			return PUGIXML_TEXT("NaN");

		case FP_INFINITE:
			return value > 0 ? PUGIXML_TEXT("Infinity") : PUGIXML_TEXT("-Infinity");

		case FP_ZERO:
			return PUGIXML_TEXT("0");

		default:
			return 0;
		}
	#else
		// fallback
		const volatile double v = value;

		if (v == 0) return PUGIXML_TEXT("0");
		if (v != v) return PUGIXML_TEXT("NaN");
		if (v * 2 == v) return value > 0 ? PUGIXML_TEXT("Infinity") : PUGIXML_TEXT("-Infinity");
		return 0;
	#endif
	}

	PUGI__FN bool convert_number_to_boolean(double value)
	{
		return (value != 0 && !is_nan(value));
	}

	PUGI__FN void truncate_zeros(char* begin, char* end)
	{
		while (begin != end && end[-1] == '0') end--;

		*end = 0;
	}

	// gets mantissa digits in the form of 0.xxxxx with 0. implied and the exponent
#if defined(PUGI__MSVC_CRT_VERSION) && PUGI__MSVC_CRT_VERSION >= 1400 && !defined(_WIN32_WCE)
	PUGI__FN void convert_number_to_mantissa_exponent(double value, char (&buffer)[32], char** out_mantissa, int* out_exponent)
	{
		// get base values
		int sign, exponent;
		_ecvt_s(buffer, sizeof(buffer), value, DBL_DIG + 1, &exponent, &sign);

		// truncate redundant zeros
		truncate_zeros(buffer, buffer + strlen(buffer));

		// fill results
		*out_mantissa = buffer;
		*out_exponent = exponent;
	}
#else
	PUGI__FN void convert_number_to_mantissa_exponent(double value, char (&buffer)[32], char** out_mantissa, int* out_exponent)
	{
		// get a scientific notation value with IEEE DBL_DIG decimals
		PUGI__SNPRINTF(buffer, "%.*e", DBL_DIG, value);

		// get the exponent (possibly negative)
		char* exponent_string = strchr(buffer, 'e');
		assert(exponent_string);

		int exponent = atoi(exponent_string + 1);

		// extract mantissa string: skip sign
		char* mantissa = buffer[0] == '-' ? buffer + 1 : buffer;
		assert(mantissa[0] != '0' && mantissa[1] == '.');

		// divide mantissa by 10 to eliminate integer part
		mantissa[1] = mantissa[0];
		mantissa++;
		exponent++;

		// remove extra mantissa digits and zero-terminate mantissa
		truncate_zeros(mantissa, exponent_string);

		// fill results
		*out_mantissa = mantissa;
		*out_exponent = exponent;
	}
#endif

	PUGI__FN xpath_string convert_number_to_string(double value, xpath_allocator* alloc)
	{
		// try special number conversion
		const char_t* special = convert_number_to_string_special(value);
		if (special) return xpath_string::from_const(special);

		// get mantissa + exponent form
		char mantissa_buffer[32];

		char* mantissa;
		int exponent;
		convert_number_to_mantissa_exponent(value, mantissa_buffer, &mantissa, &exponent);

		// allocate a buffer of suitable length for the number
		size_t result_size = strlen(mantissa_buffer) + (exponent > 0 ? exponent : -exponent) + 4;
		char_t* result = static_cast<char_t*>(alloc->allocate(sizeof(char_t) * result_size));
		if (!result) return xpath_string();

		// make the number!
		char_t* s = result;

		// sign
		if (value < 0) *s++ = '-';

		// integer part
		if (exponent <= 0)
		{
			*s++ = '0';
		}
		else
		{
			while (exponent > 0)
			{
				assert(*mantissa == 0 || static_cast<unsigned int>(*mantissa - '0') <= 9);
				*s++ = *mantissa ? *mantissa++ : '0';
				exponent--;
			}
		}

		// fractional part
		if (*mantissa)
		{
			// decimal point
			*s++ = '.';

			// extra zeroes from negative exponent
			while (exponent < 0)
			{
				*s++ = '0';
				exponent++;
			}

			// extra mantissa digits
			while (*mantissa)
			{
				assert(static_cast<unsigned int>(*mantissa - '0') <= 9);
				*s++ = *mantissa++;
			}
		}

		// zero-terminate
		assert(s < result + result_size);
		*s = 0;

		return xpath_string::from_heap_preallocated(result, s);
	}

	PUGI__FN bool check_string_to_number_format(const char_t* string)
	{
		// parse leading whitespace
		while (PUGI__IS_CHARTYPE(*string, ct_space)) ++string;

		// parse sign
		if (*string == '-') ++string;

		if (!*string) return false;

		// if there is no integer part, there should be a decimal part with at least one digit
		if (!PUGI__IS_CHARTYPEX(string[0], ctx_digit) && (string[0] != '.' || !PUGI__IS_CHARTYPEX(string[1], ctx_digit))) return false;

		// parse integer part
		while (PUGI__IS_CHARTYPEX(*string, ctx_digit)) ++string;

		// parse decimal part
		if (*string == '.')
		{
			++string;

			while (PUGI__IS_CHARTYPEX(*string, ctx_digit)) ++string;
		}

		// parse trailing whitespace
		while (PUGI__IS_CHARTYPE(*string, ct_space)) ++string;

		return *string == 0;
	}

	PUGI__FN double convert_string_to_number(const char_t* string)
	{
		// check string format
		if (!check_string_to_number_format(string)) return gen_nan();

		// parse string
	#ifdef PUGIXML_WCHAR_MODE
		return wcstod(string, 0);
	#else
		return strtod(string, 0);
	#endif
	}

	PUGI__FN bool convert_string_to_number_scratch(char_t (&buffer)[32], const char_t* begin, const char_t* end, double* out_result)
	{
		size_t length = static_cast<size_t>(end - begin);
		char_t* scratch = buffer;

		if (length >= sizeof(buffer) / sizeof(buffer[0]))
		{
			// need to make dummy on-heap copy
			scratch = static_cast<char_t*>(xml_memory::allocate((length + 1) * sizeof(char_t)));
			if (!scratch) return false;
		}

		// copy string to zero-terminated buffer and perform conversion
		memcpy(scratch, begin, length * sizeof(char_t));
		scratch[length] = 0;

		*out_result = convert_string_to_number(scratch);

		// free dummy buffer
		if (scratch != buffer) xml_memory::deallocate(scratch);

		return true;
	}

	PUGI__FN double round_nearest(double value)
	{
		return floor(value + 0.5);
	}

	PUGI__FN double round_nearest_nzero(double value)
	{
		// same as round_nearest, but returns -0 for [-0.5, -0]
		// ceil is used to differentiate between +0 and -0 (we return -0 for [-0.5, -0] and +0 for +0)
		return (value >= -0.5 && value <= 0) ? ceil(value) : floor(value + 0.5);
	}

	PUGI__FN const char_t* qualified_name(const xpath_node& node)
	{
		return node.attribute() ? node.attribute().name() : node.node().name();
	}

	PUGI__FN const char_t* local_name(const xpath_node& node)
	{
		const char_t* name = qualified_name(node);
		const char_t* p = find_char(name, ':');

		return p ? p + 1 : name;
	}

	struct namespace_uri_predicate
	{
		const char_t* prefix;
		size_t prefix_length;

		namespace_uri_predicate(const char_t* name)
		{
			const char_t* pos = find_char(name, ':');

			prefix = pos ? name : 0;
			prefix_length = pos ? static_cast<size_t>(pos - name) : 0;
		}

		bool operator()(xml_attribute a) const
		{
			const char_t* name = a.name();

			if (!starts_with(name, PUGIXML_TEXT("xmlns"))) return false;

			return prefix ? name[5] == ':' && strequalrange(name + 6, prefix, prefix_length) : name[5] == 0;
		}
	};

	PUGI__FN const char_t* namespace_uri(xml_node node)
	{
		namespace_uri_predicate pred = node.name();

		xml_node p = node;

		while (p)
		{
			xml_attribute a = p.find_attribute(pred);

			if (a) return a.value();

			p = p.parent();
		}

		return PUGIXML_TEXT("");
	}

	PUGI__FN const char_t* namespace_uri(xml_attribute attr, xml_node parent)
	{
		namespace_uri_predicate pred = attr.name();

		// Default namespace does not apply to attributes
		if (!pred.prefix) return PUGIXML_TEXT("");

		xml_node p = parent;

		while (p)
		{
			xml_attribute a = p.find_attribute(pred);

			if (a) return a.value();

			p = p.parent();
		}

		return PUGIXML_TEXT("");
	}

	PUGI__FN const char_t* namespace_uri(const xpath_node& node)
	{
		return node.attribute() ? namespace_uri(node.attribute(), node.parent()) : namespace_uri(node.node());
	}

	PUGI__FN char_t* normalize_space(char_t* buffer)
	{
		char_t* write = buffer;

		for (char_t* it = buffer; *it; )
		{
			char_t ch = *it++;

			if (PUGI__IS_CHARTYPE(ch, ct_space))
			{
				// replace whitespace sequence with single space
				while (PUGI__IS_CHARTYPE(*it, ct_space)) it++;

				// avoid leading spaces
				if (write != buffer) *write++ = ' ';
			}
			else *write++ = ch;
		}

		// remove trailing space
		if (write != buffer && PUGI__IS_CHARTYPE(write[-1], ct_space)) write--;

		// zero-terminate
		*write = 0;

		return write;
	}

	PUGI__FN char_t* translate(char_t* buffer, const char_t* from, const char_t* to, size_t to_length)
	{
		char_t* write = buffer;

		while (*buffer)
		{
			PUGI__DMC_VOLATILE char_t ch = *buffer++;

			const char_t* pos = find_char(from, ch);

			if (!pos)
				*write++ = ch; // do not process
			else if (static_cast<size_t>(pos - from) < to_length)
				*write++ = to[pos - from]; // replace
		}

		// zero-terminate
		*write = 0;

		return write;
	}

	PUGI__FN unsigned char* translate_table_generate(xpath_allocator* alloc, const char_t* from, const char_t* to)
	{
		unsigned char table[128] = {0};

		while (*from)
		{
			unsigned int fc = static_cast<unsigned int>(*from);
			unsigned int tc = static_cast<unsigned int>(*to);

			if (fc >= 128 || tc >= 128)
				return 0;

			// code=128 means "skip character"
			if (!table[fc])
				table[fc] = static_cast<unsigned char>(tc ? tc : 128);

			from++;
			if (tc) to++;
		}

		for (int i = 0; i < 128; ++i)
			if (!table[i])
				table[i] = static_cast<unsigned char>(i);

		void* result = alloc->allocate(sizeof(table));
		if (!result) return 0;

		memcpy(result, table, sizeof(table));

		return static_cast<unsigned char*>(result);
	}

	PUGI__FN char_t* translate_table(char_t* buffer, const unsigned char* table)
	{
		char_t* write = buffer;

		while (*buffer)
		{
			char_t ch = *buffer++;
			unsigned int index = static_cast<unsigned int>(ch);

			if (index < 128)
			{
				unsigned char code = table[index];

				// code=128 means "skip character" (table size is 128 so 128 can be a special value)
				// this code skips these characters without extra branches
				*write = static_cast<char_t>(code);
				write += 1 - (code >> 7);
			}
			else
			{
				*write++ = ch;
			}
		}

		// zero-terminate
		*write = 0;

		return write;
	}

	inline bool is_xpath_attribute(const char_t* name)
	{
		return !(starts_with(name, PUGIXML_TEXT("xmlns")) && (name[5] == 0 || name[5] == ':'));
	}

	struct xpath_variable_boolean: xpath_variable
	{
		xpath_variable_boolean(): xpath_variable(xpath_type_boolean), value(false)
		{
		}

		bool value;
		char_t name[1];
	};

	struct xpath_variable_number: xpath_variable
	{
		xpath_variable_number(): xpath_variable(xpath_type_number), value(0)
		{
		}

		double value;
		char_t name[1];
	};

	struct xpath_variable_string: xpath_variable
	{
		xpath_variable_string(): xpath_variable(xpath_type_string), value(0)
		{
		}

		~xpath_variable_string()
		{
			if (value) xml_memory::deallocate(value);
		}

		char_t* value;
		char_t name[1];
	};

	struct xpath_variable_node_set: xpath_variable
	{
		xpath_variable_node_set(): xpath_variable(xpath_type_node_set)
		{
		}

		xpath_node_set value;
		char_t name[1];
	};

	static const xpath_node_set dummy_node_set;

	PUGI__FN PUGI__UNSIGNED_OVERFLOW unsigned int hash_string(const char_t* str)
	{
		// Jenkins one-at-a-time hash (http://en.wikipedia.org/wiki/Jenkins_hash_function#one-at-a-time)
		unsigned int result = 0;

		while (*str)
		{
			result += static_cast<unsigned int>(*str++);
			result += result << 10;
			result ^= result >> 6;
		}

		result += result << 3;
		result ^= result >> 11;
		result += result << 15;

		return result;
	}

	template <typename T> PUGI__FN T* new_xpath_variable(const char_t* name)
	{
		size_t length = strlength(name);
		if (length == 0) return 0; // empty variable names are invalid

		// $$ we can't use offsetof(T, name) because T is non-POD, so we just allocate additional length characters
		void* memory = xml_memory::allocate(sizeof(T) + length * sizeof(char_t));
		if (!memory) return 0;

		T* result = new (memory) T();

		memcpy(result->name, name, (length + 1) * sizeof(char_t));

		return result;
	}

	PUGI__FN xpath_variable* new_xpath_variable(xpath_value_type type, const char_t* name)
	{
		switch (type)
		{
		case xpath_type_node_set:
			return new_xpath_variable<xpath_variable_node_set>(name);

		case xpath_type_number:
			return new_xpath_variable<xpath_variable_number>(name);

		case xpath_type_string:
			return new_xpath_variable<xpath_variable_string>(name);

		case xpath_type_boolean:
			return new_xpath_variable<xpath_variable_boolean>(name);

		default:
			return 0;
		}
	}

	template <typename T> PUGI__FN void delete_xpath_variable(T* var)
	{
		var->~T();
		xml_memory::deallocate(var);
	}

	PUGI__FN void delete_xpath_variable(xpath_value_type type, xpath_variable* var)
	{
		switch (type)
		{
		case xpath_type_node_set:
			delete_xpath_variable(static_cast<xpath_variable_node_set*>(var));
			break;

		case xpath_type_number:
			delete_xpath_variable(static_cast<xpath_variable_number*>(var));
			break;

		case xpath_type_string:
			delete_xpath_variable(static_cast<xpath_variable_string*>(var));
			break;

		case xpath_type_boolean:
			delete_xpath_variable(static_cast<xpath_variable_boolean*>(var));
			break;

		default:
			assert(false && "Invalid variable type"); // unreachable
		}
	}

	PUGI__FN bool copy_xpath_variable(xpath_variable* lhs, const xpath_variable* rhs)
	{
		switch (rhs->type())
		{
		case xpath_type_node_set:
			return lhs->set(static_cast<const xpath_variable_node_set*>(rhs)->value);

		case xpath_type_number:
			return lhs->set(static_cast<const xpath_variable_number*>(rhs)->value);

		case xpath_type_string:
			return lhs->set(static_cast<const xpath_variable_string*>(rhs)->value);

		case xpath_type_boolean:
			return lhs->set(static_cast<const xpath_variable_boolean*>(rhs)->value);

		default:
			assert(false && "Invalid variable type"); // unreachable
			return false;
		}
	}

	PUGI__FN bool get_variable_scratch(char_t (&buffer)[32], xpath_variable_set* set, const char_t* begin, const char_t* end, xpath_variable** out_result)
	{
		size_t length = static_cast<size_t>(end - begin);
		char_t* scratch = buffer;

		if (length >= sizeof(buffer) / sizeof(buffer[0]))
		{
			// need to make dummy on-heap copy
			scratch = static_cast<char_t*>(xml_memory::allocate((length + 1) * sizeof(char_t)));
			if (!scratch) return false;
		}

		// copy string to zero-terminated buffer and perform lookup
		memcpy(scratch, begin, length * sizeof(char_t));
		scratch[length] = 0;

		*out_result = set->get(scratch);

		// free dummy buffer
		if (scratch != buffer) xml_memory::deallocate(scratch);

		return true;
	}
PUGI__NS_END

// Internal node set class
PUGI__NS_BEGIN
	PUGI__FN xpath_node_set::type_t xpath_get_order(const xpath_node* begin, const xpath_node* end)
	{
		if (end - begin < 2)
			return xpath_node_set::type_sorted;

		document_order_comparator cmp;

		bool first = cmp(begin[0], begin[1]);

		for (const xpath_node* it = begin + 1; it + 1 < end; ++it)
			if (cmp(it[0], it[1]) != first)
				return xpath_node_set::type_unsorted;

		return first ? xpath_node_set::type_sorted : xpath_node_set::type_sorted_reverse;
	}

	PUGI__FN xpath_node_set::type_t xpath_sort(xpath_node* begin, xpath_node* end, xpath_node_set::type_t type, bool rev)
	{
		xpath_node_set::type_t order = rev ? xpath_node_set::type_sorted_reverse : xpath_node_set::type_sorted;

		if (type == xpath_node_set::type_unsorted)
		{
			xpath_node_set::type_t sorted = xpath_get_order(begin, end);

			if (sorted == xpath_node_set::type_unsorted)
			{
				sort(begin, end, document_order_comparator());

				type = xpath_node_set::type_sorted;
			}
			else
				type = sorted;
		}

		if (type != order) reverse(begin, end);

		return order;
	}

	PUGI__FN xpath_node xpath_first(const xpath_node* begin, const xpath_node* end, xpath_node_set::type_t type)
	{
		if (begin == end) return xpath_node();

		switch (type)
		{
		case xpath_node_set::type_sorted:
			return *begin;

		case xpath_node_set::type_sorted_reverse:
			return *(end - 1);

		case xpath_node_set::type_unsorted:
			return *min_element(begin, end, document_order_comparator());

		default:
			assert(false && "Invalid node set type"); // unreachable
			return xpath_node();
		}
	}

	class xpath_node_set_raw
	{
		xpath_node_set::type_t _type;

		xpath_node* _begin;
		xpath_node* _end;
		xpath_node* _eos;

	public:
		xpath_node_set_raw(): _type(xpath_node_set::type_unsorted), _begin(0), _end(0), _eos(0)
		{
		}

		xpath_node* begin() const
		{
			return _begin;
		}

		xpath_node* end() const
		{
			return _end;
		}

		bool empty() const
		{
			return _begin == _end;
		}

		size_t size() const
		{
			return static_cast<size_t>(_end - _begin);
		}

		xpath_node first() const
		{
			return xpath_first(_begin, _end, _type);
		}

		void push_back_grow(const xpath_node& node, xpath_allocator* alloc);

		void push_back(const xpath_node& node, xpath_allocator* alloc)
		{
			if (_end != _eos)
				*_end++ = node;
			else
				push_back_grow(node, alloc);
		}

		void append(const xpath_node* begin_, const xpath_node* end_, xpath_allocator* alloc)
		{
			if (begin_ == end_) return;

			size_t size_ = static_cast<size_t>(_end - _begin);
			size_t capacity = static_cast<size_t>(_eos - _begin);
			size_t count = static_cast<size_t>(end_ - begin_);

			if (size_ + count > capacity)
			{
				// reallocate the old array or allocate a new one
				xpath_node* data = static_cast<xpath_node*>(alloc->reallocate(_begin, capacity * sizeof(xpath_node), (size_ + count) * sizeof(xpath_node)));
				if (!data) return;

				// finalize
				_begin = data;
				_end = data + size_;
				_eos = data + size_ + count;
			}

			memcpy(_end, begin_, count * sizeof(xpath_node));
			_end += count;
		}

		void sort_do()
		{
			_type = xpath_sort(_begin, _end, _type, false);
		}

		void truncate(xpath_node* pos)
		{
			assert(_begin <= pos && pos <= _end);

			_end = pos;
		}

		void remove_duplicates()
		{
			if (_type == xpath_node_set::type_unsorted)
				sort(_begin, _end, duplicate_comparator());

			_end = unique(_begin, _end);
		}

		xpath_node_set::type_t type() const
		{
			return _type;
		}

		void set_type(xpath_node_set::type_t value)
		{
			_type = value;
		}
	};

	PUGI__FN_NO_INLINE void xpath_node_set_raw::push_back_grow(const xpath_node& node, xpath_allocator* alloc)
	{
		size_t capacity = static_cast<size_t>(_eos - _begin);

		// get new capacity (1.5x rule)
		size_t new_capacity = capacity + capacity / 2 + 1;

		// reallocate the old array or allocate a new one
		xpath_node* data = static_cast<xpath_node*>(alloc->reallocate(_begin, capacity * sizeof(xpath_node), new_capacity * sizeof(xpath_node)));
		if (!data) return;

		// finalize
		_begin = data;
		_end = data + capacity;
		_eos = data + new_capacity;

		// push
		*_end++ = node;
	}
PUGI__NS_END

PUGI__NS_BEGIN
	struct xpath_context
	{
		xpath_node n;
		size_t position, size;

		xpath_context(const xpath_node& n_, size_t position_, size_t size_): n(n_), position(position_), size(size_)
		{
		}
	};

	enum lexeme_t
	{
		lex_none = 0,
		lex_equal,
		lex_not_equal,
		lex_less,
		lex_greater,
		lex_less_or_equal,
		lex_greater_or_equal,
		lex_plus,
		lex_minus,
		lex_multiply,
		lex_union,
		lex_var_ref,
		lex_open_brace,
		lex_close_brace,
		lex_quoted_string,
		lex_number,
		lex_slash,
		lex_double_slash,
		lex_open_square_brace,
		lex_close_square_brace,
		lex_string,
		lex_comma,
		lex_axis_attribute,
		lex_dot,
		lex_double_dot,
		lex_double_colon,
		lex_eof
	};

	struct xpath_lexer_string
	{
		const char_t* begin;
		const char_t* end;

		xpath_lexer_string(): begin(0), end(0)
		{
		}

		bool operator==(const char_t* other) const
		{
			size_t length = static_cast<size_t>(end - begin);

			return strequalrange(other, begin, length);
		}
	};

	class xpath_lexer
	{
		const char_t* _cur;
		const char_t* _cur_lexeme_pos;
		xpath_lexer_string _cur_lexeme_contents;

		lexeme_t _cur_lexeme;

	public:
		explicit xpath_lexer(const char_t* query): _cur(query)
		{
			next();
		}

		const char_t* state() const
		{
			return _cur;
		}

		void next()
		{
			const char_t* cur = _cur;

			while (PUGI__IS_CHARTYPE(*cur, ct_space)) ++cur;

			// save lexeme position for error reporting
			_cur_lexeme_pos = cur;

			switch (*cur)
			{
			case 0:
				_cur_lexeme = lex_eof;
				break;

			case '>':
				if (*(cur+1) == '=')
				{
					cur += 2;
					_cur_lexeme = lex_greater_or_equal;
				}
				else
				{
					cur += 1;
					_cur_lexeme = lex_greater;
				}
				break;

			case '<':
				if (*(cur+1) == '=')
				{
					cur += 2;
					_cur_lexeme = lex_less_or_equal;
				}
				else
				{
					cur += 1;
					_cur_lexeme = lex_less;
				}
				break;

			case '!':
				if (*(cur+1) == '=')
				{
					cur += 2;
					_cur_lexeme = lex_not_equal;
				}
				else
				{
					_cur_lexeme = lex_none;
				}
				break;

			case '=':
				cur += 1;
				_cur_lexeme = lex_equal;

				break;

			case '+':
				cur += 1;
				_cur_lexeme = lex_plus;

				break;

			case '-':
				cur += 1;
				_cur_lexeme = lex_minus;

				break;

			case '*':
				cur += 1;
				_cur_lexeme = lex_multiply;

				break;

			case '|':
				cur += 1;
				_cur_lexeme = lex_union;

				break;

			case '$':
				cur += 1;

				if (PUGI__IS_CHARTYPEX(*cur, ctx_start_symbol))
				{
					_cur_lexeme_contents.begin = cur;

					while (PUGI__IS_CHARTYPEX(*cur, ctx_symbol)) cur++;

					if (cur[0] == ':' && PUGI__IS_CHARTYPEX(cur[1], ctx_symbol)) // qname
					{
						cur++; // :

						while (PUGI__IS_CHARTYPEX(*cur, ctx_symbol)) cur++;
					}

					_cur_lexeme_contents.end = cur;

					_cur_lexeme = lex_var_ref;
				}
				else
				{
					_cur_lexeme = lex_none;
				}

				break;

			case '(':
				cur += 1;
				_cur_lexeme = lex_open_brace;

				break;

			case ')':
				cur += 1;
				_cur_lexeme = lex_close_brace;

				break;

			case '[':
				cur += 1;
				_cur_lexeme = lex_open_square_brace;

				break;

			case ']':
				cur += 1;
				_cur_lexeme = lex_close_square_brace;

				break;

			case ',':
				cur += 1;
				_cur_lexeme = lex_comma;

				break;

			case '/':
				if (*(cur+1) == '/')
				{
					cur += 2;
					_cur_lexeme = lex_double_slash;
				}
				else
				{
					cur += 1;
					_cur_lexeme = lex_slash;
				}
				break;

			case '.':
				if (*(cur+1) == '.')
				{
					cur += 2;
					_cur_lexeme = lex_double_dot;
				}
				else if (PUGI__IS_CHARTYPEX(*(cur+1), ctx_digit))
				{
					_cur_lexeme_contents.begin = cur; // .

					++cur;

					while (PUGI__IS_CHARTYPEX(*cur, ctx_digit)) cur++;

					_cur_lexeme_contents.end = cur;

					_cur_lexeme = lex_number;
				}
				else
				{
					cur += 1;
					_cur_lexeme = lex_dot;
				}
				break;

			case '@':
				cur += 1;
				_cur_lexeme = lex_axis_attribute;

				break;

			case '"':
			case '\'':
			{
				char_t terminator = *cur;

				++cur;

				_cur_lexeme_contents.begin = cur;
				while (*cur && *cur != terminator) cur++;
				_cur_lexeme_contents.end = cur;

				if (!*cur)
					_cur_lexeme = lex_none;
				else
				{
					cur += 1;
					_cur_lexeme = lex_quoted_string;
				}

				break;
			}

			case ':':
				if (*(cur+1) == ':')
				{
					cur += 2;
					_cur_lexeme = lex_double_colon;
				}
				else
				{
					_cur_lexeme = lex_none;
				}
				break;

			default:
				if (PUGI__IS_CHARTYPEX(*cur, ctx_digit))
				{
					_cur_lexeme_contents.begin = cur;

					while (PUGI__IS_CHARTYPEX(*cur, ctx_digit)) cur++;

					if (*cur == '.')
					{
						cur++;

						while (PUGI__IS_CHARTYPEX(*cur, ctx_digit)) cur++;
					}

					_cur_lexeme_contents.end = cur;

					_cur_lexeme = lex_number;
				}
				else if (PUGI__IS_CHARTYPEX(*cur, ctx_start_symbol))
				{
					_cur_lexeme_contents.begin = cur;

					while (PUGI__IS_CHARTYPEX(*cur, ctx_symbol)) cur++;

					if (cur[0] == ':')
					{
						if (cur[1] == '*') // namespace test ncname:*
						{
							cur += 2; // :*
						}
						else if (PUGI__IS_CHARTYPEX(cur[1], ctx_symbol)) // namespace test qname
						{
							cur++; // :

							while (PUGI__IS_CHARTYPEX(*cur, ctx_symbol)) cur++;
						}
					}

					_cur_lexeme_contents.end = cur;

					_cur_lexeme = lex_string;
				}
				else
				{
					_cur_lexeme = lex_none;
				}
			}

			_cur = cur;
		}

		lexeme_t current() const
		{
			return _cur_lexeme;
		}

		const char_t* current_pos() const
		{
			return _cur_lexeme_pos;
		}

		const xpath_lexer_string& contents() const
		{
			assert(_cur_lexeme == lex_var_ref || _cur_lexeme == lex_number || _cur_lexeme == lex_string || _cur_lexeme == lex_quoted_string);

			return _cur_lexeme_contents;
		}
	};

	enum ast_type_t
	{
		ast_unknown,
		ast_op_or,						// left or right
		ast_op_and,						// left and right
		ast_op_equal,					// left = right
		ast_op_not_equal,				// left != right
		ast_op_less,					// left < right
		ast_op_greater,					// left > right
		ast_op_less_or_equal,			// left <= right
		ast_op_greater_or_equal,		// left >= right
		ast_op_add,						// left + right
		ast_op_subtract,				// left - right
		ast_op_multiply,				// left * right
		ast_op_divide,					// left / right
		ast_op_mod,						// left % right
		ast_op_negate,					// left - right
		ast_op_union,					// left | right
		ast_predicate,					// apply predicate to set; next points to next predicate
		ast_filter,						// select * from left where right
		ast_string_constant,			// string constant
		ast_number_constant,			// number constant
		ast_variable,					// variable
		ast_func_last,					// last()
		ast_func_position,				// position()
		ast_func_count,					// count(left)
		ast_func_id,					// id(left)
		ast_func_local_name_0,			// local-name()
		ast_func_local_name_1,			// local-name(left)
		ast_func_namespace_uri_0,		// namespace-uri()
		ast_func_namespace_uri_1,		// namespace-uri(left)
		ast_func_name_0,				// name()
		ast_func_name_1,				// name(left)
		ast_func_string_0,				// string()
		ast_func_string_1,				// string(left)
		ast_func_concat,				// concat(left, right, siblings)
		ast_func_starts_with,			// starts_with(left, right)
		ast_func_contains,				// contains(left, right)
		ast_func_substring_before,		// substring-before(left, right)
		ast_func_substring_after,		// substring-after(left, right)
		ast_func_substring_2,			// substring(left, right)
		ast_func_substring_3,			// substring(left, right, third)
		ast_func_string_length_0,		// string-length()
		ast_func_string_length_1,		// string-length(left)
		ast_func_normalize_space_0,		// normalize-space()
		ast_func_normalize_space_1,		// normalize-space(left)
		ast_func_translate,				// translate(left, right, third)
		ast_func_boolean,				// boolean(left)
		ast_func_not,					// not(left)
		ast_func_true,					// true()
		ast_func_false,					// false()
		ast_func_lang,					// lang(left)
		ast_func_number_0,				// number()
		ast_func_number_1,				// number(left)
		ast_func_sum,					// sum(left)
		ast_func_floor,					// floor(left)
		ast_func_ceiling,				// ceiling(left)
		ast_func_round,					// round(left)
		ast_step,						// process set left with step
		ast_step_root,					// select root node

		ast_opt_translate_table,		// translate(left, right, third) where right/third are constants
		ast_opt_compare_attribute		// @name = 'string'
	};

	enum axis_t
	{
		axis_ancestor,
		axis_ancestor_or_self,
		axis_attribute,
		axis_child,
		axis_descendant,
		axis_descendant_or_self,
		axis_following,
		axis_following_sibling,
		axis_namespace,
		axis_parent,
		axis_preceding,
		axis_preceding_sibling,
		axis_self
	};

	enum nodetest_t
	{
		nodetest_none,
		nodetest_name,
		nodetest_type_node,
		nodetest_type_comment,
		nodetest_type_pi,
		nodetest_type_text,
		nodetest_pi,
		nodetest_all,
		nodetest_all_in_namespace
	};

	enum predicate_t
	{
		predicate_default,
		predicate_posinv,
		predicate_constant,
		predicate_constant_one
	};

	enum nodeset_eval_t
	{
		nodeset_eval_all,
		nodeset_eval_any,
		nodeset_eval_first
	};

	template <axis_t N> struct axis_to_type
	{
		static const axis_t axis;
	};

	template <axis_t N> const axis_t axis_to_type<N>::axis = N;

	class xpath_ast_node
	{
	private:
		// node type
		char _type;
		char _rettype;

		// for ast_step
		char _axis;

		// for ast_step/ast_predicate/ast_filter
		char _test;

		// tree node structure
		xpath_ast_node* _left;
		xpath_ast_node* _right;
		xpath_ast_node* _next;

		union
		{
			// value for ast_string_constant
			const char_t* string;
			// value for ast_number_constant
			double number;
			// variable for ast_variable
			xpath_variable* variable;
			// node test for ast_step (node name/namespace/node type/pi target)
			const char_t* nodetest;
			// table for ast_opt_translate_table
			const unsigned char* table;
		} _data;

		xpath_ast_node(const xpath_ast_node&);
		xpath_ast_node& operator=(const xpath_ast_node&);

		template <class Comp> static bool compare_eq(xpath_ast_node* lhs, xpath_ast_node* rhs, const xpath_context& c, const xpath_stack& stack, const Comp& comp)
		{
			xpath_value_type lt = lhs->rettype(), rt = rhs->rettype();

			if (lt != xpath_type_node_set && rt != xpath_type_node_set)
			{
				if (lt == xpath_type_boolean || rt == xpath_type_boolean)
					return comp(lhs->eval_boolean(c, stack), rhs->eval_boolean(c, stack));
				else if (lt == xpath_type_number || rt == xpath_type_number)
					return comp(lhs->eval_number(c, stack), rhs->eval_number(c, stack));
				else if (lt == xpath_type_string || rt == xpath_type_string)
				{
					xpath_allocator_capture cr(stack.result);

					xpath_string ls = lhs->eval_string(c, stack);
					xpath_string rs = rhs->eval_string(c, stack);

					return comp(ls, rs);
				}
			}
			else if (lt == xpath_type_node_set && rt == xpath_type_node_set)
			{
				xpath_allocator_capture cr(stack.result);

				xpath_node_set_raw ls = lhs->eval_node_set(c, stack, nodeset_eval_all);
				xpath_node_set_raw rs = rhs->eval_node_set(c, stack, nodeset_eval_all);

				for (const xpath_node* li = ls.begin(); li != ls.end(); ++li)
					for (const xpath_node* ri = rs.begin(); ri != rs.end(); ++ri)
					{
						xpath_allocator_capture cri(stack.result);

						if (comp(string_value(*li, stack.result), string_value(*ri, stack.result)))
							return true;
					}

				return false;
			}
			else
			{
				if (lt == xpath_type_node_set)
				{
					swap(lhs, rhs);
					swap(lt, rt);
				}

				if (lt == xpath_type_boolean)
					return comp(lhs->eval_boolean(c, stack), rhs->eval_boolean(c, stack));
				else if (lt == xpath_type_number)
				{
					xpath_allocator_capture cr(stack.result);

					double l = lhs->eval_number(c, stack);
					xpath_node_set_raw rs = rhs->eval_node_set(c, stack, nodeset_eval_all);

					for (const xpath_node* ri = rs.begin(); ri != rs.end(); ++ri)
					{
						xpath_allocator_capture cri(stack.result);

						if (comp(l, convert_string_to_number(string_value(*ri, stack.result).c_str())))
							return true;
					}

					return false;
				}
				else if (lt == xpath_type_string)
				{
					xpath_allocator_capture cr(stack.result);

					xpath_string l = lhs->eval_string(c, stack);
					xpath_node_set_raw rs = rhs->eval_node_set(c, stack, nodeset_eval_all);

					for (const xpath_node* ri = rs.begin(); ri != rs.end(); ++ri)
					{
						xpath_allocator_capture cri(stack.result);

						if (comp(l, string_value(*ri, stack.result)))
							return true;
					}

					return false;
				}
			}

			assert(false && "Wrong types"); // unreachable
			return false;
		}

		static bool eval_once(xpath_node_set::type_t type, nodeset_eval_t eval)
		{
			return type == xpath_node_set::type_sorted ? eval != nodeset_eval_all : eval == nodeset_eval_any;
		}

		template <class Comp> static bool compare_rel(xpath_ast_node* lhs, xpath_ast_node* rhs, const xpath_context& c, const xpath_stack& stack, const Comp& comp)
		{
			xpath_value_type lt = lhs->rettype(), rt = rhs->rettype();

			if (lt != xpath_type_node_set && rt != xpath_type_node_set)
				return comp(lhs->eval_number(c, stack), rhs->eval_number(c, stack));
			else if (lt == xpath_type_node_set && rt == xpath_type_node_set)
			{
				xpath_allocator_capture cr(stack.result);

				xpath_node_set_raw ls = lhs->eval_node_set(c, stack, nodeset_eval_all);
				xpath_node_set_raw rs = rhs->eval_node_set(c, stack, nodeset_eval_all);

				for (const xpath_node* li = ls.begin(); li != ls.end(); ++li)
				{
					xpath_allocator_capture cri(stack.result);

					double l = convert_string_to_number(string_value(*li, stack.result).c_str());

					for (const xpath_node* ri = rs.begin(); ri != rs.end(); ++ri)
					{
						xpath_allocator_capture crii(stack.result);

						if (comp(l, convert_string_to_number(string_value(*ri, stack.result).c_str())))
							return true;
					}
				}

				return false;
			}
			else if (lt != xpath_type_node_set && rt == xpath_type_node_set)
			{
				xpath_allocator_capture cr(stack.result);

				double l = lhs->eval_number(c, stack);
				xpath_node_set_raw rs = rhs->eval_node_set(c, stack, nodeset_eval_all);

				for (const xpath_node* ri = rs.begin(); ri != rs.end(); ++ri)
				{
					xpath_allocator_capture cri(stack.result);

					if (comp(l, convert_string_to_number(string_value(*ri, stack.result).c_str())))
						return true;
				}

				return false;
			}
			else if (lt == xpath_type_node_set && rt != xpath_type_node_set)
			{
				xpath_allocator_capture cr(stack.result);

				xpath_node_set_raw ls = lhs->eval_node_set(c, stack, nodeset_eval_all);
				double r = rhs->eval_number(c, stack);

				for (const xpath_node* li = ls.begin(); li != ls.end(); ++li)
				{
					xpath_allocator_capture cri(stack.result);

					if (comp(convert_string_to_number(string_value(*li, stack.result).c_str()), r))
						return true;
				}

				return false;
			}
			else
			{
				assert(false && "Wrong types"); // unreachable
				return false;
			}
		}

		static void apply_predicate_boolean(xpath_node_set_raw& ns, size_t first, xpath_ast_node* expr, const xpath_stack& stack, bool once)
		{
			assert(ns.size() >= first);
			assert(expr->rettype() != xpath_type_number);

			size_t i = 1;
			size_t size = ns.size() - first;

			xpath_node* last = ns.begin() + first;

			// remove_if... or well, sort of
			for (xpath_node* it = last; it != ns.end(); ++it, ++i)
			{
				xpath_context c(*it, i, size);

				if (expr->eval_boolean(c, stack))
				{
					*last++ = *it;

					if (once) break;
				}
			}

			ns.truncate(last);
		}

		static void apply_predicate_number(xpath_node_set_raw& ns, size_t first, xpath_ast_node* expr, const xpath_stack& stack, bool once)
		{
			assert(ns.size() >= first);
			assert(expr->rettype() == xpath_type_number);

			size_t i = 1;
			size_t size = ns.size() - first;

			xpath_node* last = ns.begin() + first;

			// remove_if... or well, sort of
			for (xpath_node* it = last; it != ns.end(); ++it, ++i)
			{
				xpath_context c(*it, i, size);

				if (expr->eval_number(c, stack) == i)
				{
					*last++ = *it;

					if (once) break;
				}
			}

			ns.truncate(last);
		}

		static void apply_predicate_number_const(xpath_node_set_raw& ns, size_t first, xpath_ast_node* expr, const xpath_stack& stack)
		{
			assert(ns.size() >= first);
			assert(expr->rettype() == xpath_type_number);

			size_t size = ns.size() - first;

			xpath_node* last = ns.begin() + first;

			xpath_context c(xpath_node(), 1, size);

			double er = expr->eval_number(c, stack);

			if (er >= 1.0 && er <= size)
			{
				size_t eri = static_cast<size_t>(er);

				if (er == eri)
				{
					xpath_node r = last[eri - 1];

					*last++ = r;
				}
			}

			ns.truncate(last);
		}

		void apply_predicate(xpath_node_set_raw& ns, size_t first, const xpath_stack& stack, bool once)
		{
			if (ns.size() == first) return;

			assert(_type == ast_filter || _type == ast_predicate);

			if (_test == predicate_constant || _test == predicate_constant_one)
				apply_predicate_number_const(ns, first, _right, stack);
			else if (_right->rettype() == xpath_type_number)
				apply_predicate_number(ns, first, _right, stack, once);
			else
				apply_predicate_boolean(ns, first, _right, stack, once);
		}

		void apply_predicates(xpath_node_set_raw& ns, size_t first, const xpath_stack& stack, nodeset_eval_t eval)
		{
			if (ns.size() == first) return;

			bool last_once = eval_once(ns.type(), eval);

			for (xpath_ast_node* pred = _right; pred; pred = pred->_next)
				pred->apply_predicate(ns, first, stack, !pred->_next && last_once);
		}

		bool step_push_lol(xpath_node_set_raw& ns, xml_attribute_struct_ref a, xml_node_struct_ref parent, xpath_allocator* alloc)
		{
			assert(a);

			const char_t* name = a.name() ? a.name() + 0 : PUGIXML_TEXT("");

			switch (_test)
			{
			case nodetest_name:
				if (strequal(name, _data.nodetest) && is_xpath_attribute(name))
				{
					ns.push_back(xpath_node(xml_attribute(a), xml_node(parent)), alloc);
					return true;
				}
				break;

			case nodetest_type_node:
			case nodetest_all:
				if (is_xpath_attribute(name))
				{
					ns.push_back(xpath_node(xml_attribute(a), xml_node(parent)), alloc);
					return true;
				}
				break;

			case nodetest_all_in_namespace:
				if (starts_with(name, _data.nodetest) && is_xpath_attribute(name))
				{
					ns.push_back(xpath_node(xml_attribute(a), xml_node(parent)), alloc);
					return true;
				}
				break;

			default:
				;
			}

			return false;
		}

		bool step_push(xpath_node_set_raw& ns, xml_attribute_struct* a, xml_node_struct* parent, xpath_allocator* alloc)
		{
			assert(a);

			const char_t* name = a->name ? a->name + 0 : PUGIXML_TEXT("");

			switch (_test)
			{
			case nodetest_name:
				if (strequal(name, _data.nodetest) && is_xpath_attribute(name))
				{
					ns.push_back(xpath_node(xml_attribute(a), xml_node(parent)), alloc);
					return true;
				}
				break;

			case nodetest_type_node:
			case nodetest_all:
				if (is_xpath_attribute(name))
				{
					ns.push_back(xpath_node(xml_attribute(a), xml_node(parent)), alloc);
					return true;
				}
				break;

			case nodetest_all_in_namespace:
				if (starts_with(name, _data.nodetest) && is_xpath_attribute(name))
				{
					ns.push_back(xpath_node(xml_attribute(a), xml_node(parent)), alloc);
					return true;
				}
				break;

			default:
				;
			}

			return false;
		}

		bool step_push_lol(xpath_node_set_raw& ns, xml_node_struct_ref n, xpath_allocator* alloc)
		{
			assert(n);

			xml_node_type type = n.node_type();

			switch (_test)
			{
			case nodetest_name:
				if (type == node_element && n.name() && strequal(n.name(), _data.nodetest))
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_type_node:
				ns.push_back(xml_node(n), alloc);
				return true;

			case nodetest_type_comment:
				if (type == node_comment)
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_type_text:
				if (type == node_pcdata || type == node_cdata)
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_type_pi:
				if (type == node_pi)
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_pi:
				if (type == node_pi && n.name() && strequal(n.name(), _data.nodetest))
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_all:
				if (type == node_element)
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_all_in_namespace:
				if (type == node_element && n.name() && starts_with(n.name(), _data.nodetest))
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			default:
				assert(false && "Unknown axis"); // unreachable
			}

			return false;
		}

		bool step_push(xpath_node_set_raw& ns, xml_node_struct* n, xpath_allocator* alloc)
		{
			assert(n);

			xml_node_type type = PUGI__NODETYPE(n);

			switch (_test)
			{
			case nodetest_name:
				if (type == node_element && n->name && strequal(n->name, _data.nodetest))
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_type_node:
				ns.push_back(xml_node(n), alloc);
				return true;

			case nodetest_type_comment:
				if (type == node_comment)
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_type_text:
				if (type == node_pcdata || type == node_cdata)
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_type_pi:
				if (type == node_pi)
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_pi:
				if (type == node_pi && n->name && strequal(n->name, _data.nodetest))
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_all:
				if (type == node_element)
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			case nodetest_all_in_namespace:
				if (type == node_element && n->name && starts_with(n->name, _data.nodetest))
				{
					ns.push_back(xml_node(n), alloc);
					return true;
				}
				break;

			default:
				assert(false && "Unknown axis"); // unreachable
			}

			return false;
		}

		template <class T> void step_fill_lol(xpath_node_set_raw& ns, xml_node_struct_ref n, xpath_allocator* alloc, bool once, T)
		{
			const axis_t axis = T::axis;

			switch (axis)
			{
			case axis_attribute:
			{
                for (xml_attribute_struct_ref a = n.first_attribute(); a; a = a.next_attribute())
					if (step_push_lol(ns, a, n, alloc) & once)
						return;

				break;
			}

			case axis_child:
			{
				for (xml_node_struct_ref c = n.first_child(); c; c = c.next_sibling())
					if (step_push_lol(ns, c, alloc) & once)
						return;

				break;
			}

			case axis_descendant:
			case axis_descendant_or_self:
			{
				if (axis == axis_descendant_or_self)
					if (step_push_lol(ns, n, alloc) & once)
						return;

				xml_node_struct_ref cur = n.first_child();

				while (cur)
				{
					if (step_push_lol(ns, cur, alloc) & once)
						return;

					if (cur.first_child())
						cur = cur.first_child();
					else
					{
						while (!cur.next_sibling())
						{
							cur = cur.parent();

							if (cur == n) return;
						}

						cur = cur.next_sibling();
					}
				}

				break;
			}

			case axis_following_sibling:
			{
				for (xml_node_struct_ref c = n.next_sibling(); c; c = c.next_sibling())
					if (step_push_lol(ns, c, alloc) & once)
						return;

				break;
			}

			case axis_preceding_sibling:
			{
				for (xml_node_struct_ref c = n.prev_sibling_c(); c.next_sibling(); c = c.prev_sibling_c())
					if (step_push_lol(ns, c, alloc) & once)
						return;

				break;
			}

			case axis_following:
			{
				xml_node_struct_ref cur = n;

				// exit from this node so that we don't include descendants
				while (!cur.next_sibling())
				{
					cur = cur.parent();

					if (!cur) return;
				}

				cur = cur.next_sibling();

				while (cur)
				{
					if (step_push_lol(ns, cur, alloc) & once)
						return;

					if (cur.first_child())
						cur = cur.first_child();
					else
					{
						while (!cur.next_sibling())
						{
							cur = cur.parent();

							if (!cur) return;
						}

						cur = cur.next_sibling();
					}
				}

				break;
			}

			case axis_preceding:
			{
				xml_node_struct_ref cur = n;

				// exit from this node so that we don't include descendants
				while (!cur.prev_sibling_c().next_sibling())
				{
					cur = cur.parent();

					if (!cur) return;
				}

				cur = cur.prev_sibling_c();

				while (cur)
				{
					if (cur.first_child())
						cur = cur.first_child().prev_sibling_c();
					else
					{
						// leaf node, can't be ancestor
						if (step_push_lol(ns, cur, alloc) & once)
							return;

						while (!cur.prev_sibling_c().next_sibling())
						{
							cur = cur.parent();

							if (!cur) return;

							if (!node_is_ancestor_lol(cur, n))
								if (step_push_lol(ns, cur, alloc) & once)
									return;
						}

						cur = cur.prev_sibling_c();
					}
				}

				break;
			}

			case axis_ancestor:
			case axis_ancestor_or_self:
			{
				if (axis == axis_ancestor_or_self)
					if (step_push_lol(ns, n, alloc) & once)
						return;

				xml_node_struct_ref cur = n.parent();

				while (cur)
				{
					if (step_push_lol(ns, cur, alloc) & once)
						return;

					cur = cur.parent();
				}

				break;
			}

			case axis_self:
			{
				step_push_lol(ns, n, alloc);

				break;
			}

			case axis_parent:
			{
				if (n.parent())
					step_push_lol(ns, n.parent(), alloc);

				break;
			}

			default:
				assert(false && "Unimplemented axis"); // unreachable
			}
		}

		template <class T> void step_fill(xpath_node_set_raw& ns, xml_node_struct* n, xpath_allocator* alloc, bool once, T)
		{
			const axis_t axis = T::axis;

			switch (axis)
			{
			case axis_attribute:
			{
				for (xml_attribute_struct* a = n->first_attribute; a; a = a->next_attribute)
					if (step_push(ns, a, n, alloc) & once)
						return;

				break;
			}

			case axis_child:
			{
				for (xml_node_struct* c = n->first_child; c; c = c->next_sibling)
					if (step_push(ns, c, alloc) & once)
						return;

				break;
			}

			case axis_descendant:
			case axis_descendant_or_self:
			{
				if (axis == axis_descendant_or_self)
					if (step_push(ns, n, alloc) & once)
						return;

				xml_node_struct* cur = n->first_child;

				while (cur)
				{
					if (step_push(ns, cur, alloc) & once)
						return;

					if (cur->first_child)
						cur = cur->first_child;
					else
					{
						while (!cur->next_sibling)
						{
							cur = cur->parent;

							if (cur == n) return;
						}

						cur = cur->next_sibling;
					}
				}

				break;
			}

			case axis_following_sibling:
			{
				for (xml_node_struct* c = n->next_sibling; c; c = c->next_sibling)
					if (step_push(ns, c, alloc) & once)
						return;

				break;
			}

			case axis_preceding_sibling:
			{
				for (xml_node_struct* c = n->prev_sibling_c; c->next_sibling; c = c->prev_sibling_c)
					if (step_push(ns, c, alloc) & once)
						return;

				break;
			}

			case axis_following:
			{
				xml_node_struct* cur = n;

				// exit from this node so that we don't include descendants
				while (!cur->next_sibling)
				{
					cur = cur->parent;

					if (!cur) return;
				}

				cur = cur->next_sibling;

				while (cur)
				{
					if (step_push(ns, cur, alloc) & once)
						return;

					if (cur->first_child)
						cur = cur->first_child;
					else
					{
						while (!cur->next_sibling)
						{
							cur = cur->parent;

							if (!cur) return;
						}

						cur = cur->next_sibling;
					}
				}

				break;
			}

			case axis_preceding:
			{
				xml_node_struct* cur = n;

				// exit from this node so that we don't include descendants
				while (!cur->prev_sibling_c->next_sibling)
				{
					cur = cur->parent;

					if (!cur) return;
				}

				cur = cur->prev_sibling_c;

				while (cur)
				{
					if (cur->first_child)
						cur = cur->first_child->prev_sibling_c;
					else
					{
						// leaf node, can't be ancestor
						if (step_push(ns, cur, alloc) & once)
							return;

						while (!cur->prev_sibling_c->next_sibling)
						{
							cur = cur->parent;

							if (!cur) return;

							if (!node_is_ancestor(cur, n))
								if (step_push(ns, cur, alloc) & once)
									return;
						}

						cur = cur->prev_sibling_c;
					}
				}

				break;
			}

			case axis_ancestor:
			case axis_ancestor_or_self:
			{
				if (axis == axis_ancestor_or_self)
					if (step_push(ns, n, alloc) & once)
						return;

				xml_node_struct* cur = n->parent;

				while (cur)
				{
					if (step_push(ns, cur, alloc) & once)
						return;

					cur = cur->parent;
				}

				break;
			}

			case axis_self:
			{
				step_push(ns, n, alloc);

				break;
			}

			case axis_parent:
			{
				if (n->parent)
					step_push(ns, n->parent, alloc);

				break;
			}

			default:
				assert(false && "Unimplemented axis"); // unreachable
			}
		}

		template <class T> void step_fill_lol(xpath_node_set_raw& ns, xml_attribute_struct_ref a, xml_node_struct_ref p, xpath_allocator* alloc, bool once, T v)
		{
			const axis_t axis = T::axis;

			switch (axis)
			{
			case axis_ancestor:
			case axis_ancestor_or_self:
			{
				if (axis == axis_ancestor_or_self && _test == nodetest_type_node) // reject attributes based on principal node type test
					if (step_push_lol(ns, a, p, alloc) & once)
						return;

				xml_node_struct_ref cur = p;

				while (cur)
				{
					if (step_push_lol(ns, cur, alloc) & once)
						return;

					cur = cur.parent();
				}

				break;
			}

			case axis_descendant_or_self:
			case axis_self:
			{
				if (_test == nodetest_type_node) // reject attributes based on principal node type test
					step_push_lol(ns, a, p, alloc);

				break;
			}

			case axis_following:
			{
				xml_node_struct_ref cur = p;

				while (cur)
				{
					if (cur.first_child())
						cur = cur.first_child();
					else
					{
						while (!cur.next_sibling())
						{
							cur = cur.parent();

							if (!cur) return;
						}

						cur = cur.next_sibling();
					}

					if (step_push_lol(ns, cur, alloc) & once)
						return;
				}

				break;
			}

			case axis_parent:
			{
				step_push_lol(ns, p, alloc);

				break;
			}

			case axis_preceding:
			{
				// preceding:: axis does not include attribute nodes and attribute ancestors (they are the same as parent's ancestors), so we can reuse node preceding
				step_fill_lol(ns, p, alloc, once, v);
				break;
			}

			default:
				assert(false && "Unimplemented axis"); // unreachable
			}
		}

		template <class T> void step_fill(xpath_node_set_raw& ns, xml_attribute_struct* a, xml_node_struct* p, xpath_allocator* alloc, bool once, T v)
		{
			const axis_t axis = T::axis;

			switch (axis)
			{
			case axis_ancestor:
			case axis_ancestor_or_self:
			{
				if (axis == axis_ancestor_or_self && _test == nodetest_type_node) // reject attributes based on principal node type test
					if (step_push(ns, a, p, alloc) & once)
						return;

				xml_node_struct* cur = p;

				while (cur)
				{
					if (step_push(ns, cur, alloc) & once)
						return;

					cur = cur->parent;
				}

				break;
			}

			case axis_descendant_or_self:
			case axis_self:
			{
				if (_test == nodetest_type_node) // reject attributes based on principal node type test
					step_push(ns, a, p, alloc);

				break;
			}

			case axis_following:
			{
				xml_node_struct* cur = p;

				while (cur)
				{
					if (cur->first_child)
						cur = cur->first_child;
					else
					{
						while (!cur->next_sibling)
						{
							cur = cur->parent;

							if (!cur) return;
						}

						cur = cur->next_sibling;
					}

					if (step_push(ns, cur, alloc) & once)
						return;
				}

				break;
			}

			case axis_parent:
			{
				step_push(ns, p, alloc);

				break;
			}

			case axis_preceding:
			{
				// preceding:: axis does not include attribute nodes and attribute ancestors (they are the same as parent's ancestors), so we can reuse node preceding
				step_fill(ns, p, alloc, once, v);
				break;
			}

			default:
				assert(false && "Unimplemented axis"); // unreachable
			}
		}

		template <class T> void step_fill(xpath_node_set_raw& ns, const xpath_node& xn, xpath_allocator* alloc, bool once, T v)
		{
			const axis_t axis = T::axis;
			const bool axis_has_attributes = (axis == axis_ancestor || axis == axis_ancestor_or_self || axis == axis_descendant_or_self || axis == axis_following || axis == axis_parent || axis == axis_preceding || axis == axis_self);

			if (xn.node())
				step_fill(ns, xn.node().internal_object(), alloc, once, v);
			else if (axis_has_attributes && xn.attribute() && xn.parent())
				step_fill(ns, xn.attribute().internal_object(), xn.parent().internal_object(), alloc, once, v);
		}

		template <class T> xpath_node_set_raw step_do(const xpath_context& c, const xpath_stack& stack, nodeset_eval_t eval, T v)
		{
			const axis_t axis = T::axis;
			const bool axis_reverse = (axis == axis_ancestor || axis == axis_ancestor_or_self || axis == axis_preceding || axis == axis_preceding_sibling);
			const xpath_node_set::type_t axis_type = axis_reverse ? xpath_node_set::type_sorted_reverse : xpath_node_set::type_sorted;

			bool once =
				(axis == axis_attribute && _test == nodetest_name) ||
				(!_right && eval_once(axis_type, eval)) ||
				(_right && !_right->_next && _right->_test == predicate_constant_one);

			xpath_node_set_raw ns;
			ns.set_type(axis_type);

			if (_left)
			{
				xpath_node_set_raw s = _left->eval_node_set(c, stack, nodeset_eval_all);

				// self axis preserves the original order
				if (axis == axis_self) ns.set_type(s.type());

				for (const xpath_node* it = s.begin(); it != s.end(); ++it)
				{
					size_t size = ns.size();

					// in general, all axes generate elements in a particular order, but there is no order guarantee if axis is applied to two nodes
					if (axis != axis_self && size != 0) ns.set_type(xpath_node_set::type_unsorted);

					step_fill(ns, *it, stack.result, once, v);
					if (_right) apply_predicates(ns, size, stack, eval);
				}
			}
			else
			{
				step_fill(ns, c.n, stack.result, once, v);
				if (_right) apply_predicates(ns, 0, stack, eval);
			}

			// child, attribute and self axes always generate unique set of nodes
			// for other axis, if the set stayed sorted, it stayed unique because the traversal algorithms do not visit the same node twice
			if (axis != axis_child && axis != axis_attribute && axis != axis_self && ns.type() == xpath_node_set::type_unsorted)
				ns.remove_duplicates();

			return ns;
		}

	public:
		xpath_ast_node(ast_type_t type, xpath_value_type rettype_, const char_t* value):
			_type(static_cast<char>(type)), _rettype(static_cast<char>(rettype_)), _axis(0), _test(0), _left(0), _right(0), _next(0)
		{
			assert(type == ast_string_constant);
			_data.string = value;
		}

		xpath_ast_node(ast_type_t type, xpath_value_type rettype_, double value):
			_type(static_cast<char>(type)), _rettype(static_cast<char>(rettype_)), _axis(0), _test(0), _left(0), _right(0), _next(0)
		{
			assert(type == ast_number_constant);
			_data.number = value;
		}

		xpath_ast_node(ast_type_t type, xpath_value_type rettype_, xpath_variable* value):
			_type(static_cast<char>(type)), _rettype(static_cast<char>(rettype_)), _axis(0), _test(0), _left(0), _right(0), _next(0)
		{
			assert(type == ast_variable);
			_data.variable = value;
		}

		xpath_ast_node(ast_type_t type, xpath_value_type rettype_, xpath_ast_node* left = 0, xpath_ast_node* right = 0):
			_type(static_cast<char>(type)), _rettype(static_cast<char>(rettype_)), _axis(0), _test(0), _left(left), _right(right), _next(0)
		{
		}

		xpath_ast_node(ast_type_t type, xpath_ast_node* left, axis_t axis, nodetest_t test, const char_t* contents):
			_type(static_cast<char>(type)), _rettype(xpath_type_node_set), _axis(static_cast<char>(axis)), _test(static_cast<char>(test)), _left(left), _right(0), _next(0)
		{
			assert(type == ast_step);
			_data.nodetest = contents;
		}

		xpath_ast_node(ast_type_t type, xpath_ast_node* left, xpath_ast_node* right, predicate_t test):
			_type(static_cast<char>(type)), _rettype(xpath_type_node_set), _axis(0), _test(static_cast<char>(test)), _left(left), _right(right), _next(0)
		{
			assert(type == ast_filter || type == ast_predicate);
		}

		void set_next(xpath_ast_node* value)
		{
			_next = value;
		}

		void set_right(xpath_ast_node* value)
		{
			_right = value;
		}

		bool eval_boolean(const xpath_context& c, const xpath_stack& stack)
		{
			switch (_type)
			{
			case ast_op_or:
				return _left->eval_boolean(c, stack) || _right->eval_boolean(c, stack);

			case ast_op_and:
				return _left->eval_boolean(c, stack) && _right->eval_boolean(c, stack);

			case ast_op_equal:
				return compare_eq(_left, _right, c, stack, equal_to());

			case ast_op_not_equal:
				return compare_eq(_left, _right, c, stack, not_equal_to());

			case ast_op_less:
				return compare_rel(_left, _right, c, stack, less());

			case ast_op_greater:
				return compare_rel(_right, _left, c, stack, less());

			case ast_op_less_or_equal:
				return compare_rel(_left, _right, c, stack, less_equal());

			case ast_op_greater_or_equal:
				return compare_rel(_right, _left, c, stack, less_equal());

			case ast_func_starts_with:
			{
				xpath_allocator_capture cr(stack.result);

				xpath_string lr = _left->eval_string(c, stack);
				xpath_string rr = _right->eval_string(c, stack);

				return starts_with(lr.c_str(), rr.c_str());
			}

			case ast_func_contains:
			{
				xpath_allocator_capture cr(stack.result);

				xpath_string lr = _left->eval_string(c, stack);
				xpath_string rr = _right->eval_string(c, stack);

				return find_substring(lr.c_str(), rr.c_str()) != 0;
			}

			case ast_func_boolean:
				return _left->eval_boolean(c, stack);

			case ast_func_not:
				return !_left->eval_boolean(c, stack);

			case ast_func_true:
				return true;

			case ast_func_false:
				return false;

			case ast_func_lang:
			{
				if (c.n.attribute()) return false;

				xpath_allocator_capture cr(stack.result);

				xpath_string lang = _left->eval_string(c, stack);

				for (xml_node n = c.n.node(); n; n = n.parent())
				{
					xml_attribute a = n.attribute(PUGIXML_TEXT("xml:lang"));

					if (a)
					{
						const char_t* value = a.value();

						// strnicmp / strncasecmp is not portable
						for (const char_t* lit = lang.c_str(); *lit; ++lit)
						{
							if (tolower_ascii(*lit) != tolower_ascii(*value)) return false;
							++value;
						}

						return *value == 0 || *value == '-';
					}
				}

				return false;
			}

			case ast_opt_compare_attribute:
			{
				const char_t* value = (_right->_type == ast_string_constant) ? _right->_data.string : _right->_data.variable->get_string();

				xml_attribute attr = c.n.node().attribute(_left->_data.nodetest);

				return attr && strequal(attr.value(), value) && is_xpath_attribute(attr.name());
			}

			case ast_variable:
			{
				assert(_rettype == _data.variable->type());

				if (_rettype == xpath_type_boolean)
					return _data.variable->get_boolean();
			}

			// fallthrough
			default:
			{
				switch (_rettype)
				{
				case xpath_type_number:
					return convert_number_to_boolean(eval_number(c, stack));

				case xpath_type_string:
				{
					xpath_allocator_capture cr(stack.result);

					return !eval_string(c, stack).empty();
				}

				case xpath_type_node_set:
				{
					xpath_allocator_capture cr(stack.result);

					return !eval_node_set(c, stack, nodeset_eval_any).empty();
				}

				default:
					assert(false && "Wrong expression for return type boolean"); // unreachable
					return false;
				}
			}
			}
		}

		double eval_number(const xpath_context& c, const xpath_stack& stack)
		{
			switch (_type)
			{
			case ast_op_add:
				return _left->eval_number(c, stack) + _right->eval_number(c, stack);

			case ast_op_subtract:
				return _left->eval_number(c, stack) - _right->eval_number(c, stack);

			case ast_op_multiply:
				return _left->eval_number(c, stack) * _right->eval_number(c, stack);

			case ast_op_divide:
				return _left->eval_number(c, stack) / _right->eval_number(c, stack);

			case ast_op_mod:
				return fmod(_left->eval_number(c, stack), _right->eval_number(c, stack));

			case ast_op_negate:
				return -_left->eval_number(c, stack);

			case ast_number_constant:
				return _data.number;

			case ast_func_last:
				return static_cast<double>(c.size);

			case ast_func_position:
				return static_cast<double>(c.position);

			case ast_func_count:
			{
				xpath_allocator_capture cr(stack.result);

				return static_cast<double>(_left->eval_node_set(c, stack, nodeset_eval_all).size());
			}

			case ast_func_string_length_0:
			{
				xpath_allocator_capture cr(stack.result);

				return static_cast<double>(string_value(c.n, stack.result).length());
			}

			case ast_func_string_length_1:
			{
				xpath_allocator_capture cr(stack.result);

				return static_cast<double>(_left->eval_string(c, stack).length());
			}

			case ast_func_number_0:
			{
				xpath_allocator_capture cr(stack.result);

				return convert_string_to_number(string_value(c.n, stack.result).c_str());
			}

			case ast_func_number_1:
				return _left->eval_number(c, stack);

			case ast_func_sum:
			{
				xpath_allocator_capture cr(stack.result);

				double r = 0;

				xpath_node_set_raw ns = _left->eval_node_set(c, stack, nodeset_eval_all);

				for (const xpath_node* it = ns.begin(); it != ns.end(); ++it)
				{
					xpath_allocator_capture cri(stack.result);

					r += convert_string_to_number(string_value(*it, stack.result).c_str());
				}

				return r;
			}

			case ast_func_floor:
			{
				double r = _left->eval_number(c, stack);

				return r == r ? floor(r) : r;
			}

			case ast_func_ceiling:
			{
				double r = _left->eval_number(c, stack);

				return r == r ? ceil(r) : r;
			}

			case ast_func_round:
				return round_nearest_nzero(_left->eval_number(c, stack));

			case ast_variable:
			{
				assert(_rettype == _data.variable->type());

				if (_rettype == xpath_type_number)
					return _data.variable->get_number();
			}

			// fallthrough
			default:
			{
				switch (_rettype)
				{
				case xpath_type_boolean:
					return eval_boolean(c, stack) ? 1 : 0;

				case xpath_type_string:
				{
					xpath_allocator_capture cr(stack.result);

					return convert_string_to_number(eval_string(c, stack).c_str());
				}

				case xpath_type_node_set:
				{
					xpath_allocator_capture cr(stack.result);

					return convert_string_to_number(eval_string(c, stack).c_str());
				}

				default:
					assert(false && "Wrong expression for return type number"); // unreachable
					return 0;
				}

			}
			}
		}

		xpath_string eval_string_concat(const xpath_context& c, const xpath_stack& stack)
		{
			assert(_type == ast_func_concat);

			xpath_allocator_capture ct(stack.temp);

			// count the string number
			size_t count = 1;
			for (xpath_ast_node* nc = _right; nc; nc = nc->_next) count++;

			// gather all strings
			xpath_string static_buffer[4];
			xpath_string* buffer = static_buffer;

			// allocate on-heap for large concats
			if (count > sizeof(static_buffer) / sizeof(static_buffer[0]))
			{
				buffer = static_cast<xpath_string*>(stack.temp->allocate(count * sizeof(xpath_string)));
				if (!buffer) return xpath_string();
			}

			// evaluate all strings to temporary stack
			xpath_stack swapped_stack = {stack.temp, stack.result};

			buffer[0] = _left->eval_string(c, swapped_stack);

			size_t pos = 1;
			for (xpath_ast_node* n = _right; n; n = n->_next, ++pos) buffer[pos] = n->eval_string(c, swapped_stack);
			assert(pos == count);

			// get total length
			size_t length = 0;
			for (size_t i = 0; i < count; ++i) length += buffer[i].length();

			// create final string
			char_t* result = static_cast<char_t*>(stack.result->allocate((length + 1) * sizeof(char_t)));
			if (!result) return xpath_string();

			char_t* ri = result;

			for (size_t j = 0; j < count; ++j)
				for (const char_t* bi = buffer[j].c_str(); *bi; ++bi)
					*ri++ = *bi;

			*ri = 0;

			return xpath_string::from_heap_preallocated(result, ri);
		}

		xpath_string eval_string(const xpath_context& c, const xpath_stack& stack)
		{
			switch (_type)
			{
			case ast_string_constant:
				return xpath_string::from_const(_data.string);

			case ast_func_local_name_0:
			{
				xpath_node na = c.n;

				return xpath_string::from_const(local_name(na));
			}

			case ast_func_local_name_1:
			{
				xpath_allocator_capture cr(stack.result);

				xpath_node_set_raw ns = _left->eval_node_set(c, stack, nodeset_eval_first);
				xpath_node na = ns.first();

				return xpath_string::from_const(local_name(na));
			}

			case ast_func_name_0:
			{
				xpath_node na = c.n;

				return xpath_string::from_const(qualified_name(na));
			}

			case ast_func_name_1:
			{
				xpath_allocator_capture cr(stack.result);

				xpath_node_set_raw ns = _left->eval_node_set(c, stack, nodeset_eval_first);
				xpath_node na = ns.first();

				return xpath_string::from_const(qualified_name(na));
			}

			case ast_func_namespace_uri_0:
			{
				xpath_node na = c.n;

				return xpath_string::from_const(namespace_uri(na));
			}

			case ast_func_namespace_uri_1:
			{
				xpath_allocator_capture cr(stack.result);

				xpath_node_set_raw ns = _left->eval_node_set(c, stack, nodeset_eval_first);
				xpath_node na = ns.first();

				return xpath_string::from_const(namespace_uri(na));
			}

			case ast_func_string_0:
				return string_value(c.n, stack.result);

			case ast_func_string_1:
				return _left->eval_string(c, stack);

			case ast_func_concat:
				return eval_string_concat(c, stack);

			case ast_func_substring_before:
			{
				xpath_allocator_capture cr(stack.temp);

				xpath_stack swapped_stack = {stack.temp, stack.result};

				xpath_string s = _left->eval_string(c, swapped_stack);
				xpath_string p = _right->eval_string(c, swapped_stack);

				const char_t* pos = find_substring(s.c_str(), p.c_str());

				return pos ? xpath_string::from_heap(s.c_str(), pos, stack.result) : xpath_string();
			}

			case ast_func_substring_after:
			{
				xpath_allocator_capture cr(stack.temp);

				xpath_stack swapped_stack = {stack.temp, stack.result};

				xpath_string s = _left->eval_string(c, swapped_stack);
				xpath_string p = _right->eval_string(c, swapped_stack);

				const char_t* pos = find_substring(s.c_str(), p.c_str());
				if (!pos) return xpath_string();

				const char_t* rbegin = pos + p.length();
				const char_t* rend = s.c_str() + s.length();

				return s.uses_heap() ? xpath_string::from_heap(rbegin, rend, stack.result) : xpath_string::from_const(rbegin);
			}

			case ast_func_substring_2:
			{
				xpath_allocator_capture cr(stack.temp);

				xpath_stack swapped_stack = {stack.temp, stack.result};

				xpath_string s = _left->eval_string(c, swapped_stack);
				size_t s_length = s.length();

				double first = round_nearest(_right->eval_number(c, stack));

				if (is_nan(first)) return xpath_string(); // NaN
				else if (first >= s_length + 1) return xpath_string();

				size_t pos = first < 1 ? 1 : static_cast<size_t>(first);
				assert(1 <= pos && pos <= s_length + 1);

				const char_t* rbegin = s.c_str() + (pos - 1);
				const char_t* rend = s.c_str() + s.length();

				return s.uses_heap() ? xpath_string::from_heap(rbegin, rend, stack.result) : xpath_string::from_const(rbegin);
			}

			case ast_func_substring_3:
			{
				xpath_allocator_capture cr(stack.temp);

				xpath_stack swapped_stack = {stack.temp, stack.result};

				xpath_string s = _left->eval_string(c, swapped_stack);
				size_t s_length = s.length();

				double first = round_nearest(_right->eval_number(c, stack));
				double last = first + round_nearest(_right->_next->eval_number(c, stack));

				if (is_nan(first) || is_nan(last)) return xpath_string();
				else if (first >= s_length + 1) return xpath_string();
				else if (first >= last) return xpath_string();
				else if (last < 1) return xpath_string();

				size_t pos = first < 1 ? 1 : static_cast<size_t>(first);
				size_t end = last >= s_length + 1 ? s_length + 1 : static_cast<size_t>(last);

				assert(1 <= pos && pos <= end && end <= s_length + 1);
				const char_t* rbegin = s.c_str() + (pos - 1);
				const char_t* rend = s.c_str() + (end - 1);

				return (end == s_length + 1 && !s.uses_heap()) ? xpath_string::from_const(rbegin) : xpath_string::from_heap(rbegin, rend, stack.result);
			}

			case ast_func_normalize_space_0:
			{
				xpath_string s = string_value(c.n, stack.result);

				char_t* begin = s.data(stack.result);
				if (!begin) return xpath_string();

				char_t* end = normalize_space(begin);

				return xpath_string::from_heap_preallocated(begin, end);
			}

			case ast_func_normalize_space_1:
			{
				xpath_string s = _left->eval_string(c, stack);

				char_t* begin = s.data(stack.result);
				if (!begin) return xpath_string();

				char_t* end = normalize_space(begin);

				return xpath_string::from_heap_preallocated(begin, end);
			}

			case ast_func_translate:
			{
				xpath_allocator_capture cr(stack.temp);

				xpath_stack swapped_stack = {stack.temp, stack.result};

				xpath_string s = _left->eval_string(c, stack);
				xpath_string from = _right->eval_string(c, swapped_stack);
				xpath_string to = _right->_next->eval_string(c, swapped_stack);

				char_t* begin = s.data(stack.result);
				if (!begin) return xpath_string();

				char_t* end = translate(begin, from.c_str(), to.c_str(), to.length());

				return xpath_string::from_heap_preallocated(begin, end);
			}

			case ast_opt_translate_table:
			{
				xpath_string s = _left->eval_string(c, stack);

				char_t* begin = s.data(stack.result);
				if (!begin) return xpath_string();

				char_t* end = translate_table(begin, _data.table);

				return xpath_string::from_heap_preallocated(begin, end);
			}

			case ast_variable:
			{
				assert(_rettype == _data.variable->type());

				if (_rettype == xpath_type_string)
					return xpath_string::from_const(_data.variable->get_string());
			}

			// fallthrough
			default:
			{
				switch (_rettype)
				{
				case xpath_type_boolean:
					return xpath_string::from_const(eval_boolean(c, stack) ? PUGIXML_TEXT("true") : PUGIXML_TEXT("false"));

				case xpath_type_number:
					return convert_number_to_string(eval_number(c, stack), stack.result);

				case xpath_type_node_set:
				{
					xpath_allocator_capture cr(stack.temp);

					xpath_stack swapped_stack = {stack.temp, stack.result};

					xpath_node_set_raw ns = eval_node_set(c, swapped_stack, nodeset_eval_first);
					return ns.empty() ? xpath_string() : string_value(ns.first(), stack.result);
				}

				default:
					assert(false && "Wrong expression for return type string"); // unreachable
					return xpath_string();
				}
			}
			}
		}

		xpath_node_set_raw eval_node_set(const xpath_context& c, const xpath_stack& stack, nodeset_eval_t eval)
		{
			switch (_type)
			{
			case ast_op_union:
			{
				xpath_allocator_capture cr(stack.temp);

				xpath_stack swapped_stack = {stack.temp, stack.result};

				xpath_node_set_raw ls = _left->eval_node_set(c, swapped_stack, eval);
				xpath_node_set_raw rs = _right->eval_node_set(c, stack, eval);

				// we can optimize merging two sorted sets, but this is a very rare operation, so don't bother
				rs.set_type(xpath_node_set::type_unsorted);

				rs.append(ls.begin(), ls.end(), stack.result);
				rs.remove_duplicates();

				return rs;
			}

			case ast_filter:
			{
				xpath_node_set_raw set = _left->eval_node_set(c, stack, _test == predicate_constant_one ? nodeset_eval_first : nodeset_eval_all);

				// either expression is a number or it contains position() call; sort by document order
				if (_test != predicate_posinv) set.sort_do();

				bool once = eval_once(set.type(), eval);

				apply_predicate(set, 0, stack, once);

				return set;
			}

			case ast_func_id:
				return xpath_node_set_raw();

			case ast_step:
			{
				switch (_axis)
				{
				case axis_ancestor:
					return step_do(c, stack, eval, axis_to_type<axis_ancestor>());

				case axis_ancestor_or_self:
					return step_do(c, stack, eval, axis_to_type<axis_ancestor_or_self>());

				case axis_attribute:
					return step_do(c, stack, eval, axis_to_type<axis_attribute>());

				case axis_child:
					return step_do(c, stack, eval, axis_to_type<axis_child>());

				case axis_descendant:
					return step_do(c, stack, eval, axis_to_type<axis_descendant>());

				case axis_descendant_or_self:
					return step_do(c, stack, eval, axis_to_type<axis_descendant_or_self>());

				case axis_following:
					return step_do(c, stack, eval, axis_to_type<axis_following>());

				case axis_following_sibling:
					return step_do(c, stack, eval, axis_to_type<axis_following_sibling>());

				case axis_namespace:
					// namespaced axis is not supported
					return xpath_node_set_raw();

				case axis_parent:
					return step_do(c, stack, eval, axis_to_type<axis_parent>());

				case axis_preceding:
					return step_do(c, stack, eval, axis_to_type<axis_preceding>());

				case axis_preceding_sibling:
					return step_do(c, stack, eval, axis_to_type<axis_preceding_sibling>());

				case axis_self:
					return step_do(c, stack, eval, axis_to_type<axis_self>());

				default:
					assert(false && "Unknown axis"); // unreachable
					return xpath_node_set_raw();
				}
			}

			case ast_step_root:
			{
				assert(!_right); // root step can't have any predicates

				xpath_node_set_raw ns;

				ns.set_type(xpath_node_set::type_sorted);

				if (c.n.node()) ns.push_back(c.n.node().root(), stack.result);
				else if (c.n.attribute()) ns.push_back(c.n.parent().root(), stack.result);

				return ns;
			}

			case ast_variable:
			{
				assert(_rettype == _data.variable->type());

				if (_rettype == xpath_type_node_set)
				{
					const xpath_node_set& s = _data.variable->get_node_set();

					xpath_node_set_raw ns;

					ns.set_type(s.type());
					ns.append(s.begin(), s.end(), stack.result);

					return ns;
				}
			}

			// fallthrough
			default:
				assert(false && "Wrong expression for return type node set"); // unreachable
				return xpath_node_set_raw();
			}
		}

		void optimize(xpath_allocator* alloc)
		{
			if (_left)
				_left->optimize(alloc);

			if (_right)
				_right->optimize(alloc);

			if (_next)
				_next->optimize(alloc);

			optimize_self(alloc);
		}

		void optimize_self(xpath_allocator* alloc)
		{
			// Rewrite [position()=expr] with [expr]
			// Note that this step has to go before classification to recognize [position()=1]
			if ((_type == ast_filter || _type == ast_predicate) &&
				_right->_type == ast_op_equal && _right->_left->_type == ast_func_position && _right->_right->_rettype == xpath_type_number)
			{
				_right = _right->_right;
			}

			// Classify filter/predicate ops to perform various optimizations during evaluation
			if (_type == ast_filter || _type == ast_predicate)
			{
				assert(_test == predicate_default);

				if (_right->_type == ast_number_constant && _right->_data.number == 1.0)
					_test = predicate_constant_one;
				else if (_right->_rettype == xpath_type_number && (_right->_type == ast_number_constant || _right->_type == ast_variable || _right->_type == ast_func_last))
					_test = predicate_constant;
				else if (_right->_rettype != xpath_type_number && _right->is_posinv_expr())
					_test = predicate_posinv;
			}

			// Rewrite descendant-or-self::node()/child::foo with descendant::foo
			// The former is a full form of //foo, the latter is much faster since it executes the node test immediately
			// Do a similar kind of rewrite for self/descendant/descendant-or-self axes
			// Note that we only rewrite positionally invariant steps (//foo[1] != /descendant::foo[1])
			if (_type == ast_step && (_axis == axis_child || _axis == axis_self || _axis == axis_descendant || _axis == axis_descendant_or_self) && _left &&
				_left->_type == ast_step && _left->_axis == axis_descendant_or_self && _left->_test == nodetest_type_node && !_left->_right &&
				is_posinv_step())
			{
				if (_axis == axis_child || _axis == axis_descendant)
					_axis = axis_descendant;
				else
					_axis = axis_descendant_or_self;

				_left = _left->_left;
			}

			// Use optimized lookup table implementation for translate() with constant arguments
			if (_type == ast_func_translate && _right->_type == ast_string_constant && _right->_next->_type == ast_string_constant)
			{
				unsigned char* table = translate_table_generate(alloc, _right->_data.string, _right->_next->_data.string);

				if (table)
				{
					_type = ast_opt_translate_table;
					_data.table = table;
				}
			}

			// Use optimized path for @attr = 'value' or @attr = $value
			if (_type == ast_op_equal &&
				_left->_type == ast_step && _left->_axis == axis_attribute && _left->_test == nodetest_name && !_left->_left && !_left->_right &&
				(_right->_type == ast_string_constant || (_right->_type == ast_variable && _right->_rettype == xpath_type_string)))
			{
				_type = ast_opt_compare_attribute;
			}
		}

		bool is_posinv_expr() const
		{
			switch (_type)
			{
			case ast_func_position:
			case ast_func_last:
				return false;

			case ast_string_constant:
			case ast_number_constant:
			case ast_variable:
				return true;

			case ast_step:
			case ast_step_root:
				return true;

			case ast_predicate:
			case ast_filter:
				return true;

			default:
				if (_left && !_left->is_posinv_expr()) return false;

				for (xpath_ast_node* n = _right; n; n = n->_next)
					if (!n->is_posinv_expr()) return false;

				return true;
			}
		}

		bool is_posinv_step() const
		{
			assert(_type == ast_step);

			for (xpath_ast_node* n = _right; n; n = n->_next)
			{
				assert(n->_type == ast_predicate);

				if (n->_test != predicate_posinv)
					return false;
			}

			return true;
		}

		xpath_value_type rettype() const
		{
			return static_cast<xpath_value_type>(_rettype);
		}
	};

	struct xpath_parser
	{
		xpath_allocator* _alloc;
		xpath_lexer _lexer;

		const char_t* _query;
		xpath_variable_set* _variables;

		xpath_parse_result* _result;

		char_t _scratch[32];

		xpath_ast_node* error(const char* message)
		{
			_result->error = message;
			_result->offset = _lexer.current_pos() - _query;

			return 0;
		}

		xpath_ast_node* error_oom()
		{
			assert(_alloc->_error);
			*_alloc->_error = true;

			return 0;
		}

		void* alloc_node()
		{
			return _alloc->allocate(sizeof(xpath_ast_node));
		}

		xpath_ast_node* alloc_node(ast_type_t type, xpath_value_type rettype, const char_t* value)
		{
			void* memory = alloc_node();
			return memory ? new (memory) xpath_ast_node(type, rettype, value) : 0;
		}

		xpath_ast_node* alloc_node(ast_type_t type, xpath_value_type rettype, double value)
		{
			void* memory = alloc_node();
			return memory ? new (memory) xpath_ast_node(type, rettype, value) : 0;
		}

		xpath_ast_node* alloc_node(ast_type_t type, xpath_value_type rettype, xpath_variable* value)
		{
			void* memory = alloc_node();
			return memory ? new (memory) xpath_ast_node(type, rettype, value) : 0;
		}

		xpath_ast_node* alloc_node(ast_type_t type, xpath_value_type rettype, xpath_ast_node* left = 0, xpath_ast_node* right = 0)
		{
			void* memory = alloc_node();
			return memory ? new (memory) xpath_ast_node(type, rettype, left, right) : 0;
		}

		xpath_ast_node* alloc_node(ast_type_t type, xpath_ast_node* left, axis_t axis, nodetest_t test, const char_t* contents)
		{
			void* memory = alloc_node();
			return memory ? new (memory) xpath_ast_node(type, left, axis, test, contents) : 0;
		}

		xpath_ast_node* alloc_node(ast_type_t type, xpath_ast_node* left, xpath_ast_node* right, predicate_t test)
		{
			void* memory = alloc_node();
			return memory ? new (memory) xpath_ast_node(type, left, right, test) : 0;
		}

		const char_t* alloc_string(const xpath_lexer_string& value)
		{
			if (!value.begin)
				return PUGIXML_TEXT("");

			size_t length = static_cast<size_t>(value.end - value.begin);

			char_t* c = static_cast<char_t*>(_alloc->allocate((length + 1) * sizeof(char_t)));
			if (!c) return 0;

			memcpy(c, value.begin, length * sizeof(char_t));
			c[length] = 0;

			return c;
		}

		xpath_ast_node* parse_function(const xpath_lexer_string& name, size_t argc, xpath_ast_node* args[2])
		{
			switch (name.begin[0])
			{
			case 'b':
				if (name == PUGIXML_TEXT("boolean") && argc == 1)
					return alloc_node(ast_func_boolean, xpath_type_boolean, args[0]);

				break;

			case 'c':
				if (name == PUGIXML_TEXT("count") && argc == 1)
				{
					if (args[0]->rettype() != xpath_type_node_set) return error("Function has to be applied to node set");
					return alloc_node(ast_func_count, xpath_type_number, args[0]);
				}
				else if (name == PUGIXML_TEXT("contains") && argc == 2)
					return alloc_node(ast_func_contains, xpath_type_boolean, args[0], args[1]);
				else if (name == PUGIXML_TEXT("concat") && argc >= 2)
					return alloc_node(ast_func_concat, xpath_type_string, args[0], args[1]);
				else if (name == PUGIXML_TEXT("ceiling") && argc == 1)
					return alloc_node(ast_func_ceiling, xpath_type_number, args[0]);

				break;

			case 'f':
				if (name == PUGIXML_TEXT("false") && argc == 0)
					return alloc_node(ast_func_false, xpath_type_boolean);
				else if (name == PUGIXML_TEXT("floor") && argc == 1)
					return alloc_node(ast_func_floor, xpath_type_number, args[0]);

				break;

			case 'i':
				if (name == PUGIXML_TEXT("id") && argc == 1)
					return alloc_node(ast_func_id, xpath_type_node_set, args[0]);

				break;

			case 'l':
				if (name == PUGIXML_TEXT("last") && argc == 0)
					return alloc_node(ast_func_last, xpath_type_number);
				else if (name == PUGIXML_TEXT("lang") && argc == 1)
					return alloc_node(ast_func_lang, xpath_type_boolean, args[0]);
				else if (name == PUGIXML_TEXT("local-name") && argc <= 1)
				{
					if (argc == 1 && args[0]->rettype() != xpath_type_node_set) return error("Function has to be applied to node set");
					return alloc_node(argc == 0 ? ast_func_local_name_0 : ast_func_local_name_1, xpath_type_string, args[0]);
				}

				break;

			case 'n':
				if (name == PUGIXML_TEXT("name") && argc <= 1)
				{
					if (argc == 1 && args[0]->rettype() != xpath_type_node_set) return error("Function has to be applied to node set");
					return alloc_node(argc == 0 ? ast_func_name_0 : ast_func_name_1, xpath_type_string, args[0]);
				}
				else if (name == PUGIXML_TEXT("namespace-uri") && argc <= 1)
				{
					if (argc == 1 && args[0]->rettype() != xpath_type_node_set) return error("Function has to be applied to node set");
					return alloc_node(argc == 0 ? ast_func_namespace_uri_0 : ast_func_namespace_uri_1, xpath_type_string, args[0]);
				}
				else if (name == PUGIXML_TEXT("normalize-space") && argc <= 1)
					return alloc_node(argc == 0 ? ast_func_normalize_space_0 : ast_func_normalize_space_1, xpath_type_string, args[0], args[1]);
				else if (name == PUGIXML_TEXT("not") && argc == 1)
					return alloc_node(ast_func_not, xpath_type_boolean, args[0]);
				else if (name == PUGIXML_TEXT("number") && argc <= 1)
					return alloc_node(argc == 0 ? ast_func_number_0 : ast_func_number_1, xpath_type_number, args[0]);

				break;

			case 'p':
				if (name == PUGIXML_TEXT("position") && argc == 0)
					return alloc_node(ast_func_position, xpath_type_number);

				break;

			case 'r':
				if (name == PUGIXML_TEXT("round") && argc == 1)
					return alloc_node(ast_func_round, xpath_type_number, args[0]);

				break;

			case 's':
				if (name == PUGIXML_TEXT("string") && argc <= 1)
					return alloc_node(argc == 0 ? ast_func_string_0 : ast_func_string_1, xpath_type_string, args[0]);
				else if (name == PUGIXML_TEXT("string-length") && argc <= 1)
					return alloc_node(argc == 0 ? ast_func_string_length_0 : ast_func_string_length_1, xpath_type_number, args[0]);
				else if (name == PUGIXML_TEXT("starts-with") && argc == 2)
					return alloc_node(ast_func_starts_with, xpath_type_boolean, args[0], args[1]);
				else if (name == PUGIXML_TEXT("substring-before") && argc == 2)
					return alloc_node(ast_func_substring_before, xpath_type_string, args[0], args[1]);
				else if (name == PUGIXML_TEXT("substring-after") && argc == 2)
					return alloc_node(ast_func_substring_after, xpath_type_string, args[0], args[1]);
				else if (name == PUGIXML_TEXT("substring") && (argc == 2 || argc == 3))
					return alloc_node(argc == 2 ? ast_func_substring_2 : ast_func_substring_3, xpath_type_string, args[0], args[1]);
				else if (name == PUGIXML_TEXT("sum") && argc == 1)
				{
					if (args[0]->rettype() != xpath_type_node_set) return error("Function has to be applied to node set");
					return alloc_node(ast_func_sum, xpath_type_number, args[0]);
				}

				break;

			case 't':
				if (name == PUGIXML_TEXT("translate") && argc == 3)
					return alloc_node(ast_func_translate, xpath_type_string, args[0], args[1]);
				else if (name == PUGIXML_TEXT("true") && argc == 0)
					return alloc_node(ast_func_true, xpath_type_boolean);

				break;

			default:
				break;
			}

			return error("Unrecognized function or wrong parameter count");
		}

		axis_t parse_axis_name(const xpath_lexer_string& name, bool& specified)
		{
			specified = true;

			switch (name.begin[0])
			{
			case 'a':
				if (name == PUGIXML_TEXT("ancestor"))
					return axis_ancestor;
				else if (name == PUGIXML_TEXT("ancestor-or-self"))
					return axis_ancestor_or_self;
				else if (name == PUGIXML_TEXT("attribute"))
					return axis_attribute;

				break;

			case 'c':
				if (name == PUGIXML_TEXT("child"))
					return axis_child;

				break;

			case 'd':
				if (name == PUGIXML_TEXT("descendant"))
					return axis_descendant;
				else if (name == PUGIXML_TEXT("descendant-or-self"))
					return axis_descendant_or_self;

				break;

			case 'f':
				if (name == PUGIXML_TEXT("following"))
					return axis_following;
				else if (name == PUGIXML_TEXT("following-sibling"))
					return axis_following_sibling;

				break;

			case 'n':
				if (name == PUGIXML_TEXT("namespace"))
					return axis_namespace;

				break;

			case 'p':
				if (name == PUGIXML_TEXT("parent"))
					return axis_parent;
				else if (name == PUGIXML_TEXT("preceding"))
					return axis_preceding;
				else if (name == PUGIXML_TEXT("preceding-sibling"))
					return axis_preceding_sibling;

				break;

			case 's':
				if (name == PUGIXML_TEXT("self"))
					return axis_self;

				break;

			default:
				break;
			}

			specified = false;
			return axis_child;
		}

		nodetest_t parse_node_test_type(const xpath_lexer_string& name)
		{
			switch (name.begin[0])
			{
			case 'c':
				if (name == PUGIXML_TEXT("comment"))
					return nodetest_type_comment;

				break;

			case 'n':
				if (name == PUGIXML_TEXT("node"))
					return nodetest_type_node;

				break;

			case 'p':
				if (name == PUGIXML_TEXT("processing-instruction"))
					return nodetest_type_pi;

				break;

			case 't':
				if (name == PUGIXML_TEXT("text"))
					return nodetest_type_text;

				break;

			default:
				break;
			}

			return nodetest_none;
		}

		// PrimaryExpr ::= VariableReference | '(' Expr ')' | Literal | Number | FunctionCall
		xpath_ast_node* parse_primary_expression()
		{
			switch (_lexer.current())
			{
			case lex_var_ref:
			{
				xpath_lexer_string name = _lexer.contents();

				if (!_variables)
					return error("Unknown variable: variable set is not provided");

				xpath_variable* var = 0;
				if (!get_variable_scratch(_scratch, _variables, name.begin, name.end, &var))
					return error_oom();

				if (!var)
					return error("Unknown variable: variable set does not contain the given name");

				_lexer.next();

				return alloc_node(ast_variable, var->type(), var);
			}

			case lex_open_brace:
			{
				_lexer.next();

				xpath_ast_node* n = parse_expression();
				if (!n) return 0;

				if (_lexer.current() != lex_close_brace)
					return error("Expected ')' to match an opening '('");

				_lexer.next();

				return n;
			}

			case lex_quoted_string:
			{
				const char_t* value = alloc_string(_lexer.contents());
				if (!value) return 0;

				_lexer.next();

				return alloc_node(ast_string_constant, xpath_type_string, value);
			}

			case lex_number:
			{
				double value = 0;

				if (!convert_string_to_number_scratch(_scratch, _lexer.contents().begin, _lexer.contents().end, &value))
					return error_oom();

				_lexer.next();

				return alloc_node(ast_number_constant, xpath_type_number, value);
			}

			case lex_string:
			{
				xpath_ast_node* args[2] = {0};
				size_t argc = 0;

				xpath_lexer_string function = _lexer.contents();
				_lexer.next();

				xpath_ast_node* last_arg = 0;

				if (_lexer.current() != lex_open_brace)
					return error("Unrecognized function call");
				_lexer.next();

				while (_lexer.current() != lex_close_brace)
				{
					if (argc > 0)
					{
						if (_lexer.current() != lex_comma)
							return error("No comma between function arguments");
						_lexer.next();
					}

					xpath_ast_node* n = parse_expression();
					if (!n) return 0;

					if (argc < 2) args[argc] = n;
					else last_arg->set_next(n);

					argc++;
					last_arg = n;
				}

				_lexer.next();

				return parse_function(function, argc, args);
			}

			default:
				return error("Unrecognizable primary expression");
			}
		}

		// FilterExpr ::= PrimaryExpr | FilterExpr Predicate
		// Predicate ::= '[' PredicateExpr ']'
		// PredicateExpr ::= Expr
		xpath_ast_node* parse_filter_expression()
		{
			xpath_ast_node* n = parse_primary_expression();
			if (!n) return 0;

			while (_lexer.current() == lex_open_square_brace)
			{
				_lexer.next();

				if (n->rettype() != xpath_type_node_set)
					return error("Predicate has to be applied to node set");

				xpath_ast_node* expr = parse_expression();
				if (!expr) return 0;

				n = alloc_node(ast_filter, n, expr, predicate_default);
				if (!n) return 0;

				if (_lexer.current() != lex_close_square_brace)
					return error("Expected ']' to match an opening '['");

				_lexer.next();
			}

			return n;
		}

		// Step ::= AxisSpecifier NodeTest Predicate* | AbbreviatedStep
		// AxisSpecifier ::= AxisName '::' | '@'?
		// NodeTest ::= NameTest | NodeType '(' ')' | 'processing-instruction' '(' Literal ')'
		// NameTest ::= '*' | NCName ':' '*' | QName
		// AbbreviatedStep ::= '.' | '..'
		xpath_ast_node* parse_step(xpath_ast_node* set)
		{
			if (set && set->rettype() != xpath_type_node_set)
				return error("Step has to be applied to node set");

			bool axis_specified = false;
			axis_t axis = axis_child; // implied child axis

			if (_lexer.current() == lex_axis_attribute)
			{
				axis = axis_attribute;
				axis_specified = true;

				_lexer.next();
			}
			else if (_lexer.current() == lex_dot)
			{
				_lexer.next();

				if (_lexer.current() == lex_open_square_brace)
					return error("Predicates are not allowed after an abbreviated step");

				return alloc_node(ast_step, set, axis_self, nodetest_type_node, 0);
			}
			else if (_lexer.current() == lex_double_dot)
			{
				_lexer.next();

				if (_lexer.current() == lex_open_square_brace)
					return error("Predicates are not allowed after an abbreviated step");

				return alloc_node(ast_step, set, axis_parent, nodetest_type_node, 0);
			}

			nodetest_t nt_type = nodetest_none;
			xpath_lexer_string nt_name;

			if (_lexer.current() == lex_string)
			{
				// node name test
				nt_name = _lexer.contents();
				_lexer.next();

				// was it an axis name?
				if (_lexer.current() == lex_double_colon)
				{
					// parse axis name
					if (axis_specified)
						return error("Two axis specifiers in one step");

					axis = parse_axis_name(nt_name, axis_specified);

					if (!axis_specified)
						return error("Unknown axis");

					// read actual node test
					_lexer.next();

					if (_lexer.current() == lex_multiply)
					{
						nt_type = nodetest_all;
						nt_name = xpath_lexer_string();
						_lexer.next();
					}
					else if (_lexer.current() == lex_string)
					{
						nt_name = _lexer.contents();
						_lexer.next();
					}
					else
					{
						return error("Unrecognized node test");
					}
				}

				if (nt_type == nodetest_none)
				{
					// node type test or processing-instruction
					if (_lexer.current() == lex_open_brace)
					{
						_lexer.next();

						if (_lexer.current() == lex_close_brace)
						{
							_lexer.next();

							nt_type = parse_node_test_type(nt_name);

							if (nt_type == nodetest_none)
								return error("Unrecognized node type");

							nt_name = xpath_lexer_string();
						}
						else if (nt_name == PUGIXML_TEXT("processing-instruction"))
						{
							if (_lexer.current() != lex_quoted_string)
								return error("Only literals are allowed as arguments to processing-instruction()");

							nt_type = nodetest_pi;
							nt_name = _lexer.contents();
							_lexer.next();

							if (_lexer.current() != lex_close_brace)
								return error("Unmatched brace near processing-instruction()");
							_lexer.next();
						}
						else
						{
							return error("Unmatched brace near node type test");
						}
					}
					// QName or NCName:*
					else
					{
						if (nt_name.end - nt_name.begin > 2 && nt_name.end[-2] == ':' && nt_name.end[-1] == '*') // NCName:*
						{
							nt_name.end--; // erase *

							nt_type = nodetest_all_in_namespace;
						}
						else
						{
							nt_type = nodetest_name;
						}
					}
				}
			}
			else if (_lexer.current() == lex_multiply)
			{
				nt_type = nodetest_all;
				_lexer.next();
			}
			else
			{
				return error("Unrecognized node test");
			}

			const char_t* nt_name_copy = alloc_string(nt_name);
			if (!nt_name_copy) return 0;

			xpath_ast_node* n = alloc_node(ast_step, set, axis, nt_type, nt_name_copy);
			if (!n) return 0;

			xpath_ast_node* last = 0;

			while (_lexer.current() == lex_open_square_brace)
			{
				_lexer.next();

				xpath_ast_node* expr = parse_expression();
				if (!expr) return 0;

				xpath_ast_node* pred = alloc_node(ast_predicate, 0, expr, predicate_default);
				if (!pred) return 0;

				if (_lexer.current() != lex_close_square_brace)
					return error("Expected ']' to match an opening '['");
				_lexer.next();

				if (last) last->set_next(pred);
				else n->set_right(pred);

				last = pred;
			}

			return n;
		}

		// RelativeLocationPath ::= Step | RelativeLocationPath '/' Step | RelativeLocationPath '//' Step
		xpath_ast_node* parse_relative_location_path(xpath_ast_node* set)
		{
			xpath_ast_node* n = parse_step(set);
			if (!n) return 0;

			while (_lexer.current() == lex_slash || _lexer.current() == lex_double_slash)
			{
				lexeme_t l = _lexer.current();
				_lexer.next();

				if (l == lex_double_slash)
				{
					n = alloc_node(ast_step, n, axis_descendant_or_self, nodetest_type_node, 0);
					if (!n) return 0;
				}

				n = parse_step(n);
				if (!n) return 0;
			}

			return n;
		}

		// LocationPath ::= RelativeLocationPath | AbsoluteLocationPath
		// AbsoluteLocationPath ::= '/' RelativeLocationPath? | '//' RelativeLocationPath
		xpath_ast_node* parse_location_path()
		{
			if (_lexer.current() == lex_slash)
			{
				_lexer.next();

				xpath_ast_node* n = alloc_node(ast_step_root, xpath_type_node_set);
				if (!n) return 0;

				// relative location path can start from axis_attribute, dot, double_dot, multiply and string lexemes; any other lexeme means standalone root path
				lexeme_t l = _lexer.current();

				if (l == lex_string || l == lex_axis_attribute || l == lex_dot || l == lex_double_dot || l == lex_multiply)
					return parse_relative_location_path(n);
				else
					return n;
			}
			else if (_lexer.current() == lex_double_slash)
			{
				_lexer.next();

				xpath_ast_node* n = alloc_node(ast_step_root, xpath_type_node_set);
				if (!n) return 0;

				n = alloc_node(ast_step, n, axis_descendant_or_self, nodetest_type_node, 0);
				if (!n) return 0;

				return parse_relative_location_path(n);
			}

			// else clause moved outside of if because of bogus warning 'control may reach end of non-void function being inlined' in gcc 4.0.1
			return parse_relative_location_path(0);
		}

		// PathExpr ::= LocationPath
		//				| FilterExpr
		//				| FilterExpr '/' RelativeLocationPath
		//				| FilterExpr '//' RelativeLocationPath
		// UnionExpr ::= PathExpr | UnionExpr '|' PathExpr
		// UnaryExpr ::= UnionExpr | '-' UnaryExpr
		xpath_ast_node* parse_path_or_unary_expression()
		{
			// Clarification.
			// PathExpr begins with either LocationPath or FilterExpr.
			// FilterExpr begins with PrimaryExpr
			// PrimaryExpr begins with '$' in case of it being a variable reference,
			// '(' in case of it being an expression, string literal, number constant or
			// function call.
			if (_lexer.current() == lex_var_ref || _lexer.current() == lex_open_brace ||
				_lexer.current() == lex_quoted_string || _lexer.current() == lex_number ||
				_lexer.current() == lex_string)
			{
				if (_lexer.current() == lex_string)
				{
					// This is either a function call, or not - if not, we shall proceed with location path
					const char_t* state = _lexer.state();

					while (PUGI__IS_CHARTYPE(*state, ct_space)) ++state;

					if (*state != '(')
						return parse_location_path();

					// This looks like a function call; however this still can be a node-test. Check it.
					if (parse_node_test_type(_lexer.contents()) != nodetest_none)
						return parse_location_path();
				}

				xpath_ast_node* n = parse_filter_expression();
				if (!n) return 0;

				if (_lexer.current() == lex_slash || _lexer.current() == lex_double_slash)
				{
					lexeme_t l = _lexer.current();
					_lexer.next();

					if (l == lex_double_slash)
					{
						if (n->rettype() != xpath_type_node_set)
							return error("Step has to be applied to node set");

						n = alloc_node(ast_step, n, axis_descendant_or_self, nodetest_type_node, 0);
						if (!n) return 0;
					}

					// select from location path
					return parse_relative_location_path(n);
				}

				return n;
			}
			else if (_lexer.current() == lex_minus)
			{
				_lexer.next();

				// precedence 7+ - only parses union expressions
				xpath_ast_node* n = parse_expression(7);
				if (!n) return 0;

				return alloc_node(ast_op_negate, xpath_type_number, n);
			}
			else
			{
				return parse_location_path();
			}
		}

		struct binary_op_t
		{
			ast_type_t asttype;
			xpath_value_type rettype;
			int precedence;

			binary_op_t(): asttype(ast_unknown), rettype(xpath_type_none), precedence(0)
			{
			}

			binary_op_t(ast_type_t asttype_, xpath_value_type rettype_, int precedence_): asttype(asttype_), rettype(rettype_), precedence(precedence_)
			{
			}

			static binary_op_t parse(xpath_lexer& lexer)
			{
				switch (lexer.current())
				{
				case lex_string:
					if (lexer.contents() == PUGIXML_TEXT("or"))
						return binary_op_t(ast_op_or, xpath_type_boolean, 1);
					else if (lexer.contents() == PUGIXML_TEXT("and"))
						return binary_op_t(ast_op_and, xpath_type_boolean, 2);
					else if (lexer.contents() == PUGIXML_TEXT("div"))
						return binary_op_t(ast_op_divide, xpath_type_number, 6);
					else if (lexer.contents() == PUGIXML_TEXT("mod"))
						return binary_op_t(ast_op_mod, xpath_type_number, 6);
					else
						return binary_op_t();

				case lex_equal:
					return binary_op_t(ast_op_equal, xpath_type_boolean, 3);

				case lex_not_equal:
					return binary_op_t(ast_op_not_equal, xpath_type_boolean, 3);

				case lex_less:
					return binary_op_t(ast_op_less, xpath_type_boolean, 4);

				case lex_greater:
					return binary_op_t(ast_op_greater, xpath_type_boolean, 4);

				case lex_less_or_equal:
					return binary_op_t(ast_op_less_or_equal, xpath_type_boolean, 4);

				case lex_greater_or_equal:
					return binary_op_t(ast_op_greater_or_equal, xpath_type_boolean, 4);

				case lex_plus:
					return binary_op_t(ast_op_add, xpath_type_number, 5);

				case lex_minus:
					return binary_op_t(ast_op_subtract, xpath_type_number, 5);

				case lex_multiply:
					return binary_op_t(ast_op_multiply, xpath_type_number, 6);

				case lex_union:
					return binary_op_t(ast_op_union, xpath_type_node_set, 7);

				default:
					return binary_op_t();
				}
			}
		};

		xpath_ast_node* parse_expression_rec(xpath_ast_node* lhs, int limit)
		{
			binary_op_t op = binary_op_t::parse(_lexer);

			while (op.asttype != ast_unknown && op.precedence >= limit)
			{
				_lexer.next();

				xpath_ast_node* rhs = parse_path_or_unary_expression();
				if (!rhs) return 0;

				binary_op_t nextop = binary_op_t::parse(_lexer);

				while (nextop.asttype != ast_unknown && nextop.precedence > op.precedence)
				{
					rhs = parse_expression_rec(rhs, nextop.precedence);
					if (!rhs) return 0;

					nextop = binary_op_t::parse(_lexer);
				}

				if (op.asttype == ast_op_union && (lhs->rettype() != xpath_type_node_set || rhs->rettype() != xpath_type_node_set))
					return error("Union operator has to be applied to node sets");

				lhs = alloc_node(op.asttype, op.rettype, lhs, rhs);
				if (!lhs) return 0;

				op = binary_op_t::parse(_lexer);
			}

			return lhs;
		}

		// Expr ::= OrExpr
		// OrExpr ::= AndExpr | OrExpr 'or' AndExpr
		// AndExpr ::= EqualityExpr | AndExpr 'and' EqualityExpr
		// EqualityExpr ::= RelationalExpr
		//					| EqualityExpr '=' RelationalExpr
		//					| EqualityExpr '!=' RelationalExpr
		// RelationalExpr ::= AdditiveExpr
		//					  | RelationalExpr '<' AdditiveExpr
		//					  | RelationalExpr '>' AdditiveExpr
		//					  | RelationalExpr '<=' AdditiveExpr
		//					  | RelationalExpr '>=' AdditiveExpr
		// AdditiveExpr ::= MultiplicativeExpr
		//					| AdditiveExpr '+' MultiplicativeExpr
		//					| AdditiveExpr '-' MultiplicativeExpr
		// MultiplicativeExpr ::= UnaryExpr
		//						  | MultiplicativeExpr '*' UnaryExpr
		//						  | MultiplicativeExpr 'div' UnaryExpr
		//						  | MultiplicativeExpr 'mod' UnaryExpr
		xpath_ast_node* parse_expression(int limit = 0)
		{
			xpath_ast_node* n = parse_path_or_unary_expression();
			if (!n) return 0;

			return parse_expression_rec(n, limit);
		}

		xpath_parser(const char_t* query, xpath_variable_set* variables, xpath_allocator* alloc, xpath_parse_result* result): _alloc(alloc), _lexer(query), _query(query), _variables(variables), _result(result)
		{
		}

		xpath_ast_node* parse()
		{
			xpath_ast_node* n = parse_expression();
			if (!n) return 0;

			// check if there are unparsed tokens left
			if (_lexer.current() != lex_eof)
				return error("Incorrect query");

			return n;
		}

		static xpath_ast_node* parse(const char_t* query, xpath_variable_set* variables, xpath_allocator* alloc, xpath_parse_result* result)
		{
			xpath_parser parser(query, variables, alloc, result);

			return parser.parse();
		}
	};

	struct xpath_query_impl
	{
		static xpath_query_impl* create()
		{
			void* memory = xml_memory::allocate(sizeof(xpath_query_impl));
			if (!memory) return 0;

			return new (memory) xpath_query_impl();
		}

		static void destroy(xpath_query_impl* impl)
		{
			// free all allocated pages
			impl->alloc.release();

			// free allocator memory (with the first page)
			xml_memory::deallocate(impl);
		}

		xpath_query_impl(): root(0), alloc(&block, &oom), oom(false)
		{
			block.next = 0;
			block.capacity = sizeof(block.data);
		}

		xpath_ast_node* root;
		xpath_allocator alloc;
		xpath_memory_block block;
		bool oom;
	};

	PUGI__FN impl::xpath_ast_node* evaluate_node_set_prepare(xpath_query_impl* impl)
	{
		if (!impl) return 0;

		if (impl->root->rettype() != xpath_type_node_set)
		{
		#ifdef PUGIXML_NO_EXCEPTIONS
			return 0;
		#else
			xpath_parse_result res;
			res.error = "Expression does not evaluate to node set";

			throw xpath_exception(res);
		#endif
		}

		return impl->root;
	}
PUGI__NS_END

namespace pugi
{
#ifndef PUGIXML_NO_EXCEPTIONS
	PUGI__FN xpath_exception::xpath_exception(const xpath_parse_result& result_): _result(result_)
	{
		assert(_result.error);
	}

	PUGI__FN const char* xpath_exception::what() const throw()
	{
		return _result.error;
	}

	PUGI__FN const xpath_parse_result& xpath_exception::result() const
	{
		return _result;
	}
#endif

	PUGI__FN xpath_node::xpath_node()
	{
	}

	PUGI__FN xpath_node::xpath_node(const xml_node& node_): _node(node_)
	{
	}

	PUGI__FN xpath_node::xpath_node(const xml_attribute& attribute_, const xml_node& parent_): _node(attribute_ ? parent_ : xml_node()), _attribute(attribute_)
	{
	}

	PUGI__FN xml_node xpath_node::node() const
	{
		return _attribute ? xml_node() : _node;
	}

	PUGI__FN xml_attribute xpath_node::attribute() const
	{
		return _attribute;
	}

	PUGI__FN xml_node xpath_node::parent() const
	{
		return _attribute ? _node : _node.parent();
	}

	PUGI__FN static void unspecified_bool_xpath_node(xpath_node***)
	{
	}

	PUGI__FN xpath_node::operator xpath_node::unspecified_bool_type() const
	{
		return (_node || _attribute) ? unspecified_bool_xpath_node : 0;
	}

	PUGI__FN bool xpath_node::operator!() const
	{
		return !(_node || _attribute);
	}

	PUGI__FN bool xpath_node::operator==(const xpath_node& n) const
	{
		return _node == n._node && _attribute == n._attribute;
	}

	PUGI__FN bool xpath_node::operator!=(const xpath_node& n) const
	{
		return _node != n._node || _attribute != n._attribute;
	}

#ifdef __BORLANDC__
	PUGI__FN bool operator&&(const xpath_node& lhs, bool rhs)
	{
		return (bool)lhs && rhs;
	}

	PUGI__FN bool operator||(const xpath_node& lhs, bool rhs)
	{
		return (bool)lhs || rhs;
	}
#endif

	PUGI__FN void xpath_node_set::_assign(const_iterator begin_, const_iterator end_, type_t type_)
	{
		assert(begin_ <= end_);

		size_t size_ = static_cast<size_t>(end_ - begin_);

		if (size_ <= 1)
		{
			// deallocate old buffer
			if (_begin != &_storage) impl::xml_memory::deallocate(_begin);

			// use internal buffer
			if (begin_ != end_) _storage = *begin_;

			_begin = &_storage;
			_end = &_storage + size_;
			_type = type_;
		}
		else
		{
			// make heap copy
			xpath_node* storage = static_cast<xpath_node*>(impl::xml_memory::allocate(size_ * sizeof(xpath_node)));

			if (!storage)
			{
			#ifdef PUGIXML_NO_EXCEPTIONS
				return;
			#else
				throw std::bad_alloc();
			#endif
			}

			memcpy(storage, begin_, size_ * sizeof(xpath_node));

			// deallocate old buffer
			if (_begin != &_storage) impl::xml_memory::deallocate(_begin);

			// finalize
			_begin = storage;
			_end = storage + size_;
			_type = type_;
		}
	}

#ifdef PUGIXML_HAS_MOVE
	PUGI__FN void xpath_node_set::_move(xpath_node_set& rhs)
	{
		_type = rhs._type;
		_storage = rhs._storage;
		_begin = (rhs._begin == &rhs._storage) ? &_storage : rhs._begin;
		_end = _begin + (rhs._end - rhs._begin);

		rhs._type = type_unsorted;
		rhs._begin = &rhs._storage;
		rhs._end = rhs._begin;
	}
#endif

	PUGI__FN xpath_node_set::xpath_node_set(): _type(type_unsorted), _begin(&_storage), _end(&_storage)
	{
	}

	PUGI__FN xpath_node_set::xpath_node_set(const_iterator begin_, const_iterator end_, type_t type_): _type(type_unsorted), _begin(&_storage), _end(&_storage)
	{
		_assign(begin_, end_, type_);
	}

	PUGI__FN xpath_node_set::~xpath_node_set()
	{
		if (_begin != &_storage)
			impl::xml_memory::deallocate(_begin);
	}

	PUGI__FN xpath_node_set::xpath_node_set(const xpath_node_set& ns): _type(type_unsorted), _begin(&_storage), _end(&_storage)
	{
		_assign(ns._begin, ns._end, ns._type);
	}

	PUGI__FN xpath_node_set& xpath_node_set::operator=(const xpath_node_set& ns)
	{
		if (this == &ns) return *this;

		_assign(ns._begin, ns._end, ns._type);

		return *this;
	}

#ifdef PUGIXML_HAS_MOVE
	PUGI__FN xpath_node_set::xpath_node_set(xpath_node_set&& rhs): _type(type_unsorted), _begin(&_storage), _end(&_storage)
	{
		_move(rhs);
	}

	PUGI__FN xpath_node_set& xpath_node_set::operator=(xpath_node_set&& rhs)
	{
		if (this == &rhs) return *this;

		if (_begin != &_storage)
			impl::xml_memory::deallocate(_begin);

		_move(rhs);

		return *this;
	}
#endif

	PUGI__FN xpath_node_set::type_t xpath_node_set::type() const
	{
		return _type;
	}

	PUGI__FN size_t xpath_node_set::size() const
	{
		return _end - _begin;
	}

	PUGI__FN bool xpath_node_set::empty() const
	{
		return _begin == _end;
	}

	PUGI__FN const xpath_node& xpath_node_set::operator[](size_t index) const
	{
		assert(index < size());
		return _begin[index];
	}

	PUGI__FN xpath_node_set::const_iterator xpath_node_set::begin() const
	{
		return _begin;
	}

	PUGI__FN xpath_node_set::const_iterator xpath_node_set::end() const
	{
		return _end;
	}

	PUGI__FN void xpath_node_set::sort(bool reverse)
	{
		_type = impl::xpath_sort(_begin, _end, _type, reverse);
	}

	PUGI__FN xpath_node xpath_node_set::first() const
	{
		return impl::xpath_first(_begin, _end, _type);
	}

	PUGI__FN xpath_parse_result::xpath_parse_result(): error("Internal error"), offset(0)
	{
	}

	PUGI__FN xpath_parse_result::operator bool() const
	{
		return error == 0;
	}

	PUGI__FN const char* xpath_parse_result::description() const
	{
		return error ? error : "No error";
	}

	PUGI__FN xpath_variable::xpath_variable(xpath_value_type type_): _type(type_), _next(0)
	{
	}

	PUGI__FN const char_t* xpath_variable::name() const
	{
		switch (_type)
		{
		case xpath_type_node_set:
			return static_cast<const impl::xpath_variable_node_set*>(this)->name;

		case xpath_type_number:
			return static_cast<const impl::xpath_variable_number*>(this)->name;

		case xpath_type_string:
			return static_cast<const impl::xpath_variable_string*>(this)->name;

		case xpath_type_boolean:
			return static_cast<const impl::xpath_variable_boolean*>(this)->name;

		default:
			assert(false && "Invalid variable type"); // unreachable
			return 0;
		}
	}

	PUGI__FN xpath_value_type xpath_variable::type() const
	{
		return _type;
	}

	PUGI__FN bool xpath_variable::get_boolean() const
	{
		return (_type == xpath_type_boolean) ? static_cast<const impl::xpath_variable_boolean*>(this)->value : false;
	}

	PUGI__FN double xpath_variable::get_number() const
	{
		return (_type == xpath_type_number) ? static_cast<const impl::xpath_variable_number*>(this)->value : impl::gen_nan();
	}

	PUGI__FN const char_t* xpath_variable::get_string() const
	{
		const char_t* value = (_type == xpath_type_string) ? static_cast<const impl::xpath_variable_string*>(this)->value : 0;
		return value ? value : PUGIXML_TEXT("");
	}

	PUGI__FN const xpath_node_set& xpath_variable::get_node_set() const
	{
		return (_type == xpath_type_node_set) ? static_cast<const impl::xpath_variable_node_set*>(this)->value : impl::dummy_node_set;
	}

	PUGI__FN bool xpath_variable::set(bool value)
	{
		if (_type != xpath_type_boolean) return false;

		static_cast<impl::xpath_variable_boolean*>(this)->value = value;
		return true;
	}

	PUGI__FN bool xpath_variable::set(double value)
	{
		if (_type != xpath_type_number) return false;

		static_cast<impl::xpath_variable_number*>(this)->value = value;
		return true;
	}

	PUGI__FN bool xpath_variable::set(const char_t* value)
	{
		if (_type != xpath_type_string) return false;

		impl::xpath_variable_string* var = static_cast<impl::xpath_variable_string*>(this);

		// duplicate string
		size_t size = (impl::strlength(value) + 1) * sizeof(char_t);

		char_t* copy = static_cast<char_t*>(impl::xml_memory::allocate(size));
		if (!copy) return false;

		memcpy(copy, value, size);

		// replace old string
		if (var->value) impl::xml_memory::deallocate(var->value);
		var->value = copy;

		return true;
	}

	PUGI__FN bool xpath_variable::set(const xpath_node_set& value)
	{
		if (_type != xpath_type_node_set) return false;

		static_cast<impl::xpath_variable_node_set*>(this)->value = value;
		return true;
	}

	PUGI__FN xpath_variable_set::xpath_variable_set()
	{
		for (size_t i = 0; i < sizeof(_data) / sizeof(_data[0]); ++i)
			_data[i] = 0;
	}

	PUGI__FN xpath_variable_set::~xpath_variable_set()
	{
		for (size_t i = 0; i < sizeof(_data) / sizeof(_data[0]); ++i)
			_destroy(_data[i]);
	}

	PUGI__FN xpath_variable_set::xpath_variable_set(const xpath_variable_set& rhs)
	{
		for (size_t i = 0; i < sizeof(_data) / sizeof(_data[0]); ++i)
			_data[i] = 0;

		_assign(rhs);
	}

	PUGI__FN xpath_variable_set& xpath_variable_set::operator=(const xpath_variable_set& rhs)
	{
		if (this == &rhs) return *this;

		_assign(rhs);

		return *this;
	}

#ifdef PUGIXML_HAS_MOVE
	PUGI__FN xpath_variable_set::xpath_variable_set(xpath_variable_set&& rhs)
	{
		for (size_t i = 0; i < sizeof(_data) / sizeof(_data[0]); ++i)
		{
			_data[i] = rhs._data[i];
			rhs._data[i] = 0;
		}
	}

	PUGI__FN xpath_variable_set& xpath_variable_set::operator=(xpath_variable_set&& rhs)
	{
		for (size_t i = 0; i < sizeof(_data) / sizeof(_data[0]); ++i)
		{
			_destroy(_data[i]);

			_data[i] = rhs._data[i];
			rhs._data[i] = 0;
		}

		return *this;
	}
#endif

	PUGI__FN void xpath_variable_set::_assign(const xpath_variable_set& rhs)
	{
		xpath_variable_set temp;

		for (size_t i = 0; i < sizeof(_data) / sizeof(_data[0]); ++i)
			if (rhs._data[i] && !_clone(rhs._data[i], &temp._data[i]))
				return;

		_swap(temp);
	}

	PUGI__FN void xpath_variable_set::_swap(xpath_variable_set& rhs)
	{
		for (size_t i = 0; i < sizeof(_data) / sizeof(_data[0]); ++i)
		{
			xpath_variable* chain = _data[i];

			_data[i] = rhs._data[i];
			rhs._data[i] = chain;
		}
	}

	PUGI__FN xpath_variable* xpath_variable_set::_find(const char_t* name) const
	{
		const size_t hash_size = sizeof(_data) / sizeof(_data[0]);
		size_t hash = impl::hash_string(name) % hash_size;

		// look for existing variable
		for (xpath_variable* var = _data[hash]; var; var = var->_next)
			if (impl::strequal(var->name(), name))
				return var;

		return 0;
	}

	PUGI__FN bool xpath_variable_set::_clone(xpath_variable* var, xpath_variable** out_result)
	{
		xpath_variable* last = 0;

		while (var)
		{
			// allocate storage for new variable
			xpath_variable* nvar = impl::new_xpath_variable(var->_type, var->name());
			if (!nvar) return false;

			// link the variable to the result immediately to handle failures gracefully
			if (last)
				last->_next = nvar;
			else
				*out_result = nvar;

			last = nvar;

			// copy the value; this can fail due to out-of-memory conditions
			if (!impl::copy_xpath_variable(nvar, var)) return false;

			var = var->_next;
		}

		return true;
	}

	PUGI__FN void xpath_variable_set::_destroy(xpath_variable* var)
	{
		while (var)
		{
			xpath_variable* next = var->_next;

			impl::delete_xpath_variable(var->_type, var);

			var = next;
		}
	}

	PUGI__FN xpath_variable* xpath_variable_set::add(const char_t* name, xpath_value_type type)
	{
		const size_t hash_size = sizeof(_data) / sizeof(_data[0]);
		size_t hash = impl::hash_string(name) % hash_size;

		// look for existing variable
		for (xpath_variable* var = _data[hash]; var; var = var->_next)
			if (impl::strequal(var->name(), name))
				return var->type() == type ? var : 0;

		// add new variable
		xpath_variable* result = impl::new_xpath_variable(type, name);

		if (result)
		{
			result->_next = _data[hash];

			_data[hash] = result;
		}

		return result;
	}

	PUGI__FN bool xpath_variable_set::set(const char_t* name, bool value)
	{
		xpath_variable* var = add(name, xpath_type_boolean);
		return var ? var->set(value) : false;
	}

	PUGI__FN bool xpath_variable_set::set(const char_t* name, double value)
	{
		xpath_variable* var = add(name, xpath_type_number);
		return var ? var->set(value) : false;
	}

	PUGI__FN bool xpath_variable_set::set(const char_t* name, const char_t* value)
	{
		xpath_variable* var = add(name, xpath_type_string);
		return var ? var->set(value) : false;
	}

	PUGI__FN bool xpath_variable_set::set(const char_t* name, const xpath_node_set& value)
	{
		xpath_variable* var = add(name, xpath_type_node_set);
		return var ? var->set(value) : false;
	}

	PUGI__FN xpath_variable* xpath_variable_set::get(const char_t* name)
	{
		return _find(name);
	}

	PUGI__FN const xpath_variable* xpath_variable_set::get(const char_t* name) const
	{
		return _find(name);
	}

	PUGI__FN xpath_query::xpath_query(const char_t* query, xpath_variable_set* variables): _impl(0)
	{
		impl::xpath_query_impl* qimpl = impl::xpath_query_impl::create();

		if (!qimpl)
		{
		#ifdef PUGIXML_NO_EXCEPTIONS
			_result.error = "Out of memory";
		#else
			throw std::bad_alloc();
		#endif
		}
		else
		{
			using impl::auto_deleter; // MSVC7 workaround
			auto_deleter<impl::xpath_query_impl> impl(qimpl, impl::xpath_query_impl::destroy);

			qimpl->root = impl::xpath_parser::parse(query, variables, &qimpl->alloc, &_result);

			if (qimpl->root)
			{
				qimpl->root->optimize(&qimpl->alloc);

				_impl = impl.release();
				_result.error = 0;
			}
			else
			{
			#ifdef PUGIXML_NO_EXCEPTIONS
				if (qimpl->oom) _result.error = "Out of memory";
			#else
				if (qimpl->oom) throw std::bad_alloc();
				throw xpath_exception(_result);
			#endif
			}
		}
	}

	PUGI__FN xpath_query::xpath_query(): _impl(0)
	{
	}

	PUGI__FN xpath_query::~xpath_query()
	{
		if (_impl)
			impl::xpath_query_impl::destroy(static_cast<impl::xpath_query_impl*>(_impl));
	}

#ifdef PUGIXML_HAS_MOVE
	PUGI__FN xpath_query::xpath_query(xpath_query&& rhs)
	{
		_impl = rhs._impl;
		_result = rhs._result;
		rhs._impl = 0;
		rhs._result = xpath_parse_result();
	}

	PUGI__FN xpath_query& xpath_query::operator=(xpath_query&& rhs)
	{
		if (this == &rhs) return *this;

		if (_impl)
			impl::xpath_query_impl::destroy(static_cast<impl::xpath_query_impl*>(_impl));

		_impl = rhs._impl;
		_result = rhs._result;
		rhs._impl = 0;
		rhs._result = xpath_parse_result();

		return *this;
	}
#endif

	PUGI__FN xpath_value_type xpath_query::return_type() const
	{
		if (!_impl) return xpath_type_none;

		return static_cast<impl::xpath_query_impl*>(_impl)->root->rettype();
	}

	PUGI__FN bool xpath_query::evaluate_boolean(const xpath_node& n) const
	{
		if (!_impl) return false;

		impl::xpath_context c(n, 1, 1);
		impl::xpath_stack_data sd;

		bool r = static_cast<impl::xpath_query_impl*>(_impl)->root->eval_boolean(c, sd.stack);

		if (sd.oom)
		{
		#ifdef PUGIXML_NO_EXCEPTIONS
			return false;
		#else
			throw std::bad_alloc();
		#endif
		}

		return r;
	}

	PUGI__FN double xpath_query::evaluate_number(const xpath_node& n) const
	{
		if (!_impl) return impl::gen_nan();

		impl::xpath_context c(n, 1, 1);
		impl::xpath_stack_data sd;

		double r = static_cast<impl::xpath_query_impl*>(_impl)->root->eval_number(c, sd.stack);

		if (sd.oom)
		{
		#ifdef PUGIXML_NO_EXCEPTIONS
			return impl::gen_nan();
		#else
			throw std::bad_alloc();
		#endif
		}

		return r;
	}

#ifndef PUGIXML_NO_STL
	PUGI__FN string_t xpath_query::evaluate_string(const xpath_node& n) const
	{
		if (!_impl) return string_t();

		impl::xpath_context c(n, 1, 1);
		impl::xpath_stack_data sd;

		impl::xpath_string r = static_cast<impl::xpath_query_impl*>(_impl)->root->eval_string(c, sd.stack);

		if (sd.oom)
		{
		#ifdef PUGIXML_NO_EXCEPTIONS
			return string_t();
		#else
			throw std::bad_alloc();
		#endif
		}

		return string_t(r.c_str(), r.length());
	}
#endif

	PUGI__FN size_t xpath_query::evaluate_string(char_t* buffer, size_t capacity, const xpath_node& n) const
	{
		impl::xpath_context c(n, 1, 1);
		impl::xpath_stack_data sd;

		impl::xpath_string r = _impl ? static_cast<impl::xpath_query_impl*>(_impl)->root->eval_string(c, sd.stack) : impl::xpath_string();

		if (sd.oom)
		{
		#ifdef PUGIXML_NO_EXCEPTIONS
			r = impl::xpath_string();
		#else
			throw std::bad_alloc();
		#endif
		}

		size_t full_size = r.length() + 1;

		if (capacity > 0)
		{
			size_t size = (full_size < capacity) ? full_size : capacity;
			assert(size > 0);

			memcpy(buffer, r.c_str(), (size - 1) * sizeof(char_t));
			buffer[size - 1] = 0;
		}

		return full_size;
	}

	PUGI__FN xpath_node_set xpath_query::evaluate_node_set(const xpath_node& n) const
	{
		impl::xpath_ast_node* root = impl::evaluate_node_set_prepare(static_cast<impl::xpath_query_impl*>(_impl));
		if (!root) return xpath_node_set();

		impl::xpath_context c(n, 1, 1);
		impl::xpath_stack_data sd;

		impl::xpath_node_set_raw r = root->eval_node_set(c, sd.stack, impl::nodeset_eval_all);

		if (sd.oom)
		{
		#ifdef PUGIXML_NO_EXCEPTIONS
			return xpath_node_set();
		#else
			throw std::bad_alloc();
		#endif
		}

		return xpath_node_set(r.begin(), r.end(), r.type());
	}

	PUGI__FN xpath_node xpath_query::evaluate_node(const xpath_node& n) const
	{
		impl::xpath_ast_node* root = impl::evaluate_node_set_prepare(static_cast<impl::xpath_query_impl*>(_impl));
		if (!root) return xpath_node();

		impl::xpath_context c(n, 1, 1);
		impl::xpath_stack_data sd;

		impl::xpath_node_set_raw r = root->eval_node_set(c, sd.stack, impl::nodeset_eval_first);

		if (sd.oom)
		{
		#ifdef PUGIXML_NO_EXCEPTIONS
			return xpath_node();
		#else
			throw std::bad_alloc();
		#endif
		}

		return r.first();
	}

	PUGI__FN const xpath_parse_result& xpath_query::result() const
	{
		return _result;
	}

	PUGI__FN static void unspecified_bool_xpath_query(xpath_query***)
	{
	}

	PUGI__FN xpath_query::operator xpath_query::unspecified_bool_type() const
	{
		return _impl ? unspecified_bool_xpath_query : 0;
	}

	PUGI__FN bool xpath_query::operator!() const
	{
		return !_impl;
	}

	PUGI__FN xpath_node xml_node::select_node(const char_t* query, xpath_variable_set* variables) const
	{
		xpath_query q(query, variables);
		return q.evaluate_node(*this);
	}

	PUGI__FN xpath_node xml_node::select_node(const xpath_query& query) const
	{
		return query.evaluate_node(*this);
	}

	PUGI__FN xpath_node_set xml_node::select_nodes(const char_t* query, xpath_variable_set* variables) const
	{
		xpath_query q(query, variables);
		return q.evaluate_node_set(*this);
	}

	PUGI__FN xpath_node_set xml_node::select_nodes(const xpath_query& query) const
	{
		return query.evaluate_node_set(*this);
	}

	PUGI__FN xpath_node xml_node::select_single_node(const char_t* query, xpath_variable_set* variables) const
	{
		xpath_query q(query, variables);
		return q.evaluate_node(*this);
	}

	PUGI__FN xpath_node xml_node::select_single_node(const xpath_query& query) const
	{
		return query.evaluate_node(*this);
	}
}

#endif

#ifdef __BORLANDC__
#	pragma option pop
#endif

// Intel C++ does not properly keep warning state for function templates,
// so popping warning state at the end of translation unit leads to warnings in the middle.
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER)
#	pragma warning(pop)
#endif

#if defined(_MSC_VER) && defined(__c2__)
#	pragma clang diagnostic pop
#endif

// Undefine all local macros (makes sure we're not leaking macros in header-only mode)
#undef PUGI__NO_INLINE
#undef PUGI__UNLIKELY
#undef PUGI__STATIC_ASSERT
#undef PUGI__DMC_VOLATILE
#undef PUGI__UNSIGNED_OVERFLOW
#undef PUGI__MSVC_CRT_VERSION
#undef PUGI__SNPRINTF
#undef PUGI__NS_BEGIN
#undef PUGI__NS_END
#undef PUGI__FN
#undef PUGI__FN_NO_INLINE
#undef PUGI__GETHEADER_IMPL
#undef PUGI__GETPAGE_IMPL
#undef PUGI__GETPAGE
#undef PUGI__NODETYPE
#undef PUGI__IS_CHARTYPE_IMPL
#undef PUGI__IS_CHARTYPE
#undef PUGI__IS_CHARTYPEX
#undef PUGI__ENDSWITH
#undef PUGI__SKIPWS
#undef PUGI__OPTSET
#undef PUGI__PUSHNODE
#undef PUGI__POPNODE
#undef PUGI__SCANFOR
#undef PUGI__SCANWHILE
#undef PUGI__SCANWHILE_UNROLL
#undef PUGI__ENDSEG
#undef PUGI__THROW_ERROR
#undef PUGI__CHECK_ERROR

#endif

/**
 * Copyright (c) 2006-2017 Arseny Kapoulkine
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */
