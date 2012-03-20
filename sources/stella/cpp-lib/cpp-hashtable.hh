// -*- Mode: C++ -*-

/*--------------------------- BEGIN LICENSE BLOCK ---------------------------+
|                                                                            |
| Version: MPL 1.1/GPL 2.0/LGPL 2.1                                          |
|                                                                            |
| The contents of this file are subject to the Mozilla Public License        |
| Version 1.1 (the "License"); you may not use this file except in           |
| compliance with the License. You may obtain a copy of the License at       |
| http://www.mozilla.org/MPL/                                                |
|                                                                            |
| Software distributed under the License is distributed on an "AS IS" basis, |
| WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   |
| for the specific language governing rights and limitations under the       |
| License.                                                                   |
|                                                                            |
| The Original Code is the STELLA Programming Language.                      |
|                                                                            |
| The Initial Developer of the Original Code is                              |
| UNIVERSITY OF SOUTHERN CALIFORNIA, INFORMATION SCIENCES INSTITUTE          |
| 4676 Admiralty Way, Marina Del Rey, California 90292, U.S.A.               |
|                                                                            |
| Portions created by the Initial Developer are Copyright (C) 1996-2006      |
| the Initial Developer. All Rights Reserved.                                |
|                                                                            |
| Contributor(s):                                                            |
|                                                                            |
| Alternatively, the contents of this file may be used under the terms of    |
| either the GNU General Public License Version 2 or later (the "GPL"), or   |
| the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),   |
| in which case the provisions of the GPL or the LGPL are applicable instead |
| of those above. If you wish to allow use of your version of this file only |
| under the terms of either the GPL or the LGPL, and not to allow others to  |
| use your version of this file under the terms of the MPL, indicate your    |
| decision by deleting the provisions above and replace them with the notice |
| and other provisions required by the GPL or the LGPL. If you do not delete |
| the provisions above, a recipient may use your version of this file under  |
| the terms of any one of the MPL, the GPL or the LGPL.                      |
|                                                                            |
+---------------------------- END LICENSE BLOCK ----------------------------*/

// Version: cpp-hashtable.hh,v 1.9 2006/05/09 20:33:17 hans Exp

// Native C++ hash table support for STELLA based on STL extensions

// Unfortunately, hash tables did not make it into the STL on time, which
// means we have to do some extra work to make them available.  The SGI
// implementation of the STL includes support for hash tables, so, if we
// detect that particular STL implementation, we'll use it.  Unfortunately2,
// their implementation can't be groked by g++ versions prior to 2.8, which
// means we have to keep an alternative around.  For now, we simply use the
// old hash table template implementation in that case (which means we don't
// support integer and float hash tables).  In the future, we'll templatize
// one of them and by doing that supply our own portable implementation.

// The native hash table interface is now hidden by the template class
// `Native_EQL_Hash_Table<Key_Type, Data_Type>', which means we can plug
// in a different implementation without touching any of the primal stuff.


// LEAVE `stella' NAMESPACE, since some of the templates instantiations below 
// have to be defined in the same namespace as the template (this file assumes
// it is included in the middle of `primal.cc'):

} // end of namespace stella


// figure out whether we have the STL by SGI which implements hash maps
// (assumes basic STL support is available):
#include <map> 

#ifdef _SGI_STL_MAP
#  define HAVE_SGI_HASH_MAPS
#endif
#ifdef __SGI_STL_MAP
#  define HAVE_SGI_HASH_MAPS
#endif

#ifdef HAVE_SGI_HASH_MAPS
#include <hash_map>

struct hash<stella::Object*> {
  // Hash function for `Object' pointers.  Assumes a non-copying GC.
  size_t operator()(const stella::Object* x) const { return (size_t)x; }
};

// IMPROVE THIS:
struct hash<double> {
  // Hash function for double floats.
  size_t operator()(const double x) const { return (size_t)x; }
};

struct equal_to<char*> {
  // Hash table `eql?' operator for strings.
  bool operator()(const char* s1, const char* s2) const
    {return strcmp(s1, s2) == 0;}
};

namespace stella {

class gc_alloc {
  // Special allocator that needs to be used by the STL hash table implementation
  //    to ensure proper garbage collection.
  // Otherwise, even if we free obsolete hash tables via GC finalization, mixing
  //    the GC allocator and the STL-internal one leads to bizarre interactions
  //    that seem to every once in a while corrupt objects that are still in use.
public:
  static void* allocate(size_t n) {return GC_MALLOC(n);}
  static void  deallocate(void *p, size_t n) {GC_FREE(p);}
  static void* reallocate(void *p, size_t old_sz, size_t new_sz)
    {return GC_REALLOC(p, new_sz);}
};

template <class Key_Type, class Data_Type> class Native_EQL_Hash_Table :
  public hash_map<Key_Type, Data_Type, hash<Key_Type>, equal_to<Key_Type>,
#ifdef __STL_USE_STD_ALLOCATORS
                  // we need an adaptor; check whether this slows us down:
                  __allocator<Data_Type, gc_alloc>
#else
                  gc_alloc
#endif
                 >,
  public gc {
public:
  Data_Type null_value; // making this static causes linking problems, -l problem?

  Native_EQL_Hash_Table(Data_Type nv = (Data_Type)NULL) {null_value = nv;}

  Data_Type get(Key_Type key) {
    // Return `key'->value or `null_value' if the value is undefined.
    // Need this, since the default []-operator inserts a dummy value in
    //    the undefined case.
    const_iterator pos = find(key);
    if (pos == end())
      return (null_value);
    else
      return ((*pos).second);
  }
};
} // end of namespace stella


#else

// Provide dummy implementations (these used to be implemented by the old
// C++ hash table library);  now we use STELLA hash tables in this case.

namespace stella {

template <class Key_Type, class Data_Type> class Native_EQL_Hash_Table :
  public gc {
public:
  Native_EQL_Hash_Table(Data_Type nv = (Data_Type)NULL) {
    std::cerr << "new Native_EQL_Hash_Table: Unimplemented hash table type" << std::endl;
    std::cerr << ((char*)nv)[0];
  }
  Data_Type get(Key_Type key) {}
  void erase(Key_Type key) {}
  Data_Type& operator[](Key_Type key) {}
};

template <> class Native_EQL_Hash_Table<char*, int> {
public:
  Native_EQL_Hash_Table(int nv) {}
  int get(char* key) {}
  void erase(char* key) {}
  int& operator[](char* key) {}
};

template <> class Native_EQL_Hash_Table<char*, Object*> {
public:
  Native_EQL_Hash_Table(Object* nv = NULL) {}
  Object* get(char* key) {}
  void erase(char* key) {}
  Object*& operator[](char* key) {}
};

template <> class Native_EQL_Hash_Table<Object*, Object*> {
public:
  Native_EQL_Hash_Table(Object* nv = NULL) {}
  Object* get(Object* key) {}
  void erase(Object* key) {}
  Object*& operator[](Object* key) {}
};

template <> class Native_EQL_Hash_Table<int, Object*> {
public:
Native_EQL_Hash_Table(Object* nv = (Object*)NULL) {}
  Object* get(int key) {}
  void erase(int key) {}
  Object*& operator[](int key) {}
};

template <> class Native_EQL_Hash_Table<double, Object*> {
public:
Native_EQL_Hash_Table(Object* nv = (Object*)NULL) {}
  Object* get(double key) {}
  void erase(double key) {}
  Object*& operator[](double key) {}
};

} // end of namespace stella

#endif /* HAVE_SGI_HASH_MAPS */


// REENTER `stella' NAMESPACE:
namespace stella {
