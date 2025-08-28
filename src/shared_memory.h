#pragma once

#include <Rcpp.h>

#include "memory_page.h"
#include <memory>
#include <cstring>
#include <string>

#include "metadata.h"


#ifdef _WIN32
  #include <windows.h>
#else
  #include <sys/mman.h>
  #include <sys/stat.h> /* For mode constants */
  #include <fcntl.h>    /* For O_* constants */
  #include <unistd.h>
#endif


using namespace Rcpp;
using namespace std;

/**
 * A class encapsulating a memory page with its metadata.
 */
// [[Rcpp::depends(Rcpp)]]
class SharedData {
protected:
    std::unique_ptr<MemoryPage> mem, meta;
    std::string metaname;

public:
    /**
     * Allocates a new memory page for a given object.
     * 
     * @param shared_mem_name     Unique identifier for the memory page holding the actual data
     * @param shared_meta_name    Unique identifier for the memory page holding the metadata information
     * @param obj                 The object that gets copied into the memory page.
     */
    void alloc(const std::string& shared_mem_name, const std::string& shared_meta_name, SEXP obj);

    /**
     * Retrieves an already existing memory page for a given identifier set.
     * 
     * @param shared_mem_name     Unique identifier for the memory page holding the actual data
     * @param shared_meta_name    Unique identifier for the memory page holding the metadata information
     */
    void view(const std::string& shared_mem_name, const std::string& shared_meta_name);

    /**
     * Disposes of this particular instance. In effect this simply deletes mem and meta; see their destructors
     * for further information.
     */
    void dispose();

    /**
     * Accessor for the memory chunk pointed to via mem; gets returned as a raw double*.
     * Ownership stays within this classes responsibility.
     */
    double* memPtr();

    /**
     * Accessor for the memory chunk pointed to via meta. Gets wrapped into a metadata*.
     * Ownership stays within this classes responsibility.
     */
    metadata* metaPtr();

    /**
     * Empty destructor.
     */
    ~SharedData() {}
};


/**
 * Data structures for holding the currently open pages and views for this instance of the memshare dll/so.
 */
extern std::map<std::string, std::unique_ptr<SharedData>> pages;
extern std::map<std::string, std::shared_ptr<SharedData>> views;

/**
 * Retrieve a data wrapper via viewing it (i.e. it already exists as shared memory).
 * 
 * @param shm_mem_name      The unique identifier of the actual data page.
 * @param shm_meta_name     The unique identifier of its metadata page.
 * 
 * @result  A shared_ptr pointing to a new instance of SharedData which manages the memory state internally.
 *          This can be used to construct an ALTREP pointer to the data retrieved.
 */
std::shared_ptr<SharedData> viewPage(std::string shm_mem_name, std::string shm_meta_name);

/**
 * Register a new memory page for a given object. The page is added to pages.
 * 
 * @param name          The unique identifier of the actual data page.
 * @param metaname      The unique identifier of its metadata page.
 * @param obj           The object to register (double matrix, double vector or a list of these).
 */
void registerPage(std::string name, std::string metaname, SEXP obj);

/**
 * Release a memory page from ownership of this component.
 * The memory might stay allocated if there is some worker still holding a view of it (which is the same as a handle).
 * This is not undefined behavior as the memory just gets cleaned up whenever the last view of it is destroyed, however
 * it should never happen that there is shared memory present that has no clear owner in the sense that it is a page
 * for some process! This shows that the library is not used properly.
 * 
 * @param name          The unique identifier of the data page.
 */
void releasePage(std::string name);

/**
 * Release a memory page from viewership of this component.
 * This should always happen *before* the memory is released from ownership of its owner process.
 * 
 * @param name          The unique identifier of the data page.
 */
void releaseView(std::string name);

/**
 * Get the vector of data page names in the viewership of the current process.
 * 
 * @result  A vector containing the memory page names
 */
std::vector<std::string> getSharedViews();

/**
 * Get the vector of data page names in the ownership of the current process.
 * 
 * @result  A vector containing the memory page names
 */
std::vector<std::string> getSharedPages();
