#pragma once

#include <cstddef> // size_t
#include <stdexcept>
#include <string>
#include <cstdint>   // uint64_t, MCT correction in 1.0.3

#ifdef _WIN32
//MCT correction in 1.0.3
// Avoid macro collisions with std::min/std::max
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#else
#include <sys/mman.h>
#include <sys/stat.h> /* For mode constants */
#include <fcntl.h>    /* For O_* constants */
#include <unistd.h>
#endif

/**
 * A Memory page is the raw memory section in RAM which is shared among different processes.
 * 
 * It can model ownership or viewership.
 */
class MemoryPage {
public:
  /**
   * Allocates a shared memory section and retrieves a handle for it.
   * 
   * @param name        The name of the section
   * @param byteSize    The size in bytes of the section
   */
  void alloc(const std::string& name, size_t byteSize);
  /**
   * Retrieves viewership (a handle) of a shared memory section by name.
   * 
   * @param name        The name of the section
   * @param byteSize    The size in bytes of the section
   */
  void view(const std::string& name, size_t byteSize);

  /**
   * Gives the handle back to the OS in order for it to track if there still are open handles.
   * Otherwise GC will simply delete the memory.
   */
  ~MemoryPage();

  /**
   * A getter for the actual raw data pointer.
   * 
   * It returns a double* but for metadata memory this can safely be static_cast into a metadata*.
   */
  double* data();
  
  /**
   * Getter for the name of the memory page.
   */
  std::string get_name() const;
  /**
   * Getter for the byteSize of the memory page.
   */
  size_t size() const;

private:
  std::string name_;
  size_t size_ = 0; // size in bytes, MCT correction
  void* ptr_ = nullptr;
  bool is_view = false;

#ifdef _WIN32
  HANDLE hMapFile_ = nullptr; //MCT correction in 1.0.3
#else
  int fd_ = -1;
#endif
};
