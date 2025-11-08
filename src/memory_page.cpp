#include "memory_page.h"


void MemoryPage::alloc(const std::string& name, size_t byteSize) {
    // set name, size and viewership/ownership as is.
    name_ = name;
    size_ = byteSize;
    is_view = false;
#ifdef _WIN32
    // for windows create a file mapping and retrieve a handle to it.
//MCT correction in 1.0.3
  ULONGLONG maxSize = static_cast<ULONGLONG>(size_);
  DWORD maxSizeLow  = static_cast<DWORD>(maxSize & 0xFFFFFFFFull);
  DWORD maxSizeHigh = static_cast<DWORD>((maxSize >> 32) & 0xFFFFFFFFull);
  hMapFile_ = CreateFileMappingA(
    INVALID_HANDLE_VALUE,    // use paging file
    NULL,                    // default security
    PAGE_READWRITE,          // read/write access
    maxSizeHigh,             // maximum object size (high-order DWORD)
    maxSizeLow,              // maximum object size (low-order DWORD)
    name.c_str());           // name of mapping object
  //end MCT correction in 1.0.3
    if (GetLastError() == ERROR_ALREADY_EXISTS) {
        throw std::runtime_error("Variable was already registered!");
    }

    if (hMapFile_ == NULL) {
        throw std::runtime_error("Could not create file mapping " + name);
    }

    ptr_ = MapViewOfFile(
        hMapFile_,            // handle to map object
        FILE_MAP_ALL_ACCESS,  // read/write permission
        0,
        0,
        size_);

    if (ptr_ == NULL) {
        CloseHandle(hMapFile_);
        throw std::runtime_error("Could not map view of file.");
    }
    
#else
    #ifdef __APPLE__
        if (name.size() > 32) { // including leading '/'
            throw std::runtime_error("On MacOS shared variable allocations are only allowed for UIDs with a length < 32 characters; " + name + " exceeds this! Choose a shorter namespace and variable name!");
        }
    #endif

    // for ubuntu open a new shm and mmap it.
    fd_ = shm_open(name.c_str(), O_CREAT | O_EXCL | O_RDWR, 0666);
    if (fd_ == -1) {
        throw std::runtime_error("Variable was already registered!");
        //throw std::runtime_error("Failed to open shared memory.");
    }

    if (ftruncate(fd_, size_) == -1) {
        close(fd_);
        throw std::runtime_error("Failed to set size of shared memory.");
    }

    ptr_ = mmap(NULL, size_, PROT_READ | PROT_WRITE, MAP_SHARED, fd_, 0);
    if (ptr_ == MAP_FAILED) {
        close(fd_);
        throw std::runtime_error("Failed to map shared memory.");
    }
#endif
}

void MemoryPage::view(const std::string& name, size_t byteSize) {
    // set name, size and view as is.
    name_ = name;
    size_ = byteSize;
    is_view = true;
#ifdef _WIN32
    // for windows open an already existing file mapping and retrieve a handle to it.
    hMapFile_ = OpenFileMappingA(
        FILE_MAP_READ,
        FALSE,
        name.c_str()
    );

    if (hMapFile_ == NULL) 
        throw std::runtime_error("Could not create file mapping " + name);

    ptr_ = MapViewOfFile(
        hMapFile_,
        FILE_MAP_READ,
        0,
        0,
        byteSize
    );

    if (ptr_ == NULL) {
        CloseHandle(hMapFile_);
        throw std::runtime_error("Could not map view of file.");
    }
#else
    // for ubuntu open an already existing shm and mmap it.
    fd_ = shm_open(name.c_str(), O_RDONLY, 0666);
    if (fd_ == -1)
        throw std::runtime_error("Failed to open shared memory.");

    ptr_ = mmap(0, byteSize, PROT_READ, MAP_SHARED, fd_, 0);
    if (ptr_ == MAP_FAILED)
        throw std::runtime_error("Failed to map shared memory.");
#endif
}

MemoryPage::~MemoryPage() {
#ifdef _WIN32
    // destroy the handle via unmap.
    if (ptr_) {
        UnmapViewOfFile(ptr_);
    }
    if (hMapFile_) {
        CloseHandle(hMapFile_);
    }
#else
    // destroy the handle via munmap.
    if (ptr_) {
        munmap(ptr_, size_);
    }
    if (fd_ != -1) {
        close(fd_);
        if (!is_view) shm_unlink(name_.c_str());
    }
#endif
}

double* MemoryPage::data() {
    return static_cast<double*>(ptr_);
}

std::string MemoryPage::get_name() const {
    return name_;
}
size_t MemoryPage::size() const {
    return size_ / sizeof(double);
}
