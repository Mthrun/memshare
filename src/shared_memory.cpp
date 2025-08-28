#include "shared_memory.h"
#include <iostream>

std::map<std::string, std::shared_ptr<SharedData>> views;
std::map<std::string, std::unique_ptr<SharedData>> pages;

void SharedData::alloc(const std::string& shared_mem_name, const std::string& shared_meta_name, SEXP obj) {
    try {
        // differentiate by the type of the object and conditionally on it initialize a metadata object, a memory page of the appropriate size and fill the memory.
        if (Rf_isMatrix(obj) && TYPEOF(obj) == REALSXP) {
            NumericMatrix mat(obj);
            // make the metadata
            metadata m = make_matrix_metadata(mat.nrow(), mat.ncol());
            meta = std::make_unique<MemoryPage>();
            meta->alloc(shared_meta_name, sizeof(metadata));
            // make the memory page
            mem = std::make_unique<MemoryPage>();
            mem->alloc(shared_mem_name, m.matrix_data.nrow * m.matrix_data.ncol * sizeof(double));

            // fill the data
            std::memcpy(meta->data(), &m, sizeof(metadata));
            std::memcpy(mem->data(), mat.begin(), m.matrix_data.nrow * m.matrix_data.ncol * sizeof(double));
        } else if (Rf_isVector(obj) && TYPEOF(obj) == REALSXP) {
            NumericVector vec(obj);
            // make the metadata
            metadata m = make_vector_metadata(vec.size());
            meta = std::make_unique<MemoryPage>();
            meta->alloc(shared_meta_name, sizeof(metadata));
            // make the memory page
            mem = std::make_unique<MemoryPage>();
            mem->alloc(shared_mem_name, m.vector_data.n * sizeof(double));

            // fill the data
            std::memcpy(meta->data(), &m, sizeof(metadata));
            std::memcpy(mem->data(), vec.begin(), m.vector_data.n * sizeof(double));
        } else if (Rf_isNewList(obj)) {
            List l(obj);
            // make the metadata
            std::vector<metadata> m = make_list_metadata(l);
            size_t total_elements = 0;
            for (size_t i = 1; i < m.size(); i++) {
                if (m[i].data_type == metadata::type::MATRIX) {
                    total_elements += m[i].matrix_data.nrow * m[i].matrix_data.ncol;
                } else if (m[i].data_type == metadata::type::VECTOR) {
                    total_elements += m[i].vector_data.n;
                } else if (m[i].data_type == metadata::type::LIST) {
                    stop("Nested Lists are not supported yet!");
                } else {
                    stop("Unknown element type in List!");
                }
            }

            m[0].list_data.numDoubles = total_elements;

            meta = std::make_unique<MemoryPage>();
            meta->alloc(shared_meta_name, sizeof(metadata) * m.size());
            // make the memory page
            mem = std::make_unique<MemoryPage>();
            mem->alloc(shared_mem_name, l.size() * sizeof(unsigned long long) + total_elements * sizeof(double));
            
            // fill it
            std::memcpy(meta->data(), m.data(), m.size() * sizeof(metadata));

            // reserve first m.size() - 1 many pointer-sized entries for the locations of the data in the memory chunk.
            unsigned long long curr = 0;
            double* start = static_cast<double*>(static_cast<void*>(static_cast<unsigned long long*>(static_cast<void*>(mem->data())) + l.size()));
            for (int i = 0; i < l.size(); i++) {
                *(static_cast<unsigned long long*>(static_cast<void*>(mem->data())) + i) = curr;
                if (m[i+1].data_type == metadata::type::MATRIX) {
                    NumericMatrix mat = as<NumericMatrix>(l[i]);
                    unsigned long long size = m[i+1].matrix_data.nrow * (unsigned long long) m[i+1].matrix_data.ncol;
                    std::memcpy(start + curr, mat.begin(), size * sizeof(double));
                    curr += size;
                } else if (m[i+1].data_type == metadata::type::VECTOR) {
                    NumericVector vec = as<NumericVector>(l[i]);
                    unsigned long long size = m[i+1].vector_data.n;
                    std::memcpy(start + curr, vec.begin(), size * sizeof(double));
                    curr += size;
                }
            }
        } else {
            stop("Unsupported shared memory type.");
        }
    } catch (std::exception &e) {
        throw std::runtime_error("Allocation error: " + std::string(e.what()));
    }
}

void SharedData::view(const std::string& shared_mem_name, const std::string& shared_meta_name) {
    try {
        meta = std::make_unique<MemoryPage>();
        meta->view(shared_meta_name, sizeof(metadata));

        // retrieve the metadata of the object
        metadata* m = static_cast<metadata*>(static_cast<void*>(meta->data()));
        metadata::type data_type = m->data_type;

        // retrieve the memory page according to the metadata object
        if (data_type == metadata::type::MATRIX) {
            size_t nrow = m->matrix_data.nrow;
            size_t ncol = m->matrix_data.ncol;
            mem = std::make_unique<MemoryPage>();
            mem->view(shared_mem_name, nrow*ncol*sizeof(double));
        } else if (data_type == metadata::type::VECTOR) {
            size_t n = m->vector_data.n;
            mem = std::make_unique<MemoryPage>();
            mem->view(shared_mem_name, n * sizeof(double));
        } else if (data_type == metadata::type::LIST) {
            size_t n = m->list_data.n;
            meta = std::make_unique<MemoryPage>();
            meta->view(shared_meta_name, (n+1) * sizeof(metadata));
            m = static_cast<metadata*>(static_cast<void*>(meta->data()));

            mem = std::make_unique<MemoryPage>();
            mem->view(shared_mem_name, m[0].list_data.n * sizeof(unsigned long long) + m[0].list_data.numDoubles * sizeof(double));
        } else {
            stop("Unknown type '%s' for variable '%s'", data_type, shared_mem_name);
        }
    } catch (std::runtime_error& e) {
        stop("The requested variable was not registered!");
    }
}

double* SharedData::memPtr() {
    return mem->data();
}

metadata* SharedData::metaPtr() {
    return static_cast<metadata*>(static_cast<void*>(meta->data()));
}

void SharedData::dispose() {
    mem.reset();
    meta.reset();
}


std::shared_ptr<SharedData> viewPage(std::string name, std::string metaname) {
    auto it = views.find(name);
    if (it != views.end()) {
        return it->second;
    }
    auto ptr = std::make_shared<SharedData>();
    ptr->view(name, metaname);
    views.insert({name, ptr});
    return ptr;
}

void registerPage(std::string name, std::string metaname, SEXP obj) {
    auto ptr = std::make_unique<SharedData>();
    ptr->alloc(name, metaname, obj);
    pages.insert({name, std::move(ptr)});
}

void releasePage(std::string name) {
    auto it = pages.find(name);
    if (it == pages.end()) {
      stop("Tried to release variable " + name + " which was not previously allocated in this compilation unit!");
    }
    if (it->second) it->second->dispose();
    pages.erase(it);
}

void releaseView(std::string name) {
    auto it = views.find(name);
    if (it == views.end()) {
      stop("Tried to release variable " + name + " which was not previously allocated in this compilation unit!");
    }
    if (it->second) it->second->dispose();
    views.erase(it);
}
std::vector<std::string> getSharedViews() {
    std::vector<std::string> viewNames;
    for(auto const& key: views)
        viewNames.push_back(key.first);
    return viewNames;
}
std::vector<std::string> getSharedPages() {
    std::vector<std::string> pageNames;
    for(auto const& key: pages)
        pageNames.push_back(key.first);
    return pageNames;
}
