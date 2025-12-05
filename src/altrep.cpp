#include "altrep.h"
#include <iostream>

extern "C" {
    SEXP make_altrep_matrix(double* ptr, size_t nrow, size_t ncol) {
        // Allocate a vector under PROTECT with 3 elements for the metadata
        SEXP info = PROTECT(Rf_allocVector(VECSXP, 3));
        SET_VECTOR_ELT(info, 0, R_MakeExternalPtr(ptr, R_NilValue, R_NilValue)); // data ptr
        SET_VECTOR_ELT(info, 1, Rf_ScalarInteger(nrow)); // nrow
        SET_VECTOR_ELT(info, 2, Rf_ScalarInteger(ncol)); // ncol

        // Now we allocate a new ALTREP matrix object and associate to it the metadata from above used from R-side to determine that this is a matrix.
        SEXP alt_vec = PROTECT(R_new_altrep(altrep_matrix_class, info, R_NilValue));

        // Set dimension attribute (R uses column-major order)
        SEXP dim = PROTECT(Rf_allocVector(INTSXP, 2));
        INTEGER(dim)[0] = nrow;
        INTEGER(dim)[1] = ncol;
        Rf_setAttrib(alt_vec, R_DimSymbol, dim);

        UNPROTECT(3);
        return alt_vec;
    }

    Rboolean altrep_matrix_inspect(SEXP x, int min, int max, int showData, void (*callBack)(SEXP, int, int, int)) {
        Rprintf("Inspecting external double ALTREP\n");

        // Optionally use the callback to display elements, for example:
        if (showData) {
            for (int i = min; i < max; i++) {
                // call callback with element i info; you can customize this as needed
                callBack(x, i, i+1, 1);
            }
        }
        return TRUE;
    }

    R_xlen_t altrep_matrix_length(SEXP x) {
        // retrieve the metadata "info" from the altrep and multiply the metadata ncol and nrow.
        SEXP info = R_altrep_data1(x);
        int nrow = INTEGER(VECTOR_ELT(info, 1))[0];
        int ncol = INTEGER(VECTOR_ELT(info, 2))[0];
        return static_cast<R_xlen_t>(nrow) * ncol;
    }

    void* altrep_matrix_dataptr(SEXP x, Rboolean writeable) {
        // retrieve the data ptr from the metadata
        SEXP info = R_altrep_data1(x);
        double* ptr = (double*) R_ExternalPtrAddr(VECTOR_ELT(info, 0));
        return (void*) ptr;
    }

    const void* altrep_matrix_dataptr_or_null(SEXP x) {
        return (const void*) altrep_matrix_dataptr(x, TRUE);
    }

    double altrep_matrix_real_elt(SEXP x, R_xlen_t i) {
        double* ptr = (double*) altrep_matrix_dataptr(x, TRUE);
        return ptr[i];
    }








    SEXP make_altrep_vector(double* ptr, size_t len) {
        SEXP info = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(info, 0, R_MakeExternalPtr(ptr, R_NilValue, R_NilValue));
        SET_VECTOR_ELT(info, 1, Rf_ScalarInteger(len));

        SEXP alt_vec = PROTECT(R_new_altrep(altrep_vector_class, info, R_NilValue));

        UNPROTECT(2);
        return alt_vec;
    }

    Rboolean altrep_vector_inspect(SEXP x, int min, int max, int showData, void (*callBack)(SEXP, int, int, int)) {
        Rprintf("Inspecting external double ALTREP\n");

        // Optionally use the callback to display elements, for example:
        if (showData) {
            for (int i = min; i < max; i++) {
                // call callback with element i info; you can customize this as needed
                callBack(x, i, i+1, 1);
            }
        }
        return TRUE;
    }

    R_xlen_t altrep_vector_length(SEXP x) {
        SEXP info = R_altrep_data1(x);
        int len = INTEGER(VECTOR_ELT(info, 1))[0];
        return static_cast<R_xlen_t>(len);
    }

    void* altrep_vector_dataptr(SEXP x, Rboolean writeable) {
        SEXP info = R_altrep_data1(x);
        double* ptr = (double*) R_ExternalPtrAddr(VECTOR_ELT(info, 0));
        return (void*) ptr;
    }

    const void* altrep_vector_dataptr_or_null(SEXP x) {
        return (const void*) altrep_vector_dataptr(x, TRUE);
    }

    double altrep_vector_real_elt(SEXP x, R_xlen_t i) {
        double* ptr = (double*) altrep_vector_dataptr(x, TRUE);
        return ptr[i];
    }







    SEXP make_altrep_list(metadata* metadatas, double* data) {
        // list metadata has 2 elements:
        SEXP info = PROTECT(Rf_allocVector(VECSXP, 2));
    
        // Store data_ptrs (cast to void*) and metadata
        SET_VECTOR_ELT(info, 0, R_MakeExternalPtr(data, R_NilValue, R_NilValue)); // data chunk
        SET_VECTOR_ELT(info, 1, R_MakeExternalPtr(metadatas, R_NilValue, R_NilValue)); // the metadata array.

        // Create the ALTREP list object
        SEXP alt_list = PROTECT(R_new_altrep(altrep_list_class, info, R_NilValue));

        UNPROTECT(2);
        return alt_list;
    }

    R_xlen_t altrep_list_length(SEXP x) {
        // dereference the first metadata element of the metadata->metadata (which is the metadata of the list itself) and get the length from there.
        void* meta_ptr = R_ExternalPtrAddr(VECTOR_ELT(R_altrep_data1(x), 1));
        metadata* m = static_cast<metadata*>(meta_ptr);
        return m[0].list_data.n;
    }

    SEXP altrep_list_elt(SEXP x, R_xlen_t i) {
        // retrieve a new ALTREP from an element of the list.
        if (i < 0 || i >= altrep_list_length(x))
            Rf_error("Index out of bounds");

        // get the metadatas
        void* meta_ptr = R_ExternalPtrAddr(VECTOR_ELT(R_altrep_data1(x), 1));
        // get the list-metadata (which is the first metadata in the metadata*)
        metadata* m = static_cast<metadata*>(meta_ptr);
        // get the data chunk
        void* data = R_ExternalPtrAddr(VECTOR_ELT(R_altrep_data1(x), 0));
        double* start = static_cast<double*>(static_cast<void*>(static_cast<unsigned long long*>(data) + m[0].list_data.n));
        unsigned long long* sizes = static_cast<unsigned long long*>(data);

        // retrieve the i-th metadata and initialize a new object of this kind and metadata at the position of the current element in the data chunk.
        metadata::type data_type = m[i+1].data_type;
        if (data_type == metadata::type::MATRIX) {
            return make_altrep_matrix(start + sizes[i], m[i+1].matrix_data.nrow, m[i+1].matrix_data.ncol);
        } else if (data_type == metadata::type::VECTOR) {
            return make_altrep_vector(start + sizes[i], m[i+1].vector_data.n);
        } else if (data_type == metadata::type::LIST) {
            Rf_error("Nested Lists are not supported yet!");
        } else {
            Rf_error("Unknown datatype!");
        }
    }

    Rboolean altrep_list_inspect(SEXP x, int min, int max, int showData, void (*callBack)(SEXP, int, int, int)) {
        Rprintf("Inspecting ALTREP list\n");
        if (showData) {
            for (int i = min; i < max; i++) {
                callBack(x, i, i + 1, 1);
            }
        }
        return TRUE;
    }
}
