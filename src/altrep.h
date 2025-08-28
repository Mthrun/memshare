// SOURCE: https://github.com/romainfrancois/altrepisode/blob/master/src/altrepisode.h

// to manipulate R objects, aka SEXP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

#undef length
#undef isNull

// because we need to initialize the altrep class
#include <R_ext/Rdynload.h>

#if R_VERSION < R_Version(3, 6, 0)

// workaround because R's <R_ext/Altrep.h> not so conveniently uses `class`
// as a variable name, and C++ is not happy about that
//
// SEXP R_new_altrep(R_altrep_class_t class, SEXP data1, SEXP data2);
//
#define class klass

// Because functions declared in <R_ext/Altrep.h> have C linkage
extern "C" {
  #include <R_ext/Altrep.h>
}

// undo the workaround
#undef class

#else
  #include <R_ext/Altrep.h>
#endif


#include "metadata.h"

// Declaration of the ALTREP classes for each of the allowed types.
extern R_altrep_class_t altrep_matrix_class;
extern R_altrep_class_t altrep_vector_class;
extern R_altrep_class_t altrep_list_class;


extern "C" {
    /**
     * Initializers for the different ALTREP handles of the raw memory types.
     */

    /**
     * Get ALTREP wrapper of matrix data.
     * 
     * @param ptr     Pointer to the actual data section.
     * @param nrow    Number of rows of the matrix.
     * @param ncol    Number of cols of the matrix.
     * 
     * @return ALTREP that looks and behaves exactly like a matrix to R but actually uses the C memory from the shared page.
     */
    SEXP make_altrep_matrix(double* ptr, size_t nrow, size_t ncol);
    /**
     * Get ALTREP wrapper of vector data.
     * 
     * @param ptr     Pointer to the actual data section.
     * @param len     Number of elements of the vector.
     * 
     * @return ALTREP that looks and behaves exactly like a vector to R but actually uses the C memory from the shared page.
     */
    SEXP make_altrep_vector(double* ptr, size_t len);
    /**
     * Get ALTREP wrapper of a list.
     * 
     * @param metadatas       A contiguous list of metadatas (first is the list metadata and the (i+1)-st is the metadata of element i).
     * @param data            The contiguous data block of this list (memory of all elements in one contiguous block)
     * 
     * @return ALTREP that looks and behaves exactly like a list to R but actually uses the C memory from the shared page.
     */
    SEXP make_altrep_list(metadata* metadatas, double* data);


    /**
     * BEHAVIOR FUNCTIONS OF THE ALTREPS
     */
    /**
     * What happens if the R-side inspects the matrix data
     * 
     * @param x           The ALTREP matrix object.
     * @param min, max    Integer indices specifying the range of elements (columns) to inspect.
     * @param showData    A flag (nonzero/zero) indicating whether to actually display or traverse the data values or only provide structural information
     * @param callBack    A function pointer that gets invoked during inspection for each inspected element (column) of the matrix.
     * 
     * @result Whether the inspection was successful.
     */
    Rboolean altrep_matrix_inspect(SEXP x, int min, int max, int showData, void (*callBack)(SEXP, int, int, int));
    /**
     * Returns the total length (number of elements) of an ALTREP matrix object.
     * 
     * @param x     The ALTREP matrix object
     * 
     * @result  nrow * ncol
     */
    R_xlen_t altrep_matrix_length(SEXP x);
    /**
     * Returns the data pointer to the raw memory of an ALTREP matrix object.
     * 
     * @param x           The ALTREP matrix object
     * @param writeable   Whether the memory is const or not.
     * 
     * @result  The raw memory section as a void*.
     */
    void* altrep_matrix_dataptr(SEXP x, Rboolean writeable);
    /**
     * Returns the data pointer to the raw memory of an ALTREP matrix object.
     * 
     * @param x           The ALTREP matrix object
     * 
     * @result  The constant raw memory chunk of the matrix ALTREP.
     */
    const void* altrep_matrix_dataptr_or_null(SEXP x);
    /**
     * Returns the i-th element of the ALTREP matrix object (as container).
     * 
     * @param x           The ALTREP matrix object
     * @param i           Index of the element to retrieve.
     * 
     * @result  The i-th element of the matrix as a double.
     */
    double altrep_matrix_real_elt(SEXP x, R_xlen_t i);

    // cf. altrep_matrix functions
    Rboolean altrep_vector_inspect(SEXP x, int min, int max, int showData, void (*callBack)(SEXP, int, int, int));
    R_xlen_t altrep_vector_length(SEXP x);
    void* altrep_vector_dataptr(SEXP x, Rboolean writeable);
    const void* altrep_vector_dataptr_or_null(SEXP x);
    double altrep_vector_real_elt(SEXP x, R_xlen_t i);


    /**
     * What happens if the R-side inspects the list data
     * 
     * @param x           The ALTREP list object.
     * @param min, max    Integer indices specifying the range of elements to inspect.
     * @param showData    A flag (nonzero/zero) indicating whether to actually display or traverse the data values or only provide structural information
     * @param callBack    A function pointer that gets invoked during inspection for each inspected element of the list.
     * 
     * @result Whether the inspection was successful.
     */
    Rboolean altrep_list_inspect(SEXP x, int min, int max, int showData, void (*callBack)(SEXP, int, int, int));
    /**
     * Returns the i-th element of the ALTREP list object (as container).
     * This element is itself an ALTREP element.
     * 
     * @param x           The ALTREP list object
     * @param i           Index of the element to retrieve.
     * 
     * @result  The i-th element of the list as an ALTREP container itself.
     */
    SEXP altrep_list_elt(SEXP x, R_xlen_t i);
    /**
     * Returns the total length (number of elements) of an ALTREP list object.
     * 
     * @param x     The ALTREP list object
     * 
     * @result      The number of ALTREP objects this list stores.
     */
    R_xlen_t altrep_list_length(SEXP x);
}
