#pragma once

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

/**
 * Injection pattern; define different possible metadata's and inject them into the metadata struct as a union.
 */
// Matrices only have their dimensions
struct MatrixData { std::size_t nrow, ncol; };
// Vectors only know their length
struct VectorData { std::size_t n; };
// Lists know their length in terms of elements (matrices/vectors) and the total memory size (i.e. the number of doubles of all their elements put together)
struct ListData { std::size_t n, numDoubles; };

/**
 * The metadata struct encapsulates information about an object.
 * 
 * 
 * The type is either MATRIX, VECTOR or LIST.
 * 
 * matrix_data, vector_data, list_data contains the metadata of the respective type.
 */
struct metadata {
    enum type {
        MATRIX,
        VECTOR,
        LIST
    } data_type;

    union {
        MatrixData matrix_data;
        VectorData vector_data;
        ListData list_data;
    };
};

/**
 * Get new metadata structs from the relevant data for each different type.
 */
metadata make_matrix_metadata(std::size_t nrow, std::size_t ncol);
metadata make_vector_metadata(std::size_t n);
std::vector<metadata> make_list_metadata(List l);
