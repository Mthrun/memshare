#include "metadata.h"

metadata make_matrix_metadata(std::size_t nrow, std::size_t ncol) {
    // matrix metadata has type MATRIX and sets nrow, ncol.
    metadata m{};
    m.data_type = metadata::MATRIX;
    m.matrix_data.nrow = nrow;
    m.matrix_data.ncol = ncol;
    return m;
}

metadata make_vector_metadata(std::size_t n) {
    // vector metadata has type VECTOR and sets n.
    metadata m{};
    m.data_type = metadata::VECTOR;
    m.vector_data.n = n;
    return m;
}

std::vector<metadata> make_list_metadata(List l) {
    // list metadata has type LIST and sets n (the list size). Also it makes its own metadata for every element of the list.
    std::vector<metadata> res(l.size() + 1, metadata{});
    res[0].data_type = metadata::LIST;
    res[0].list_data.n = l.size();

    for (int i = 0; i < l.size(); i++) {
        SEXP obj = l[i];
        if (Rf_isMatrix(obj) && TYPEOF(obj) == REALSXP) {
            NumericMatrix mat(obj);
            res[i+1] = make_matrix_metadata(mat.nrow(), mat.ncol());
        } else if (Rf_isVector(obj) && TYPEOF(obj) == REALSXP) {
            NumericVector vec(obj);
            res[i+1] = make_vector_metadata(vec.size());
        } else {
            Rf_error("Unknown element type of list!");
        }
    }
    return res;
}
