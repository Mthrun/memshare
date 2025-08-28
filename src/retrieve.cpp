
#include "retrieve.h"
#include <iostream>

#include "altrep.h"
#include "shared_memory.h"
#include "metadata.h"

List retrieveViews(std::string name_space, CharacterVector vars) {
#ifdef _WIN32
    // For windows we prepend the namespace identifier by "Local\\" because otherwise the shared memory is shared system-wide (instead of user-wide) which needs admin privileges
    name_space = "Local\\" + name_space;   
#endif
    if (vars.size() == 0) {
        return List::create();
    }

    List result(vars.size());

    for (int i = 0; i < vars.size(); ++i) {
        std::string varname = Rcpp::as<std::string>(vars[i]);

        // open a viewership page to the variable
        auto view = viewPage(name_space + "." + varname, name_space + ".metadata." + varname);
        metadata::type data_type = view->metaPtr()->data_type;

        // wrap the page into an ALTREP
        if (data_type == metadata::type::MATRIX) {
            result[i] = make_altrep_matrix(view->memPtr(), view->metaPtr()->matrix_data.nrow, view->metaPtr()->matrix_data.ncol);
        } else if (data_type == metadata::type::VECTOR) {
            result[i] = make_altrep_vector(view->memPtr(), view->metaPtr()->vector_data.n);
        } else if (data_type == metadata::type::LIST) {
            result[i] = make_altrep_list(view->metaPtr(), view->memPtr());
        } else {
            stop("Unknown Datatype!");
        }
    }

    result.attr("names") = vars;
    return result;
}

List retrieveMetadata(std::string name_space, std::string varname) {
#ifdef _WIN32
    // For windows we prepend the namespace identifier by "Local\\" because otherwise the shared memory is shared system-wide (instead of user-wide) which needs admin privileges
    name_space = "Local\\" + name_space;   
#endif
    // retrieve a viewership page of the variable
    auto view = viewPage(name_space + "." + varname, name_space + ".metadata." + varname);
    metadata::type data_type = view->metaPtr()->data_type;

    // wrap the metadata of the viewership page into a list.
    if (data_type == metadata::type::MATRIX) {
        return List::create(
            Named("type") = "matrix",
            Named("nrow") = view->metaPtr()->matrix_data.nrow,
            Named("ncol") = view->metaPtr()->matrix_data.ncol
        );
    } else if (data_type == metadata::type::VECTOR) {
        return List::create(
            Named("type") = "vector",
            Named("n") = view->metaPtr()->vector_data.n
        );
    } else if (data_type == metadata::type::LIST) {
        return List::create(
            Named("type") = "list",
            Named("n") = view->metaPtr()->list_data.n
        );
    } else {
        stop("Unknown type '%s' for variable '%s'", data_type, varname);
    }
}
void releaseViews(std::string name_space, CharacterVector vars) {
#ifdef _WIN32
    // For windows we prepend the namespace identifier by "Local\\" because otherwise the shared memory is shared system-wide (instead of user-wide) which needs admin privileges
    name_space = "Local\\" + name_space;   
#endif
    for (long int i = 0; i < vars.size(); ++i) {
        std::string varname = Rcpp::as<std::string>(vars[i]);

        releaseView(name_space + "." + varname);
    }
}
List viewList() {
    std::vector<std::string> viewNames = getSharedViews();
    List result(viewNames.size());
    for (std::size_t i = 0; i < viewNames.size(); i++)
        result[i] = viewNames[i];
    return result;
}

extern "C" SEXP C_retrieveViews(SEXP name_spaceSEXP, SEXP varsSEXP) {
    try {
        std::string name_space = as<std::string>(name_spaceSEXP);
        CharacterVector vars = as<CharacterVector>(varsSEXP);

        List result = retrieveViews(name_space, vars);
        return result;
    } catch (std::exception &e) {
        Rf_error("retrieveViews error: %s", e.what());
    } catch (...) {
        Rf_error("retrieveViews unknown error");
    }
}
extern "C" SEXP C_retrieveMetadata(SEXP name_spaceSEXP, SEXP varnameSEXP) {
    try {
        std::string name_space = as<std::string>(name_spaceSEXP);
        std::string varname = as<std::string>(varnameSEXP);
        
        List res = retrieveMetadata(name_space, varname);
        return res;
    } catch (std::exception &e) {
        Rf_error("retrieveMetadata error: %s", e.what());
    } catch (...) {
        Rf_error("retrieveMetadata unknown error");
    }
}
extern "C" SEXP C_releaseViews(SEXP name_spaceSEXP, SEXP varsSEXP) {
    try {
        std::string name_space = as<std::string>(name_spaceSEXP);
        CharacterVector vars = as<CharacterVector>(varsSEXP);

        releaseViews(name_space, vars);

        return R_NilValue; // function returns void
    } catch (std::exception &e) {
        Rf_error("retrieveMetadata error: %s", e.what());
    } catch (...) {
        Rf_error("retrieveMetadata unknown error");
    }
}
extern "C" SEXP C_viewList() {
    return viewList();
}
