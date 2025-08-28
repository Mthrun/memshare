#include "altrep.h"
#include "register.h"
#include "retrieve.h"
#include "c_mutualinfo.h"

// The actual definition of the declared ALTREP classes.
R_altrep_class_t altrep_matrix_class = {0};
R_altrep_class_t altrep_vector_class = {0};
R_altrep_class_t altrep_list_class = {0};

extern "C" {

    /**
     * Here we define the wrappers and callable functions with their number of parameters by hand (instead of using Rcpp::export)
     */
    static const R_CallMethodDef CallEntries[] = {
        {"C_registerVariables", (DL_FUNC) &C_registerVariables, 2},
        {"C_retrieveViews", (DL_FUNC) &C_retrieveViews, 2},
        {"C_releaseVariables", (DL_FUNC) &C_releaseVariables, 2},
        {"C_releaseViews", (DL_FUNC) &C_releaseViews, 2},
        {"C_retrieveMetadata", (DL_FUNC) &C_retrieveMetadata, 2},
        {"C_viewList", (DL_FUNC) &C_viewList, 0},
        {"C_pageList", (DL_FUNC) &C_pageList, 0},
        {"C_mutualinfo", (DL_FUNC) &C_mutualinfo, 2},
        {NULL, NULL, 0}
    };
    
    /**
     * This is the main-function that gets triggered when NAMESPACE loads this DLL as an Rcpp DLL.
     */
    void R_init_memshare(DllInfo* dll) {
        // Register native routines
        R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
        R_useDynamicSymbols(dll, FALSE); // Optional but good practice

        // Register ALTREP class
        altrep_matrix_class = R_make_altreal_class("altrep_matrix", "memshare", dll);

        R_set_altrep_Length_method(altrep_matrix_class, altrep_matrix_length);
        R_set_altrep_Inspect_method(altrep_matrix_class, altrep_matrix_inspect);

        R_set_altreal_Elt_method(altrep_matrix_class, altrep_matrix_real_elt);
        //R_set_altreal_Dataptr_method(altrep_matrix_class, altrep_matrix_dataptr);
        //R_set_altreal_Dataptr_or_null_method(altrep_matrix_class, altrep_matrix_dataptr_or_null);
        /*#if (R_VERSION_MAJOR > 4) || (R_VERSION_MAJOR == 4 && R_VERSION_MINOR >= 3)
            R_set_altreal_Dataptr_method(altrep_matrix_class, altrep_matrix_dataptr);
            R_set_altreal_Dataptr_or_null_method(altrep_matrix_class, altrep_matrix_dataptr_or_null);
        #endif*/
        R_set_altvec_Dataptr_method(altrep_matrix_class, altrep_matrix_dataptr);
        R_set_altvec_Dataptr_or_null_method(altrep_matrix_class, altrep_matrix_dataptr_or_null);



        altrep_vector_class = R_make_altreal_class("altrep_vector", "memshare", dll);

        R_set_altrep_Length_method(altrep_vector_class, altrep_vector_length);
        R_set_altrep_Inspect_method(altrep_vector_class, altrep_vector_inspect);

        R_set_altreal_Elt_method(altrep_vector_class, altrep_vector_real_elt);
        //R_set_altreal_Dataptr_method(altrep_vector_class, altrep_vector_dataptr);
        //R_set_altreal_Dataptr_or_null_method(altrep_vector_class, altrep_vector_dataptr_or_null);
        /*#if (R_VERSION_MAJOR > 4) || (R_VERSION_MAJOR == 4 && R_VERSION_MINOR >= 3)
            R_set_altreal_Dataptr_method(altrep_vector_class, altrep_vector_dataptr);
            R_set_altreal_Dataptr_or_null_method(altrep_vector_class, altrep_vector_dataptr_or_null);
        #endif*/
        R_set_altvec_Dataptr_method(altrep_vector_class, altrep_vector_dataptr);
        R_set_altvec_Dataptr_or_null_method(altrep_vector_class, altrep_vector_dataptr_or_null);





        altrep_list_class = R_make_altlist_class("altrep_list", "memshare", dll);

        R_set_altrep_Length_method(altrep_list_class, altrep_list_length);
        R_set_altrep_Inspect_method(altrep_list_class, altrep_list_inspect);
        R_set_altlist_Elt_method(altrep_list_class, altrep_list_elt);
    }
}
