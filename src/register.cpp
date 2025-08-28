
#include "register.h"
#include <iostream>

#include "shared_memory.h"
#include "metadata.h"

void registerVariables(std::string name_space, List vars) {
#ifdef _WIN32
    // For windows we prepend the namespace identifier by "Local\\" because otherwise the shared memory is shared system-wide (instead of user-wide) which needs admin privileges
    name_space = "Local\\" + name_space;   
#endif

    for (int i = 0; i < vars.size(); ++i) {
        Rcpp::CharacterVector varnames = vars.names();
        std::string varname = Rcpp::as<std::string>(varnames[i]);

        SEXP obj = vars[i];

        // register a page for every variable in the list.
        registerPage(name_space + "." + varname, name_space + ".metadata." + varname, obj);
    }
}
void releaseVariables(std::string name_space, CharacterVector vars) {
#ifdef _WIN32
    // For windows we prepend the namespace identifier by "Local\\" because otherwise the shared memory is shared system-wide (instead of user-wide) which needs admin privileges
    name_space = "Local\\" + name_space;    
#endif
    for (long int i = 0; i < vars.size(); ++i) {
        std::string varname = Rcpp::as<std::string>(vars[i]);

        // release the page of everyy variable in the list.
        releasePage(name_space + "." + varname);
    }
}

List pageList() {
    std::vector<std::string> pageNames = getSharedPages();
    List result(pageNames.size());
    for (std::size_t i = 0; i < pageNames.size(); i++)
        result[i] = pageNames[i];
    return result;
}

extern "C" SEXP C_registerVariables(SEXP name_spaceSEXP, SEXP varsSEXP) {
    try {
        std::string name_space = as<std::string>(name_spaceSEXP);
        List vars = as<List>(varsSEXP);

        registerVariables(name_space, vars);

        return R_NilValue; // function returns void
    } catch (std::exception &e) {
        Rf_error("registerVariables error: %s", e.what());
    } catch (...) {
        Rf_error("registerVariables unknown error");
    }
}
extern "C" SEXP C_releaseVariables(SEXP name_spaceSEXP, SEXP varsSEXP) {
    try {
        std::string name_space = as<std::string>(name_spaceSEXP);
        CharacterVector vars = as<CharacterVector>(varsSEXP);

        releaseVariables(name_space, vars);

        return R_NilValue; // function returns void
    } catch (std::exception &e) {
        Rf_error("registerVariables error: %s", e.what());
    } catch (...) {
        Rf_error("registerVariables unknown error");
    }
}
extern "C" SEXP C_pageList() {
    return pageList();
}
