#pragma once

#include <Rcpp.h>

using namespace Rcpp;

/**
 * Retrieves an ALTREP representation of previously shared memory.
 * 
 * @param name_space        A string identifying the memory space we are working in.
 * @param vars              A character vector (R-equivalent of std::vector<std::string>) containing the variable names inside the memory space that should be retrieved to R.
 * 
 * @result  An R list of ALTREP representations of the shared objects (double matrices, double vectors, or lists of these).
 */
List retrieveViews(std::string name_space, CharacterVector vars);

/**
 * Retrieves a type-specific, named list containing the metadata for an object.
 * 
 * @param name_space        A string identifying the memory space we are working in.
 * @param varname           A string identifying the variable name inside the memory space.
 * 
 * @result  A type-specific list containing the metadata attributes of the object, i.e. one of
 *              {type: "matrix", nrow: n, ncol: m}
 *              {type: "vector", n: n}
 *              {type: "list", n: n}
 * 
 * @note    This retrieves a view that has to be manually released from within R afterwards!
 */
List retrieveMetadata(std::string name_space, std::string varname);

/**
 * Releases variables from viewership of the current process.
 * 
 * @param name_space        A string identifying the memory space we are working in.
 * @param vars              A character vector identifying the variables that should be released from the given memory space.
 */
void releaseViews(std::string name_space, CharacterVector vars);

/**
 * Retrieves a list of the variables currently held in viewership of the current process.
 */
List viewList();









/**
 * Wrapper function for retrieveViews above. It retrieves an ALTREP representation of previously shared memory.
 * 
 * @param name_spaceSEXP        A character (R-string) identifying the memory space we are working in.
 * @param varsSEXP              A character vector (R-equivalent of std::vector<std::string>) containing the variable names inside the memory space that should be retrieved to R.
 * 
 * @result  An R list of ALTREP representations of the shared objects (double matrices, double vectors, or lists of these).
 */
extern "C" SEXP C_retrieveViews(SEXP name_spaceSEXP, SEXP varsSEXP);

/**
 * Wrapper function for retrieveMetadata above. It retrieves a type-specific, named list containing the metadata for an object.
 * 
 * @param name_spaceSEXP        A character (R-string) identifying the memory space we are working in.
 * @param varnameSEXP           A character (R-string) identifying the variable name inside the memory space.
 * 
 * @result  A type-specific list containing the metadata attributes of the object, i.e. one of
 *              {type: "matrix", nrow: n, ncol: m}
 *              {type: "vector", n: n}
 *              {type: "list", n: n}
 * 
 * @note    This retrieves a view that has to be manually released from within R afterwards!
 */
extern "C" SEXP C_retrieveMetadata(SEXP name_spaceSEXP, SEXP varnameSEXP);
/**
 * Wrapper function for releaseViews above. It releases variables from viewership of the current process.
 * 
 * @param name_spaceSEXP        A character (R-string) identifying the memory space we are working in.
 * @param varsSEXP              A character vector identifying the variables that should be released from the given memory space.
 * 
 * @result NULL (no other way when manually registering Rcpp functions)
 */
extern "C" SEXP C_releaseViews(SEXP name_spaceSEXP, SEXP varsSEXP);

/**
 * Wrapper function for viewList above. It retrieves a list of the variables currently held in viewership of the current process.
 */
extern "C" SEXP C_viewList();