#pragma once
#include <Rcpp.h>
using namespace Rcpp;

/**
 * Registers a shared memory space for a given list of variables.
 * 
 * @param name_spaceSEXP        A character (R-string) identifying the memory space we are working in.
 * @param varsSEXP              A list of variables to register in the shared memory space.
 */
void registerVariables(std::string name_space, List vars);

/**
 * Releases a list of variables from a shared memory space.
 * 
 * @param name_spaceSEXP        A character (R-string) identifying the memory space we are working in.
 * @param varsSEXP              A character vector identifying the variables to release.
 */
void releaseVariables(std::string name_space, CharacterVector vars);

/**
 * Retrieves a list of variables owned by the current process as a shared memory page.
 * 
 * @result  List stating the variables in current ownership of this process.
 */
List pageList();








/**
 * Wrapper function for registerVariables above. It registers a shared memory space for a given list of variables.
 * 
 * @param name_spaceSEXP        A character (R-string) identifying the memory space we are working in.
 * @param varsSEXP              A list of variables to register in the shared memory space.
 * 
 * @result  NULL (no other way when manually registering Rcpp functions)
 */
extern "C" SEXP C_registerVariables(SEXP name_spaceSEXP, SEXP varsSEXP);

/**
 * Wrapper function for releaseVariables above. It releases a list of variables from a shared memory space.
 * 
 * @param name_spaceSEXP        A character (R-string) identifying the memory space we are working in.
 * @param varsSEXP              A character vector identifying the variables to release.
 *  
 * @result  NULL (no other way when manually registering Rcpp functions)
 */
extern "C" SEXP C_releaseVariables(SEXP name_spaceSEXP, SEXP varsSEXP);

/**
 * Wrapper function for pageList above. It retrieves a list of variables owned by the current process as a shared memory page.
 * 
 * @result  List of Characters stating the variables in current ownership of this process.
 */
extern "C" SEXP C_pageList();