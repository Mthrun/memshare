pageList <- function() {
    # pageList()
    #
    # Function to obtain a list of the registered variables of the current sessione.
    #
    # 
    # INPUT
    #
    #OUTPUT
    #
    #  An 1:(m) list of characters of the registered p namespaces, each of them having up to k variables. Each element of the list is a combination of namespace and variable name
    #author: JM 05/2025

    .Call("C_pageList", PACKAGE = "memshare")
}
