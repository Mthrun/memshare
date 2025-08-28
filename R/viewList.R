viewList <- function() {
    # viewList()
    #
    #  Function to obtain a list of the views the current session holds..
    #
    # 
    # INPUT.
    #
    #OUTPUT
    #   An 1:p list of characters of the the p retrieved view
    #author: JM 05/2025

    .Call("C_viewList", PACKAGE = "memshare")
}
