// compile_commonfunc.stan

functions {
    // Include from the stanc --include-paths:
    #include commonfunc.stan
}

transformed data {
    print("--- All tests successful.");
}
