(module
    ;; Import print_string
    (import  "env"  "print_string" (func  $print (param  i32  i32)))
    
    ;; Let's export our own memory to the host
    (memory (export "memory") 1) ;; Memory section of our module
    ;; memory is organized in pages, each page is 64KB
    
    ;; Data section of our module
    (data (i32.const 0) "Hello World from WebAssembly!")

    ;; Function declaration: Exported as helloWorld(), no arguments
    (func (export  "hello")
        i32.const 0  ;; pass offset 0 to print_string
        i32.const 29  ;; pass length 29 to print_string (strlen of sample text)
        call  $print
        )
)




