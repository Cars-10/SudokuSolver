use std::env;
use std::ffi::OsStr;
use std::path::Path;

fn get_extension_from_filename(filename: &str) -> Option<&str> {
    Path::new(filename)
        .extension()
        .and_then(OsStr::to_str)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);


    for arg in args.iter() {
        assert_eq!(get_extension_from_filename(arg), Some("matrix"));
        let file_path = std::path::Path::new(arg);
        let content_type = file_path.extension().and_then(OsStr::to_str).and_then(|ext| {
        match ext {
            "matrix" => println!("Matrix File {}", arg),
            _ => println!("Nutin but {}", arg),
        }
       });
    }
}

    /* struct timeval stop, start;
    gettimeofday(&start, NULL);

    // For each .matrix file supplied on the commandline run the solver
    int i;
    char * point;
    for ( i = 0; i < argc; i++)
    {
        if((point = strrchr(argv[i],'.')) != NULL ) {
            //printf("Main: i=%i: %s\n", i, argv[i]);
            //Check if supplied filename ends with csv
            if(strcmp(point,".matrix") == 0) {
                printf("%s\n", argv[i]);
                readMatrixFile(argv[i]);
                printPuzzle(); 
                count = 0;
                solve();
            }
        }
    }
    gettimeofday(&stop, NULL);
    printf("Seconds to process %f Seconds\n", (stop.tv_sec - start.tv_sec) + (stop.tv_usec - start.tv_usec)/1000000.0); 
    return 0; */
