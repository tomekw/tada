complete -c tada -f

complete -c tada -n __fish_use_subcommand -a build -d "Compile the package"
complete -c tada -n "__fish_seen_subcommand_from build" -l profile -rf -a "debug release" -d "Build profile"

complete -c tada -n __fish_use_subcommand -a cache -d "Install package to the local cache, use --force to overwrite"
complete -c tada -n "__fish_seen_subcommand_from cache" -l force -d "Overwrite cache"

complete -c tada -n __fish_use_subcommand -a clean -d "Remove build artifacts"

complete -c tada -n __fish_use_subcommand -a config -d "Display configuration"

complete -c tada -n __fish_use_subcommand -a help -d "Show help"

complete -c tada -n __fish_use_subcommand -a init -d "Create a new package"
complete -c tada -n "__fish_seen_subcommand_from init" -l exe -d "Executable package"
complete -c tada -n "__fish_seen_subcommand_from init" -l lib -d "Library package"

complete -c tada -n __fish_use_subcommand -a install -d "Install dependencies"

complete -c tada -n __fish_use_subcommand -a run -d "Build and run the executable"
complete -c tada -n "__fish_seen_subcommand_from run" -l profile -rf -a "debug release" -d "Build profile"

complete -c tada -n __fish_use_subcommand -a test -d "Build and run the tests"
complete -c tada -n "__fish_seen_subcommand_from test" -l profile -rf -a "debug release" -d "Build profile"

complete -c tada -n __fish_use_subcommand -a version -d "Show version"
