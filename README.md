# build-profile

Help output:

    $ build-profile --help
    build-profile

    Usage: build-profile [--version] [--help] COMMAND
      build-profile

    Available options:
      --version                Show version
      --help                   Show this help text

    Available commands:
      generate                 Generate a database of the profile


Generating a database:

    $ build-profile generate --help
    Usage: build-profile generate --title TEXT [--sqlite-file PATH] PATH
      Generate a database of the profile

    Available options:
      -h,--help                Show this help text
      --title TEXT             Title for this build
      --sqlite-file PATH       Filepath to use for sqlite database
      PATH                     Log file path
