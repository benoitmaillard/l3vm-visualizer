# L3VM visualizer

## Run the program
Once the repository is cloned, the code can be compiled by entering the following command
in sbt:
```bash
fastOptJS
```

This should create a `main.js` file in a subdirectory of the `target` directory.

The resources should be placed under a `resources` repository. It should look like this:
```
.
├── resources
│   ├── index.bin
│   ├── phases.bin
│   └── trace.bin
...
```

After this, the easiest way to proceed is to launch a basic HTTP server such as [http-server](https://www.npmjs.com/package/http-server) from the current directory:
```
http-server
```

The page should now be available at [http://127.0.0.1:8080/](http://127.0.0.1:8080/).

## Code organization
The code is made of the following source files
* `Animation.scala`: tools executing an action at a regular interval
* `CanvasPainter.scala`: drawing on a canvas with several layers
* `GridManager.scala`: displaying a program trace on a discrete grid
* `GridUtils.scala`: helper methods for computations with square on a discrete grids and colors
* `Http.scala`: static methods to make HTTP requests with futures
* `Main.scala`: main program
* `ProgramTrace.scala`: representation of a program trace located on a remote server
* `RemoteFile.scala`: abstractions for dealing with remote files
* `SpatialMemoryRepresentation.scala`: mapping from addresses to a position on a grid

All these files contain extensive Scaladoc comments.