# Chisel Image Processor


## Things to do
1. Find a strategy for edge pixels.
    - Since edge pixels are missing some neighbor pixels, kernel becomes off the image.
    - Currently the processor doesn't apply filters on edge pixels since we haven't decided how to handle them.
    - We can treat those missing pixels as empty, duplicate pixels from the nearest edge, or wrap pixels from the other side.
1. Parameterize the pipeline so that the processor can take a variable number of pixels every cycle.
1. After parameterizing the pipeline, parallelize filter application so that the pipeline won't have to stall.
1. Add more filters to match their equivalent from the library.


## Completed
1. Add support for non-convolutional (no kernel) image filters.
    - We can create two FSMs for two types of image processors: (`BasicImageProcessor` and `KernelImageProcessor`)
    - These two types of image processors would inherit from a parent class `ImageProcessor` that also has common logic.
    - The user interface would be unaware of this difference and use a generator, or instantiate the `ImageProcessor` class only.
    - The logic of the `BasicImageProcessor` would be simpler and it can immediately output the new pixel (no need for row buffers).
1. Check if output pixels match the library in unit tests.
1. Add FSM for a filter that uses a kernel.
1. Implement simple image processor logic without any filtering (output the same image).
1. Add `ImageProcessorModel` to model the behavior of hardware using a library.
    - Also add unit tests for this model.
    - The model should read/write image files and prepare input pixels.
1. Add a simple image for unit tests.


## How to run
1. Run sbt test.
1. The tester will take `sample.png` from the image directory and create output of each filter in temp directory from the
    library and the processor.


## Assumptions
1. We're using the Scrimage library for unit tests. Because of floating-point precision issues, rounding pixel values can
    cause incorrect calculations. Therefore, we're testing pixel values with a tolerance of being off by 1.
