# Chisel Image Processor
This project is a parameterized Chisel generator for image processing hardware. The top-level hardware module can take a number of pixels
every cycle, apply the given filter on the image, and return the processed pixels in a similar streamlined fashion.


## Features
1. Support for two types of image filters:
    - Basic (pixel-level)
    - Convolution (kernel-based)
1. Parallelism
1. Implemented filters:
    - Blur
    - Bump
    - Grayscale
    - Solarize
1. Generic implementation to support new filters


## How to use
In order to instantiate an image processor, you need to use our generator interface.
```scala
val p = ImageProcessorParams(...)                               // Image processor parameters
val filterName = ...                                            // Name of the filter
val imageProcessor = ImageProcessorGenerator.get(p, filterName) // Image processor instance
```

### Custom filter
If you would like to add your own filter, you need to do two things. First, add your filter logic in a class like the following.
```scala
class MyFilter(p: ImageProcessorParams) extends FilterOperator(p, ???, ???) {
    // Your filter logic here (see FilterOperator.scala for implementation examples)
}
```
Second, add an entry to the filter generator interface. Also, make sure that `FilterGenerator.isKernelFilter` returns true
if your filter is a convolution (kernel-based) filter. You can extend the Vector in that function with your filter's name.
```scala
object FilterGenerator {
  ...
  val myfilter = "myfilter"
  ...
  def get(p: ImageProcessorParams, name: String): FilterOperator = {
    ...
    } else if (name == myfilter) {
        return new MyFilter(p)
    }
    ...
  }
}
```


## How to test
1. Run sbt test.
1. The tester will take `sample.png` from the image directory and create output of each filter in temp directory from the
    library and the processor.


## Things to do
1. Add more filters to match their equivalent from the library.


## Completed
1. After parameterizing the pipeline, parallelize filter application so that the pipeline won't have to stall.
1. Parameterize the pipeline so that the processor can take a variable number of pixels every cycle.
1. Find a strategy for edge pixels.
    - Since edge pixels are missing some neighbor pixels, kernel becomes off the image.
    - Currently the processor doesn't apply filters on edge pixels since we haven't decided how to handle them.
    - We can treat those missing pixels as empty, duplicate pixels from the nearest edge, or wrap pixels from the other side.
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


## Assumptions
1. We're using the Scrimage library for unit tests. Because of floating-point precision issues, rounding pixel values can
    cause incorrect calculations. Therefore, we're testing pixel values with a tolerance of being off by 1.
1. Image processor implementation assumes that the convolution filters' kernel size is 3x3. Some parts of the implementation
    were written parameterized, but we'll need to change row buffer usage if there is a need for larger kernel sizes.
