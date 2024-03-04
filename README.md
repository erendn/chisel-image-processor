# Chisel Image Processor

Things to do:
1. Find a strategy for edge pixels.
    - Since edge pixels are missing some neighbor pixels, kernel becomes off the image.
    - Currently the processor doesn't apply filters on edge pixels since we haven't decided how to handle them.
    - We can treat those missing pixels as empty, duplicate pixels from the nearest edge, or wrap pixels from the other side.
1. Parameterize the pipeline so that the processor can take a variable number of pixels every cycle.
1. After parameterizing the pipeline, parallelize filter application so that the pipeline won't have to stall.
1. Add more filters to match their equivalent from the library
1. Check if output pixels match the library in unit tests
