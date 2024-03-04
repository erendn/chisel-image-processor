# Chisel Image Processor

Things to do:
1. Find a strategy for edge pixels.
    - Since edge pixels are missing some neighbor pixels, kernel becomes off the image.
    - Currently the processor doesn't apply filters on edge pixels since we haven't decided how to handle them.
    - We can treat those missing pixels as empty, duplicate pixels from the nearest edge, or wrap pixels from the other side.
1. Attempt to fix the Sobel filter so that it will match the library
    - Scrimage library combines vertical and horizontal kernel results in a different way.
    - Our filter does: `sum = max(abs(gx) + abs(gy), 255)`
    - Scrimage library does: `sum = max(sqrt(gx*gx + gy*gy) / 1.8, 255)`
1. Add more filters to match their equivalent from the library
1. Check if output pixels match the library in unit tests
