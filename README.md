# Finger Smudge

![Finger smudged](https://raw.githubusercontent.com/josephwilk/finger-smudge/master/resources/finger-smudge.png)

## Notes 

Chromaprint Algorithm:

1. FFT transform of audio.
2. Frequencies => Musical notes (not octaves). Chroma features.
3. 16x12 pixel window moving across image one pixel at a time
4. Apply 16 filteres that capture intensity dirrerences across musical notes && time.
5. Filters "calculate the sum of specific areas of the grayscale subimage and then compare the two sums."
6. Quantize the real number with 3 coeeficents (learnt).
7. 16 filters and each can produce an integer that can be encoded into 2 bits (using the Gray code), so if you combine all the results, you get a 32-bit integer.

## Resources

* [Chromaprint](https://bitbucket.org/acoustid/chromaprint)
* [How Chromaprint works](https://oxygene.sk/2011/01/how-does-chromaprint-work/)

## License

Copyright © 2016 Joseph Wilk

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
