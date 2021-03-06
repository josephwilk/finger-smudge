# Finger Smudge

An art project producing :musical_score: music for :computer: machines. 
Firstly exploring the digital fingerprinting of audio music. As used in sites like YouTube & SoundCloud to detect copyright infringment.

Listening: https://soundcloud.com/fingerprint-smudger/tracks

![Finger smudged](https://raw.githubusercontent.com/josephwilk/finger-smudge/master/resources/finger-smudge.png)

## Chromaprint

Using an open source audio fingerprinter Chromaprint to investigate forward and reverse generation of fingerprints. 

#### Fingerprint Images

Clojure FFT Example.
![Example spectro](https://raw.githubusercontent.com/josephwilk/finger-smudge/master/resources/spectro2.png)

Chromaprint FFT Image representation
![Chroma FFT Internal Image](https://raw.githubusercontent.com/josephwilk/finger-smudge/master/resources/chroma_fft_image.png)

* Y axis (Energy) -> A A# B C C# D D# E F F# G G#
* X axis (Time) - Is is fixed or dynamic...

#### Steps

1. FFT transform of audio (sampling rate 11025 Hz, frame size is 4096 (0.371 s) with 2/3 overlap.)
2. Frequencies => Musical notes (not octaves). Chroma features.
3. 16x12 pixel window moving across image one pixel at a time
4. Apply 16 filteres that capture intensity dirrerences across musical notes && time.
5. Filters "calculate the sum of specific areas of the grayscale subimage and then compare the two sums."
6. Quantize the real number with 3 coeeficents (learnt).
7. 16 filters and each can produce an integer that can be encoded into 2 bits (using the Gray code), so if you combine all the results, you get a 32-bit integer.

## Generative Shazaming 

Turn Shazam on, run a generative piece of music until a match is found (We screen-scrap to detect a Shazam match notification). Desktop matcher seems more accurate + requires more sampling time than mobile so we only use this: https://itunes.apple.com/gb/app/shazam/id897118787?mt=12. 

Avoid any noise by redirecting audio to SoundFlower and set input to SoundFlower. Perfect siganl :)

Turns out its pretty easy to trick Shazam. To make it more interesting we consider not only a match but the match with the highest number of previous Shazam examples (hence more data and Shazam should be more accurate).

#### Notes

* Chord progressions seem to dominate detection.
* Unsuprisingly pentatonic scale tends to do well.

## Listen to the Audio Experiments

* https://soundcloud.com/fingerprint-smudger/tracks
* https://soundcloud.com/fingerprint-smudger/generative-1467636802259

## Resources

* [Chromaprint](https://bitbucket.org/acoustid/chromaprint)
* [How Chromaprint works](https://oxygene.sk/2011/01/how-does-chromaprint-work/)
* [Experiments in Overtone with spectral analysis / FFTs](https://github.com/mikera/spectral)
* [Sonographic sound processing](http://designingsound.org/2013/04/sonographic-sound-processing/)
* JTransforms FFT fast in Java https://sites.google.com/site/piotrwendykier/software/jtransforms
* Clojure using JTransforms https://gist.github.com/scottdw/26e2491e53ebc28649f5

## License

Copyright © 2016-present Joseph Wilk

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
