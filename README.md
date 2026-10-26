# FindingResonance

A tool for singers and phoneticians to explore formant tuning.

Small adjustments to vowel quality can make a sung note either come alive or fizzle out. On a given pitch, shifting the vowel just slightly can sometimes produce a beautiful, resonant explosion of amplitude. FindingResonance is a way to explore where those moments happen.

Think of singing in the shower: when a harmonic of your voice lines up with a resonance of the room, it gets an extra boost. Something similar happens in the vocal tract, except that we can change its resonances by adjusting vowel quality. The fundamental frequency (f0) and its harmonics excite the vocal tract, whose resonances amplify particular frequencies.

FindingResonance lets you experiment with different vowels on a particular pitch. For example, you could sing a series of FLEECE vowels on D4, making small adjustments to tongue height and frontness/backness. The scripts measure f0, estimate F1–F3, and identify how prominent each harmonic is relative to its neighbours. An R plot shows the vowel's estimated F1/F2 position, formant bandwidths, and a lattice of harmonics, alongside a plot of harmonic prominence.

**Caution:** Formant measurements are not direct measurements of vocal-tract resonances. Especially at higher f0, estimated formants can be attracted towards harmonics (Whalen et al. 2022). Treat the plots as an aid to exploration, alongside listening and your own experience of the sound, not proof of resonance–harmonic alignment.

## Getting started

You'll need [Praat](https://www.fon.hum.uva.nl/praat/) and [R](https://cran.r-project.org/) with [RStudio](https://posit.co/download/rstudio-desktop/).

1. On GitHub, choose **Code → Download ZIP**, then unzip the repository.
2. Open `findingResonance.praat` in Praat (**Praat → Open Praat script…**) and choose **Run → Run**.
3. In **Settings**, choose `midpoint` or `middle50` for measurements, then `recordNew` or `openFile`. Set `base_dir` to the folder containing the scripts (you can drag the folder into the field).
4. **If you selected `recordNew`, then record your voice:** when prompted, use the Praat Objects window: **New → Record mono Sound**. Record your vowels, click **Save to list**, select the new Sound object, then continue the script.
5. **To use existing recordings:** choose `openFile` and set `snd_dir` to a folder containing `.wav` files.
6. In the sound editor, select the vowel portion you want analysed. Enter a vowel label and choose **selectAnotherChunk** or **lastChunkForThisSoundfile**. Repeat as needed.
7. The Praat script saves measurements (CSV), audio clips, and spectra in a timestamped output folder. For new recordings, look inside `archive/`.
8. Open `findingResonance_singleVowel.R` in RStudio. Install the required packages once using `install.packages(c("tidyverse", "patchwork", "rstudioapi"))` (in RStudio you can also click the 'install' button near the top of the interface), then run the script to generate PNG plots in the output folder.

## What the plots show

- **Vowel plot:** estimated F1 and F2, their bandwidths, and the frequencies of harmonics at the recorded f0. When F3 is closer to a harmonic than F1 or F2, an arrow points towards F3.
- **Harmonic prominence plot:** the amplitude of each harmonic relative to the average of its neighbouring harmonics, in dB. A larger value means that harmonic stands out more strongly.
- **Combined vowel plot:** if some vowels are within a semitone of each other you get a combo plot with the harmonic grid based on their median f0

## Conference presentations and posters

- [BAAP presentation, 20260409](conferenceOutputs/Gibson-BAAP-presentation-20260409.pdf) — introduction to FindingResonance.
- [LabPhon 20 poster, 20260627](conferenceOutputs/Gibson-LabPhon-poster-20260627.pdf) — *Finding Resonance in Speech and Song*.
- [NWAV poster, 20261024](conferenceOutputs/Gibson-NWAV-poster-20261024.pdf) — *Pop Song English and Singing Technique: Investigating the Role of Formant-Harmonic Alignment*.

## Reference

Whalen, D. H., et al. (2022). “Formants are easy to measure; resonances, not so much: Lessons from Klatt (1986).” *Journal of the Acoustical Society of America*, 152(2).
