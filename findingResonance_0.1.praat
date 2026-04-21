# getFormantsBandwidths fromSelection

# Andy Gibson

# to do:
#save 2 versions of csv, one for posterity in the out folder, and another named predictably so R script doesn't need input;

# v0.1 setting up so it's shareable

# old version: getFormants_Bandwidths_fromSelection_v1.2.praat
# v1.2 remove duplication of the audio analysis process script
# v1.1 fixed output of targetA; some tidying
# v1.0 first version for working with mphil data: allow points not selections. allow to get from textgrid points. 
# v0.9 baap final version
# v0.8 going back to dB for prominence measure, taking difference of target H to mean of neighbour Hs in dB
# v0.7 deleting targetA_avg stuff which was not working properly.
# v0.6 option to record a new sound or upload from file.
# v0.5 getting the spectrum from the middle 50% of the vowel and saving it as part of this. Hashing out stuff about an uploaded spec
# v0.4 getting formants/bws from the file too and putting all in a single csv
# v0.3 improving output csv name etc
# v0.2 looping over vowel spectra and outputting csv
# v0.1 getting started

# --- Output CSV ---

form Settings
	comment: "What kind of measurement would you like for f0 and formants?"
	choice: "measure", 1
	button: "midpoint"
	button: "middle50"
	choice: "recordingSource", 1
	button: "recordNew"
	button: "openFile"
	comment: "Drag the folder where the scripts are to this field:"
	text base_dir /Users/andrewgibson/Documents/GitHub/FindingResonance
	text snd_dir /AddPathToWavFiles_ifUsingPrerecordedAudio

endform

# GET DATESTAMP FOR SAVING:
@format_date_time
#pause ding dong, it's currently 'formatted_dt$'  

# add trailing slash according to system
if macintosh = 1
   base_dir$ = base_dir$ + "/"
elsif Windows = 1
	base_dir$ = base_dir$ + "\"
endif

# add archive folder

### TO DEBUG: ADD IFs SO IT ONLY DOES THIS IF MISSING FINAL SLASH

createFolder: base_dir$ + "archive"


# add trailing slash according to system
if macintosh = 1
   base_dir$ = base_dir$ + "/"
elsif Windows = 1
	base_dir$ = base_dir$ + "\"
endif


if recordingSource = 2
	save_dir$ = snd_dir$ + formatted_dt$ + "/"
elsif recordingSource = 1
	save_dir$ = base_dir$ + "archive/" + formatted_dt$ + "/"
endif
createFolder: save_dir$

# make a file with the path for R to read later
pointer_file$ = base_dir$ + "latest_run.txt"
writeFileLine: pointer_file$, save_dir$

results_file_name$ = "testingScript_" + measure$
results_file_extension$ = ".csv"


# SET UP OUTPUT

# save results file
results_file$ = save_dir$ + results_file_name$ + "_" + formatted_dt$ + results_file_extension$
latest_file$ = base_dir$ + "resonance_data.csv"

# Check if the result file exists; if it does, give option to delete it
if fileReadable (results_file$)
	pause The result file already exists! Overwrite?
	filedelete 'results_file$'
endif



results_file$ = save_dir$ + results_file_name$ + "_" + formatted_dt$ + results_file_extension$

# Write a row with column titles to the result file
titleline$ = "Token,Vowel,f0,rms,F1,F2,F3,Duration,BW1,BW2,BW3,Harmonic,TargetF,LoNeighbourF,HiNeighbourF,TargetAmp,LoNeighbourAmp,HiNeighbourAmp,AvgNeighbourAmp,TargetProm"
writeFileLine: results_file$, titleline$
writeFileLine: latest_file$, titleline$

# GET SOUNDS

# if a new recording, record, then save the long wav
if recordingSource = 1
	nsnd = 1
	pause Record your voice! (readme for help)!
	snd = selected()
	selectObject: snd
	save_path1$ = save_dir$ + formatted_dt$ + "_long.wav"
	Save as WAV file: "'save_path1$'"
endif

# if using existing sounds: list the wav files in the directory
if recordingSource = 2
	sndlist = Create Strings as file list... list 'snd_dir$'*.wav
	selectObject: sndlist
	nsnd = Get number of strings
	pause There are 'nsnd' wavs in the folder
endif

############## FOR EACH WAV/SPECTRUM (or for the file currently selected in objects if recording new)
for w from 1 to nsnd
##############

	vowel$ = "test"
	# open the wav assign id number (if getting from file)
	if recordingSource = 2
		selectObject: sndlist
		sndname$ = Get string: w
		snd = Read from file... 'snd_dir$''sndname$'
		vowel$ = mid$ ("'sndname$'", 4, 3)
	endif

	# select the vowel portions to be measured

	# set up the variable for the user interface
	eachClip = 1
	audCode = 0

	### begin repeat grab info for vowels
	repeat

		selectObject: snd
		View & Edit
		editor: snd

		# if doing many clips, likely you want the default button to be 'last chunk'
		beginPause: "select part to analyse then decide if this is the last clip"
			sentence: "Vowel", vowel$
		clicked = endPause: "selectAnotherChunk", "lastChunkForThisSoundfile", 2

		#pause about to measure 'vowel$' vowel

		# If no range is selected, a 1s clip is taken around the cursor
		range = Get length of selection
		if range = 0
			cursor = Get cursor
			Select: cursor - 0.5, cursor + 0.5
		endif

		clip = Extract selected sound (time from 0)
		editor: snd
		Close
		selectObject: clip
		dur = Get total duration
		rms = Get root-mean-square: 0, 0
		View & Edit
		editor: clip

			Select: 0.25 * dur, 0.75 * dur
			spec = View spectral slice

			# gather fundamental and formant info based on either a point or a range
			if measure$ = "middle50"
				Select: 0.25 * dur, 0.75 * dur
			elsif measure$ = "midpoint"
				Move cursor to: dur / 2
			else
				pause wierd! you should have a setting for measure.. but it's 'measure$'
			endif

			# get f0 into a number object (a bit tricky)
			f0string$ = Get pitch
			# this gives:   ; e.g. "--undefined-- Hz (interpolated pitch at CURSOR)" or "82.7027639593251 Hz ..."
			# Check for undefined first, if undefined make it 1Hz
			if index(f0string$, "--undefined--") > 0
	   		f0 = 1
			else
		   # Extract number before first space
				spacePos = index(f0string$, " ")
				f0 = number(left$(f0string$, spacePos - 1))
			endif

			# get formants and bandwidths
			f1 = Get first formant
			b1 = Get first bandwidth
			f2 = Get second formant
			b2 = Get second bandwidth
			f3 = Get third formant
			b3 = Get third bandwidth

			Close
		endeditor
	
		editor: spec
		Close

		# save the spectrum and short wav
		selectObject: spec
		save_path_spec$ = save_dir$ + "_" + string$(eachClip) + "_" + vowel$ + "_" + formatted_dt$ + ".Spectrum"
		Save as text file: "'save_path_spec$'"

		selectObject: clip
		save_path2$ = save_dir$ + "_" + string$(eachClip) + "_" + vowel$ + "_" +  formatted_dt$ + "_short.wav"
		Save as WAV file: "'save_path2$'"

		selectObject: spec

		#FOR EACH HARMONIC
			for targetH from 1 to 13
			selectObject: spec
			# get frequencies of harmonics from f0
			loNeighF = (targetH - 1) * f0
			targetF  = targetH * f0
			hiNeighF = (targetH + 1) * f0

			# for H1 let's not calc prominence since there's no lower harmonic
			if targetH = 1
				loNeighF = targetF
				hiNeighF = targetF
			endif
 
			# get amplitudes from spectrum at each harmonic, for the neighbours... i know this is a bit redundant. we collect the info 3 times for each harmonic. oh well.
			loNeighA = Get sound pressure level of nearest maximum: loNeighF
			targetA  = Get sound pressure level of nearest maximum: targetF
			hiNeighA = Get sound pressure level of nearest maximum: hiNeighF

			# get the average amp of the neighbours
			neighA = (loNeighA + hiNeighA) / 2

			targetProm = targetA - neighA
   	   fileappend "'results_file$'" 'eachClip', 'vowel$', 'f0', 'rms', 'f1', 'f2', 'f3', 'dur', 'b1', 'b2', 'b3', 'targetH', 'targetF', 'loNeighF', 'hiNeighF','targetA','loNeighA','hiNeighA','neighA','targetProm''newline$'
			fileappend "'latest_file$'" 'eachClip', 'vowel$', 'f0', 'rms', 'f1', 'f2', 'f3', 'dur', 'b1', 'b2', 'b3', 'targetH', 'targetF', 'loNeighF', 'hiNeighF','targetA','loNeighA','hiNeighA','neighA','targetProm''newline$'
		endfor
		#ENDFOR EACH HARMONIC

		select all
		minusObject: snd
		if recordingSource = 2
			minusObject: sndlist
		endif
		Remove

		eachClip += 1
		
		if clicked = 1	
			pause now select the bit for token 'eachClip'
		endif

	### end repeat grab info for vowels			
	until clicked > 1
	### end repeat grab info for vowels			

############## ENDFOR EACH WAV/SPECTRUM (or for the file currently selected in objects if recording new)
endfor
############## ENDFOR


echo your csv is "input_csv": 'results_file$'

# make a file with the path for R to read later
pointer_file2$ = base_dir$ + "latest_results.txt"
writeFileLine: pointer_file2$, results_file$





## ---------------------------------------------------------------------------------------
# FORMAT CURRENT DATE/TIME
procedure format_date_time
#
allMonths$ = "JanFebMarAprMayJunJulAugSepOctNovDec"
date_time$ = date$()
year$ = right$(date_time$,4)
month$ = string$(((index_regex (allMonths$, mid$(date_time$,5,3))-1)/3)+1)
if length(month$) = 1
	month$ = "0" + month$
endif
day$ = replace_regex$(mid$(date_time$,9,2), "^ ", "0", 1)
hour$ = mid$(date_time$,12,2)
min$ = mid$(date_time$,15,2)

formatted_dt$ = year$ + month$ + day$ + "_" + hour$ + min$

endproc

## ---------------------------------------------------------------------------------------





# to do: error when using point not span, only on last time