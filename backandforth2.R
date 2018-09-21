listrationals <- function(maxsum = 10) {
	rationals = c(0)
	sum = 2
	numerator = 1
	increase_numerator = TRUE
	while(sum <= maxsum) {
		if (increase_numerator) {
			while (numerator < sum) { 
				print(c("num, denom, sum:", numerator, sum - numerator, sum))
				rationals = append(rationals, (numerator / (sum - numerator)))
				rationals = append(rationals, (-numerator / (sum - numerator)))
				numerator = numerator + 1
				if (numerator == sum) {sum = sum + 1; break}
			}
			increase_numerator = !increase_numerator
		} else {
			while (numerator > 0) {
				print(c("num, denom, sum:", numerator, sum - numerator, sum))
				rationals = append(rationals, (numerator / (sum - numerator)))
				rationals = append(rationals, (-numerator / (sum - numerator)))
				if (numerator == 1) {sum = sum + 1; break} else {numerator = numerator - 1}
			}
			increase_numerator = !increase_numerator
		}
	}
	return(unique(rationals))
}

backandforth <- function() {
	Q = listrationals(100)
	Qsort = sort(Q)
	Qm = Q[-1]
	Qmsort = sort(Qm)
	functiontable = c(1) # as an arbitrary(?) choice, map 0 to 1, which means mapping Q[1] to Qm[1].  Index of functiontable = index of Q
	for (i in 2:length(Q)) {
	  maxQ = max(Q[1:length(functiontable)])
	  maxQm = max(functiontable) # don't need the largest element of Qm overall, just the largest we've used so far
	  minQ = min(Q[1:length(functiontable)])
	  minQm = min(functiontable)
	  if (Q[i] > maxQ) {
	    if (max(Qm) > maxQm) { functiontable[i] = min(Qm[Qm > maxQm]) } else {functiontable[i] = NA} # the goal is to choose the smallest element of Qm greater than any currently in the table
	  } else if (Q[i] < minQ) {
	    if (min(Qm) < minQm) { functiontable[i] = max(Qm[Qm < minQm]) } else {functiontable[i] = NA}
	  } else {
	    
	  }
	}
}