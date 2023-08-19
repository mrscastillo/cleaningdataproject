Codebook.

Row data

The features come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ

  Features:       The complete list of variables of each feature vector is available in 'features.txt'
  Activitylabels: activitycodes and labels
  testdfDT:       Testdf  Signals of Test subjects transformed in data frame and named with understandable                   names
  traindfDT:      traindf  Signals of tranning subjects transformed in data frame and named with                             understandable names
  test:           testdfDT with new variable setstype = "test"
  training:       traindfDT with new variable setstype ="train"
  ds:             Merges the training and the test sets to create one data set.
  dsfactor:       ds with descriptive activity names to name the activities in the data set
  Variables:      where rename to: activity, activityid and sets 
  Dsfactordf:     dsfactor  where converted to data frame
  dsmeans :       Setsmean: mean valiables of each massure converted in a data frame and renamed
  dssd:           Setssd: standard deviation valiables of each massure converted in a data frame and                         renamed
  dscompleate:    combine the variables mean a standard deviation of each massurament descriptive variable                   names
  dsgroup:        is dscompleate  grouped by: subject, setstype, activity )
  dssummarize:    summarize dsgroup by: subject, setstype, activity and the avg of each measure
 
