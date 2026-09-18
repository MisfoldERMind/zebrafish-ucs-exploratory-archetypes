* Encoding: UTF-8.
TWOSTEP CLUSTER
  /CONTINUOUS VARIABLES=
    distance_rz
    crossing_rz
    top_entry_rz
    top_time_rz
  /DISTANCE LIKELIHOOD
  /NUMCLUSTERS AUTO 15 BIC
  /NOSTANDARDIZE VARIABLES=
    distance_rz
    crossing_rz
    top_entry_rz
    top_time_rz
  /HANDLENOISE 0
  /MEMALLOCATE 64
  /CRITERIA INITHRESHOLD(0) MXBRANCH(8) MXLEVEL(3)
  /VIEWMODEL DISPLAY=YES EVALUATIONFIELDS=ID Author
  /SAVE VARIABLE=TwoStep_Cluster.
