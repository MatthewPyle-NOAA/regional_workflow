#! /bin/ksh -l

  export TOTAL_TASKS=${TOTAL_TASKS:-1}
  export NCTSK=${NCTSK:-1}
  export NCNODE=${NCNODE:-24}
  #export OMP_NUM_THREADS_CH=${OMP_THREADS:-24}
  export OMP_NUM_THREADS_CH=${OMP_NUM_THREADS_CH:-${OMP_THREADS:-${OMP_NUM_THREADS:-24}}}
  export OMP_NUM_THREADS=${OMP_THREADS:-${OMP_NUM_THREADS:-24}}
  export KMP_STACKSIZE=1024m
  export KMP_AFFINITY=disabled

# WCOSS-cray definitions
  export APRUNS=${APRUNS:-"aprun -b -j1 -n1 -N1 -d1 -cc depth"}
  export APRUNF=${APRUNF:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth cfp"}
  export APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}
  export APRUNO="time"
  export BACKGROUND=""
