      *01  DPCOPE-REC.
           03  DPCOPE-YUKOSTYMD      PIC X(8).
           03  DPCOPE-YUKOEDYMD      PIC X(8).
           03  DPCOPE-MDC-CD         PIC X(2).
           03  DPCOPE-BUNRUI-CD      PIC X(4).
           03  DPCOPE-OPE-VAL        PIC 9(5).
           03  DPCOPE-OPE-FLG        PIC 9(2).
           03  DPCOPE-OPE-AGE        PIC X(1).
           03  DPCOPE-OPE-CD         PIC X(2).
           03  DPCOPE-OPE-OCC        OCCURS 5.
               05  DPCOPE-K-NAME     PIC X(200).
               05  DPCOPE-K-CD       PIC X(20).
      *virtual
           03  DPCOPE-SEL-DPC-CD     PIC X(14).
