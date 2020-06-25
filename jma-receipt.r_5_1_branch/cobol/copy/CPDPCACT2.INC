      *01  DPCACT2-REC.
           03  DPCACT2-YUKOSTYMD      PIC X(8).
           03  DPCACT2-YUKOEDYMD      PIC X(8).
           03  DPCACT2-MDC-CD         PIC X(2).
           03  DPCACT2-BUNRUI-CD      PIC X(4).
           03  DPCACT2-ACT2-CD        PIC X(1).
           03  DPCACT2-ACT2-FLG       PIC 9(2).
           03  DPCACT2-ACT2-OCC       OCCURS 2.
               05  DPCACT2-J-NAME     PIC X(200).
               05  DPCACT2-J-CD       PIC X(20).
      *virtual
           03  DPCACT2-SEL-DPC-CD     PIC X(14).
