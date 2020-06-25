      *01  DPCACT1-REC.
           03  DPCACT1-YUKOSTYMD      PIC X(8).
           03  DPCACT1-YUKOEDYMD      PIC X(8).
           03  DPCACT1-MDC-CD         PIC X(2).
           03  DPCACT1-BUNRUI-CD      PIC X(4).
           03  DPCACT1-ACT1-CD        PIC X(1).
           03  DPCACT1-ACT1-FLG       PIC 9(2).
           03  DPCACT1-K-CD           PIC X(20).
           03  DPCACT1-ACT1-OCC       OCCURS 2.
               05  DPCACT1-J-NAME     PIC X(200).
               05  DPCACT1-J-CD       PIC X(20).
      *virtual
           03  DPCACT1-SEL-DPC-CD     PIC X(14).
