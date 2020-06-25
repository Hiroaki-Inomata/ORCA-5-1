      *01  DPCTENSU-REC.
           03  DPCTENSU-YUKOSTYMD              PIC X(8).
           03  DPCTENSU-YUKOEDYMD              PIC X(8).
           03  DPCTENSU-DPC-CD.
               05  DPCTENSU-DPC-MDC-CD         PIC X(2).
               05  DPCTENSU-DPC-BUNRUI-CD      PIC X(4).
               05  DPCTENSU-DPC-BYOTAI-CD      PIC X(1).
               05  DPCTENSU-DPC-AGE-CD         PIC X(1).
               05  DPCTENSU-DPC-OPE-CD         PIC X(2).
               05  DPCTENSU-DPC-ACT1-CD        PIC X(1).
               05  DPCTENSU-DPC-ACT2-CD        PIC X(1).
               05  DPCTENSU-DPC-SUBBYO-CD      PIC X(1).
               05  DPCTENSU-DPC-JUSHO-CD       PIC X(1).
           03  DPCTENSU-MDC-CD         PIC X(2).
           03  DPCTENSU-BUNRUI-CD      PIC X(4).
           03  DPCTENSU-MOKUTEKI-CD    PIC X(1).
           03  DPCTENSU-AGE-CD         PIC X(1).
           03  DPCTENSU-OPE-CD         PIC X(2).
           03  DPCTENSU-ACT1-CD        PIC X(1).
           03  DPCTENSU-ACT2-CD        PIC X(1).
           03  DPCTENSU-SUBBYO-CD      PIC X(1).
           03  DPCTENSU-JUSHO-CD       PIC X(1).
           03  DPCTENSU-BYOMEI         PIC X(120).
           03  DPCTENSU-OPE            PIC X(160).
           03  DPCTENSU-ACT1           PIC X(30).
           03  DPCTENSU-ACT2           PIC X(30).
           03  DPCTENSU-SUBBYO         PIC X(30).
           03  DPCTENSU-JUSHO          PIC X(50).
           03  DPCTENSU-KIKAN          PIC 9(3) OCCURS 3.
           03  DPCTENSU-TEN            PIC 9(8) OCCURS 3.
           03  DPCTENSU-SORT-VALUE     PIC X(14).
      *    à»â∫virtualçÄñ⁄
           03  DPCTENSU-NO             PIC 9(5).
