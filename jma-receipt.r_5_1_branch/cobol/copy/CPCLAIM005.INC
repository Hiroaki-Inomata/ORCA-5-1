      ******************************************************************
      *    CPCLAIM005.INC
      ******************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CLAIM005-VALUES.
           02  CLAIM005-T.
             03  CLAIM005-T01.
               05  CLAIM005-ORCA-T01   PIC  X(9)   VALUE   "700140000" .
               05  CLAIM005-CLAIM-T01  PIC  X(2)   VALUE   "01" .
               05  CLAIM005-NAME-T01   PIC  X(50)  
                   VALUE   "八切" .
             03  CLAIM005-T02.
               05  CLAIM005-ORCA-T02   PIC  X(9)   VALUE   "700110000" .
               05  CLAIM005-CLAIM-T02  PIC  X(2)   VALUE   "02" .
               05  CLAIM005-NAME-T02   PIC  X(50)  
                   VALUE   "六切" .
             03  CLAIM005-T03.
               05  CLAIM005-ORCA-T03   PIC  X(9)   VALUE   "700080000" .
               05  CLAIM005-CLAIM-T03  PIC  X(2)   VALUE   "03" .
               05  CLAIM005-NAME-T03   PIC  X(50)  
                   VALUE   "四切" .
             03  CLAIM005-T04.
               05  CLAIM005-ORCA-T04   PIC  X(9)   VALUE   "700050000" .
               05  CLAIM005-CLAIM-T04  PIC  X(2)   VALUE   "04" .
               05  CLAIM005-NAME-T04   PIC  X(50)  
                   VALUE   "大四切" .
             03  CLAIM005-T05.
               05  CLAIM005-ORCA-T05   PIC  X(9)   VALUE   "700030000" .
               05  CLAIM005-CLAIM-T05  PIC  X(2)   VALUE   "05" .
               05  CLAIM005-NAME-T05   PIC  X(50)  
                   VALUE   "大角" .
             03  CLAIM005-T06.
               05  CLAIM005-ORCA-T06   PIC  X(9)   VALUE   "700010000" .
               05  CLAIM005-CLAIM-T06  PIC  X(2)   VALUE   "06" .
               05  CLAIM005-NAME-T06   PIC  X(50)  
                   VALUE   "半切" .
             03  CLAIM005-T07.
               05  CLAIM005-ORCA-T07   PIC  X(9)   VALUE   "700450000" .
               05  CLAIM005-CLAIM-T07  PIC  X(2)   VALUE   "07" .
               05  CLAIM005-NAME-T07   PIC  X(50)  
                   VALUE   "半切（コンピューター断層撮影用）" .
           02  CLAIM005-R  REDEFINES   CLAIM005-T.
             03  CLAIM005-TX           OCCURS  7.
               05  CLAIM005-ORCA       PIC  X(9).
               05  CLAIM005-CLAIM      PIC  X(2).
               05  CLAIM005-NAME       PIC  X(50).
