      ******************************************************************
      *    CPMML0016.INC
      ******************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * �����ե�����˽��Ϥ���Ȥ��ϡ�ZZZ�Խ�����
      * â�������ݤξ��Τߡ�00�פ����ꤹ��
      ************************************************************
       01  MML0016-VALUES.
           02  MML0016-T.
               05  FILLER               PIC  X(20)   VALUE   "died" .
               05  FILLER               PIC  X(01)   VALUE   "2".  
               05  FILLER               PIC  X(20)   
                   VALUE   "worsening" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
               05  FILLER               PIC  X(20)   
                   VALUE   "unchanged" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
               05  FILLER               PIC  X(20)   
                   VALUE   "recovering" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
               05  FILLER               PIC  X(20)   
                   VALUE   "fullyRecovered" .
               05  FILLER               PIC  X(01)   VALUE   "1".  
               05  FILLER               PIC  X(20)   
                   VALUE   "sequelae" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
               05  FILLER               PIC  X(20)   
                   VALUE   "end" .
               05  FILLER               PIC  X(01)   VALUE   "1".  
               05  FILLER               PIC  X(20)   
                   VALUE   "pause" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
               05  FILLER               PIC  X(20)   
                   VALUE   "continued" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
               05  FILLER               PIC  X(20)   
                   VALUE   "transfer" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
               05  FILLER               PIC  X(20)   
                   VALUE   "transferAcute" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
               05  FILLER               PIC  X(20)   
                   VALUE   "transferChronic" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
               05  FILLER               PIC  X(20)   
                   VALUE   "home" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
               05  FILLER               PIC  X(20)   
                   VALUE   "unknown" .
               05  FILLER               PIC  X(01)   VALUE   "3".  
           02  MML0016-R  REDEFINES   MML0016-T.
             03  MML0016-TX           OCCURS  14.
               05  MML0016-TENKI        PIC  X(20).
               05  MML0016-ORCA-TENKI   PIC  X(01).