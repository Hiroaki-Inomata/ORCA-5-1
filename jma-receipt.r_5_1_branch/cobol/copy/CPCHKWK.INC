      *****************************************************************
      *    �����å��ޥ���        COPY (CPCHKWK.INC)                   *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  CHKWK-REC.
      *������ʬ
           03  CHKWK-SYORIKBN            PIC  X(01).
      *
           03  CHKWK-KEY.
      *�����å���ʬ
               05  CHKWK-CHKKBN          PIC  X(01).
      *���ť�����
               05  CHKWK-SRYCD           PIC  X(09).
      *ͭ������ǯ����
               05  CHKWK-YUKOSTYMD       PIC  X(08).
      *ͭ����λǯ����
               05  CHKWK-YUKOEDYMD       PIC  X(08).
      *�����ɶ�ʬ
               05  CHKWK-CDKBN           PIC  X(01).
      *Ϣ�ֹ�
               05  CHKWK-RENNUM          PIC  9(02).
      *ͭ����λǯ����2
           03  CHKWK-YUKOEDYMD2          PIC  X(08).
      *Ŭ�ѥ����ɸĿ�
           03  CHKWK-TEKCDNUM            PIC  9(03).
      *�����ɣ����飱��
           03  CHKWK-CD-G.
               05  CHKWK-CD1             PIC  X(09).
               05  CHKWK-CD2             PIC  X(09).
               05  CHKWK-CD3             PIC  X(09).
               05  CHKWK-CD4             PIC  X(09).
               05  CHKWK-CD5             PIC  X(09).
               05  CHKWK-CD6             PIC  X(09).
               05  CHKWK-CD7             PIC  X(09).
               05  CHKWK-CD8             PIC  X(09).
               05  CHKWK-CD9             PIC  X(09).
               05  CHKWK-CD10            PIC  X(09).
      *
           03  CHKWK-CD-R                REDEFINES   CHKWK-CD-G.
               05  CHKWK-CD              PIC  X(09)
                                         OCCURS   10.
      *ü���ɣ�
           03  CHKWK-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CHKWK-OPID                PIC  X(16).
      *����ǯ����
           03  CHKWK-CREYMD              PIC  X(08).
      *����ǯ����
           03  CHKWK-UPYMD               PIC  X(08).
      *�������� 
           03  CHKWK-UPHMS               PIC  X(06).
      *