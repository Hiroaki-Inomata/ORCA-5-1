      * 01  TOUKEICSV-H-REC.
            03  TOUKEICSV-H-KEY.
                05  TOUKEICSV-H-HOSPNUM
                                       PIC 9(2).
                05  TOUKEICSV-H-TBL-KEY
                                       PIC X(8).
      *         Ϣ��
                05  TOUKEICSV-H-RENNUM PIC 9(4).
      *         ���롼��
                05  TOUKEICSV-H-TBL-GROUP
                                       PIC X(14).
      *         ������Ϣ��
                05  TOUKEICSV-H-SHORI-RENNUM
                                       PIC 9(4).
      *    ������ɣ�
           03  TOUKEICSV-H-SHELLID     PIC X(20).
      *    ����ǯ��
           03  TOUKEICSV-H-SRYYM       PIC X(6).
      *    ����ǯ����
           03  TOUKEICSV-H-SRYYMD      PIC X(8).
      *    ���
           03  TOUKEICSV-H-CNT         PIC 9(8).
      *    ������ʬ
           03  TOUKEICSV-H-NYUGAIKBN   PIC X(1).
      *    ���Ǽ���
           03  TOUKEICSV-H-TO-FOLDER   PIC X(200).
      *    ��ե�����̾
           03  TOUKEICSV-H-TO-DATA     PIC X(200).
      *    �����ɶ�ʬ�ʶ��򡢣����ţգ� �����ӡݣ����
           03  TOUKEICSV-H-CODE-TYPE   PIC X(01).
      *    ���ϥե饰
           03  TOUKEICSV-H-PUTFLG      PIC X(1).
      *    ���������ȥ�
           03  TOUKEICSV-H-TITLE       PIC X(100).
      *    ������
           03  TOUKEICSV-H-CREYMD      PIC X(8).
      *    ����ǯ����
           03  TOUKEICSV-H-UPYMD       PIC  X(08).
      *    ��������
           03  TOUKEICSV-H-UPHMS       PIC  X(06).