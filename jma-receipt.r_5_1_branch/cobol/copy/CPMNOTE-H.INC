      * 01  MNOTE-H-REC.
            03  MNOTE-H-KEY.
                05  MNOTE-H-HOSPNUM
                                       PIC 9(2).
                05  MNOTE-H-TBL-KEY
                                       PIC X(8).
      *         Ϣ��
                05  MNOTE-H-RENNUM PIC 9(4).
      *         ���롼��
                05  MNOTE-H-TBL-GROUP
                                       PIC X(14).
      *         UUID
                05  MNOTE-H-TBL-UUID
                                       PIC X(36).
      *         ������Ϣ��
                05  MNOTE-H-SHORI-RENNUM
                                       PIC 9(4).
      *    ��̳������
           05  MNOTE-H-GYOUMU-CD    PIC 9(03).
      *    ���Ͼ�ꥳ����
           05  MNOTE-H-LOCATION-CD  PIC X(02).
      *    ������ɣ�
           03  MNOTE-H-SHELLID     PIC X(20).
      *    ����ǯ��
           03  MNOTE-H-SRYYM       PIC X(6).
      *    ����ǯ����
           03  MNOTE-H-SRYYMD      PIC X(8).
      *    ���
           03  MNOTE-H-CNT         PIC 9(5).
      *    ������ʬ
           03  MNOTE-H-NYUGAIKBN   PIC X(1).
      *    ���ԣɣ�
           03  MNOTE-H-PTID        PIC 9(10).
      *    �ɥ�����������
           03  MNOTE-H-DRCD        PIC X(5).
      *    ���Ǽ���
           03  MNOTE-H-TO-FOLDER   PIC X(200).
      *    ��ե�����̾
           03  MNOTE-H-TO-DATA     PIC X(200).
      *    �����ɶ�ʬ�ʶ��򡢣����ţգ� �����ӡݣ����
           03  MNOTE-H-CODE-TYPE   PIC X(01).
      *    ���ϥե饰
           03  MNOTE-H-PUTFLG      PIC X(1).
      *    ���������ȥ�
           03  MNOTE-H-TITLE       PIC X(100).
      *    �ǡ����С������
           03  MNOTE-H-DATA-VERSION
                                   PIC X(1).
      *    ���ڥ졼���ɣ�
           03  MNOTE-H-OPID        PIC X(16).
      *    ������
           03  MNOTE-H-CREYMD      PIC X(8).
      *    ����ǯ����
           03  MNOTE-H-UPYMD       PIC  X(08).
      *    ��������
           03  MNOTE-H-UPHMS       PIC  X(06).