       01  TECHO-REC.
      *    ������
           03  TECHO-SRYYMD          PIC X(08).
      *    �������
           03  TECHO-CNT             PIC 9(03).
      *   �ɥ�����
           03  TECHO-DRCD            PIC X(05).
      *    ��������
           03  TECHO-DATA-REC.
      *        ������
               05  TECHO-ZAI-MAX         PIC 9(03).
      *        �ޥơ��֥�
               05  TECHO-REC-TBL         OCCURS   100.
      *            �޲��
                   07  TECHO-ZAIKAIKEI       PIC 9(03).
      *            ���Ŷ�ʬ
                   07  TECHO-SRYKBN          PIC X(02).
      *            ���ż���
                   07  TECHO-SRYSYUKBN       PIC X(03).
      *            ���ֹ�
                   07  TECHO-ZAINUM          PIC  9(08).
      *            ������
                   07  TECHO-NAI-TBL         OCCURS   50.
      *                ������
                       09  TECHO-SRYCD           PIC  X(09).
      *                ����
                       09  TECHO-SRYSURYO        PIC  9(05)V9(05).
      *                ����ʳ�������
                       09  TECHO-SRYKAISU        PIC  9(03).
      *                ������̾��
                       09  TECHO-INPUTCOMENT     PIC  X(140).
      *                ����̾������
                       09  TECHO-SHO-SRYCD       PIC  X(09).
      *                �����Ϳ���
                       09  TECHO-KANSURYO        PIC  9(05)V9(05).
      *                ���̡ʣ���ī�������롢�����롢������������������
                       09  TECHO-YOURYOU-TBL.
                           11  TECHO-YOURYOU     PIC 9(05)V9(05)
                                                   OCCURS   5.
      *            ��ˡ�����ɡʶ��򤢤��
                   07  TECHO-YSRYCD          PIC X(09).
      *
      *    ��ɼ�ֹ�ʼ����ɼ�ֹ��
           03  TECHO-DENPNUM         PIC 9(07).
      *    ��ɼ�ֹ�ʽ������Ƥ��о���ɼ�ֹ��
           03  TECHO-GRP-DENPNUM-G.
               05  TECHO-GRP-DENPNUM     PIC 9(07)
                                           OCCURS   10.
      *
      *    COMMON-SPA
           03  TECHO-TERMID                      PIC X(64).
           03  TECHO-MOTOPG                      PIC x(08).
           03  TECHO-MOTOPG2                     PIC x(08).
           03  TECHO-MOTOPG3                     PIC x(08).
           03  TECHO-SAKIPG                      PIC x(08).
           03  TECHO-NAME                        PIC X(50).
           03  TECHO-KANANAME                    PIC X(50).