      ************************************************************
      *    �����ģУü����ѥ�᥿������COPY (CPORCSDPCNYUIN.INC)
      *                                ���ѥץ������ (ORCSDPCNYUIN)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ����ǯ��
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.00.01    NACL-��Ʀ��  xx/xx/xx  ���롼�׿����б�
      ************************************************************
       01  ORCSDPCNYUIN-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-DPCNYUIN-HOSPNUM                PIC  9(02).
      *���ԣɣ�
           03  LNK-DPCNYUIN-PTID                   PIC  9(10).
      *����ǯ��
           03  LNK-DPCNYUIN-SANTEIYM               PIC  X(06).
      *�ݸ���ʬ�ʣ������ݡ��������ݡ���������Ϣ���
           03  LNK-DPCNYUIN-HKNKBN                 PIC  X(01).
      *�о��ݸ��ȹ礻�ֹ�
           03  LNK-DPCNYUIN-SET-HKNCOMBI           PIC  9(04) OCCURS 10.
      *�ֵѹ���
           03  LNK-DPCNYUIN-HENKYAKU-AREA.
      *�����ֵѥơ��֥��ź��������������ź��������������������������
      *                  ź�������������ܡʿ������ܡ�
      *                  ź�������������ܡʴĶ����ܡˡ�
             04  LNK-DPCNYUIN-HENKYAKU-TABLE       OCCURS  5.
      *�������ֵѥơ��֥�
               05  LNK-DPCNYUIN-HENKYAKU-TBL       OCCURS  40.
      *�ݸ��ȹ礻
                   07  LNK-DPCNYUIN-HKNCOMBI       PIC  9(04).
      *�����ֹ�
                   07  LNK-DPCNYUIN-BTUNUM         PIC  9(02).
      *�����ʬ
                   07  LNK-DPCNYUIN-BTUKBN         PIC  X(02).
      *���Ų�
                   07  LNK-DPCNYUIN-SRYKA          PIC  X(02).
      *�ɥ�����������
                   07  LNK-DPCNYUIN-DRCD           PIC  X(05).
      *�����ֹ棲��Ʊ���������ѡ�
                   07  LNK-DPCNYUIN-BTUNUM2        PIC  9(02).
      *�����ʬ����Ʊ���������ѡ�
                   07  LNK-DPCNYUIN-BTUKBN2        PIC  X(02).
      *���Ųʣ���Ʊ���������ѡ�
                   07  LNK-DPCNYUIN-SRYKA2         PIC  X(02).
      *�ɥ����������ɣ���Ʊ���������ѡ�
                   07  LNK-DPCNYUIN-DRCD2          PIC  X(05).
      *���ٿ�
                   07  LNK-DPCNYUIN-MEISAISU       PIC  9(03).
      *�쥻�ŵ�Ͽ���ꥢ
                   07  LNK-DPCNYUIN-NYUIN          OCCURS 100.
      *�쥳���ɼ���
                       09  LNK-DPCNYUIN-RECSKB     PIC  X(02).
      *���ż���
                       09  LNK-DPCNYUIN-SRYSKB     PIC  X(02).
      *���Ź԰٥�����
                       09  LNK-DPCNYUIN-SRYCD      PIC  X(09).
      *����ü�������о�
                       09  LNK-DPCNYUIN-HASUU      PIC  X(01).
      *�������������ʬ
                       09  LNK-DPCNYUIN-SENTEI-GAIHAKU
                                                   PIC 9(01).
      *����
                       09  LNK-DPCNYUIN-SURYO      PIC  9(01).
      *����
                       09  LNK-DPCNYUIN-TEN        PIC  9(07).
      *���
                       09  LNK-DPCNYUIN-KAISU      PIC  9(02).
      *�����ȵ�Ͽ�ǡ���
                       09  LNK-DPCNYUIN-COMMENT    PIC  X(76).
      *Ʊ����������ʬ
                       09  LNK-DPCNYUIN-SAINYUINKBN PIC X(01).
      *�о���
                   07  LNK-DPCNYUIN-DD             PIC  9(02).
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼
      *            ����������פʤ����������ݸ��ȹ礻�������顼��
           03  LNK-DPCNYUIN-RC                     PIC S9(9) BINARY.
      *
