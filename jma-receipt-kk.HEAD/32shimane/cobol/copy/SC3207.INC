        01  SC07.
      *����ǯ
            02  SC07-PRTYY  PIC X(4).
      *������
            02  SC07-PRTMM  PIC X(4).
      *���ŵ��ؽ����
            02  SC07-ADRS   PIC X(100).
      *���ŵ���̾��
            02  SC07-HOSPNAME   PIC X(100).
      *���߼Ի�̾
            02  SC07-KANRINAME  PIC X(30).
      *�����ֹ�
            02  SC07-TEL    PIC X(30).
      *����Ʒ��ֹ�
            02  SC07-KOHBANGO   PIC X(2).
            02  SC07-ZAIKEI
                    PIC X(2)    OCCURS  3 TIMES.
      *ʡ�������ô���ֹ�
            02  SC07-KOHFTNJANUM.
              03  SC07-KOHFTNJANUM1   PIC X(2).
              03  SC07-KOHFTNJANUM2   PIC X(2).
              03  SC07-KOHFTNJANUM3   PIC X(2).
              03  SC07-KOHFTNJANUM4   PIC X(2).
      *��Į¼�ֹ�
            02  SC07-TOWNNUM.
              03  SC07-TOWNNUM1   PIC X(2).
              03  SC07-TOWNNUM2   PIC X(2).
              03  SC07-TOWNNUM3   PIC X(2).
              03  SC07-TOWNNUM4   PIC X(2).
      *ʡ�������ô��̾
            02  SC07-TOWNNAME   PIC X(16).
      *����ǯ����
            02  SC07-SEIYMD PIC X(22).
      *���
            02  SC07-GOUKEI-TBL         OCCURS  2 TIMES.
      *��׷��
              03  SC07-KENSU  PIC X(5).
      *�������
              03  SC07-GNISSU PIC X(5).
      *�������
              03  SC07-GTENSU PIC X(9).
      *��װ�����ô���
              03  SC07-GRJNFTN    PIC X(9).
      *���02Ĺ���
              03  SC07-GTYOKI PIC X(2).
      *��׸���Ʒ�
            02  SC07-KOHBG.
      *��׸���ƷǷ��
              03  SC07-BKENSU PIC X(5).
      *��׸���Ʒ�����
              03  SC07-BTENSU PIC X(9).
      *��׸���Ʒ���ô���
              03  SC07-BRJNFTN    PIC X(9).
            02  SC07-KOHZK-TBL          OCCURS  3 TIMES.
              03  SC07-ZKENSU PIC X(5).
              03  SC07-ZTENSU PIC X(9).
              03  SC07-ZRJFTN PIC X(9).
      *���ŵ��𥳡���
            02  SC07-HOSPCD.
              03  SC07-HOSPCD1    PIC X(2).
              03  SC07-HOSPCD2    PIC X(2).
              03  SC07-HOSPCD3    PIC X(2).
              03  SC07-HOSPCD4    PIC X(2).
              03  SC07-HOSPCD5    PIC X(2).
              03  SC07-HOSPCD6    PIC X(2).
              03  SC07-HOSPCD7    PIC X(2).
      *������ʬ
            02  SC07-KOBETU-TBL         OCCURS  14 TIMES.
      *ʡ����ż�����ֹ�
              03  SC07-JKYSNUM    PIC X(7).
      *ʡ������оݼԻ�̾
              03  SC07-NAME   PIC X(18).
      *�ݸ����ֹ�
              03  SC07-HKNJANUM   PIC X(18).
      *���ݸ��Ծڵ����ֹ�
              03  SC07-KIGONUM    PIC X(36).
      *Ϸ�Ͱ��ż�����ֹ�
              03  SC07-RJNNUM PIC X(7).
      *������ô���ֹ棱
              03  SC07-KOH1FTNJANUM   PIC X(8).
      *���������ֹ棱
              03  SC07-KOH1JKYJANUM   PIC X(7).
      *����ǯ��
              03  SC07-SRYYM  PIC X(4).
      *�ܲ�����
              03  SC07-NYUGAIKBN  PIC X(1).
      *��ǯ����
              03  SC07-BIRTHDAY   PIC X(9).
      *���ճ��
              03  SC07-KYUFURATE  PIC X(1).
      *����Ĺ
              03  SC07-TYOKI  PIC X(2).
      *����
              03  SC07-ZAITAKU    PIC X(1).
      *����
              03  SC07-NISSU  PIC X(2).
      *�ݸ�����
              03  SC07-TENSU  PIC X(7).
      *��������
              03  SC07-KOH1TEN    PIC X(7).
      *����ʻ�ѻ�ʡ������
              03  SC07-FKSTEN PIC X(7).
      *������ô���
              03  SC07-FTNMONEY   PIC X(7).
      *���񣱰�����ô���
              03  SC07-KOH1FTN    PIC X(7).
      *������
              03  SC07-KOUJO  PIC X(6).
