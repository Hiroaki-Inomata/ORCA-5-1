        01  SC33.
      *�ڡ�����
            02  SC33-PAGE   PIC X(3).
      *������
            02  SC33-SEIYMD-TBL.
              03  SC33-SEIYY  PIC X(4).
              03  SC33-SEIMM  PIC X(4).
              03  SC33-SEIDD  PIC X(4).
      *���Ŷ�ʬ
            02  SC33-SRKBN
                    PIC X(2)    OCCURS  6 TIMES.
      *���ŵ��ؽ���
            02  SC33-HOSPADRS   PIC X(100).
      *���ŵ���̾��
            02  SC33-HOSPNAME   PIC X(100).
      *��ȯ�Ի�̾
            02  SC33-DAIHYOUNAME    PIC X(40).
      *���ŵ�������
            02  SC33-HOSPTEL    PIC X(15).
      *���ŵ��إ�����
            02  SC33-HOSPCD-TBL.
              03  SC33-HOSPCD1    PIC X(2).
              03  SC33-HOSPCD2    PIC X(2).
              03  SC33-HOSPCD3    PIC X(2).
              03  SC33-HOSPCD4    PIC X(2).
              03  SC33-HOSPCD5    PIC X(2).
              03  SC33-HOSPCD6    PIC X(2).
              03  SC33-HOSPCD7    PIC X(2).
              03  SC33-HOSPCD8    PIC X(2).
      *����ǯ��
            02  SC33-SRYM1-TBL.
              03  SC33-SRYY1  PIC X(4).
              03  SC33-SRMM1  PIC X(4).
      *������ֹ�
            02  SC33-JKYSNUM
                    PIC X(14)   OCCURS  10 TIMES.
      *���Ǽ�̾
            02  SC33-KATAKANA
                    PIC X(30)   OCCURS  10 TIMES.
      *��ǯ����
            02  SC33-BRITHDAY-TBL       OCCURS  10 TIMES.
              03  SC33-BRITHH PIC X(2).
              03  SC33-BRITHYY    PIC X(4).
              03  SC33-BRITHMM    PIC X(4).
              03  SC33-BRITHDD    PIC X(4).
      *�������ݸ�
            02  SC33-HKKBN-TBL          OCCURS  10 TIMES.
              03  SC33-HKKBN
                      PIC X(2)    OCCURS  4 TIMES.
            02  SC33-HKNAME
                    PIC X(20)   OCCURS  10 TIMES.
      *�ݸ����Ű�����ô��
            02  SC33-HKNFTN
                    PIC X(7)    OCCURS  10 TIMES.
      *����ǯ��
            02  SC33-SRYM-TBL           OCCURS  10 TIMES.
              03  SC33-SRYY   PIC X(2).
              03  SC33-SRMM   PIC X(2).
      *���Ŷ�ʬ
            02  SC33-SRKBN1
                    PIC X(1)    OCCURS  10 TIMES.
      *������ʬ
            02  SC33-NGKBN-TBL          OCCURS  10 TIMES.
              03  SC33-NGKBN
                      PIC X(2)    OCCURS  2 TIMES.
      *����
            02  SC33-NITISU
                    PIC X(2)    OCCURS  10 TIMES.
      *�ݸ�����������
            02  SC33-HKSRYTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  SC33-HOUBETU
                    PIC X(2)    OCCURS  10 TIMES.
            02  SC33-TAHOUTEN
                    PIC X(7)    OCCURS  10 TIMES.
      *��ô���
            02  SC33-FTNWARIAI
                    PIC X(1)    OCCURS  10 TIMES.
      *����������������
            02  SC33-SKJFTN
                    PIC X(7)    OCCURS  10 TIMES.
            02  SC33-SANTEIKAI
                    PIC X(3)    OCCURS  10 TIMES.
      *����
            02  SC33-SYOUKEI-TBL.
              03  SC33-SK-HKNFTN  PIC X(9).
              03  SC33-SK-SKJFTN  PIC X(9).
      *���
            02  SC33-GAOKEI-TBL.
              03  SC33-GK-HKNFTN  PIC X(9).
              03  SC33-GK-SKJFTN  PIC X(9).
