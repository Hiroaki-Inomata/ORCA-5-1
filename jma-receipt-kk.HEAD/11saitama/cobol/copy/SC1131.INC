        01  SC31.
      *�ݸ�����
            02  SC31-HKNMEI
                    PIC X(2)    OCCURS  3 TIMES.
      *�ڡ�����
            02  SC31-PAGE   PIC X(3).
      *���ڡ�����
            02  SC31-ZPAGE  PIC X(3).
      *��Ĺ̾
            02  SC31-SITYOMEI   PIC X(8).
      *��̾
            02  SC31-SITYOMEI1  PIC X(8).
      *����ǯ����
            02  SC31-SEIYMD     PIC X(22).
      *���ŵ���̾
            02  SC31-HOSPNAME   PIC X(100).
      *��ɽ��̾
            02  SC31-DAIHYOUNAME    PIC X(40).
      *������ֹ�
            02  SC31-JKYSNUM
                    PIC X(12)   OCCURS  8 TIMES.
      *����Ի�̾
            02  SC31-KATAKANA
                    PIC X(30)   OCCURS  8 TIMES.
      *��ǯ����
            02  SC31-BRITHDAY   OCCURS  8 TIMES.
              03 SC31-BRITHDAYH    PIC X(2).
              03 SC31-BRITHDAYY1   PIC X(2).
              03 SC31-BRITHDAYY2   PIC X(2).
              03 SC31-BRITHDAYM1   PIC X(2).
              03 SC31-BRITHDAYM2   PIC X(2).
              03 SC31-BRITHDAYD1   PIC X(2).
              03 SC31-BRITHDAYD2   PIC X(2).
      *����ǯ��
            02  SC31-SRYYM      OCCURS  8 TIMES.
              03 SC31-SRYYMH         PIC X(4).
              03 SC31-SRYYMY1        PIC X(2).
              03 SC31-SRYYMY2        PIC X(2).
              03 SC31-SRYYMM1        PIC X(2).
              03 SC31-SRYYMM2        PIC X(2).
      *��������
            02  SC31-NITISU-TBL         OCCURS  8 TIMES.
              03  SC31-NITISU1    PIC X(1).
              03  SC31-NITISU2    PIC X(1).
      *�ݸ�����������
            02  SC31-HKSRYTEN           OCCURS  8 TIMES.
              03  SC31-HKSRYTEN1  PIC X(1).
              03  SC31-HKSRYTEN2  PIC X(1).
              03  SC31-HKSRYTEN3  PIC X(1).
              03  SC31-HKSRYTEN4  PIC X(1).
              03  SC31-HKSRYTEN5  PIC X(1).
              03  SC31-HKSRYTEN6  PIC X(1).
      *¾ˡ����
            02  SC31-TAHOTENSU          OCCURS  8 TIMES.
              03  SC31-TAHO1  PIC X(1).
              03  SC31-TAHO2  PIC X(1).
              03  SC31-TAHO3  PIC X(1).
              03  SC31-TAHO4  PIC X(1).
      *�ݸ����Ű�����ô��
            02  SC31-HKNFTN     OCCURS  8 TIMES.
              03  SC31-HKNFTN1    PIC X(1).
              03  SC31-HKNFTN2    PIC X(1).
              03  SC31-HKNFTN3    PIC X(1).
              03  SC31-HKNFTN4    PIC X(1).
              03  SC31-HKNFTN5    PIC X(1).
      *����Լ�����ô��
            02  SC31-JKYSFTN    OCCURS  8 TIMES.
              03  SC31-JKYSFTN1   PIC X(1).
              03  SC31-JKYSFTN2   PIC X(1).
              03  SC31-JKYSFTN3   PIC X(1).
              03  SC31-JKYSFTN4   PIC X(1).
      *���ŵ��ء����������    
            02  SC31-SASHKNFTN  OCCURS  8 TIMES.
              03  SC31-SASHKNFTN1   PIC X(1).
              03  SC31-SASHKNFTN2   PIC X(1).
              03  SC31-SASHKNFTN3   PIC X(1).
              03  SC31-SASHKNFTN4   PIC X(1).
              03  SC31-SASHKNFTN5   PIC X(1).
      *���׷��
            02  SC31-SYKEI.
              03  SC31-SYKEI1 PIC X(1).
              03  SC31-SYKEI2 PIC X(1).
      *��׷��
            02  SC31-GOKEI.
              03  SC31-GOKEI1 PIC X(1).
              03  SC31-GOKEI2 PIC X(1).
              03  SC31-GOKEI3 PIC X(1).
              03  SC31-GOKEI4 PIC X(1).
      *���װ��ŵ��ء����������
            02  SC31-SASJKYSFTN.
              03  SC31-SASJKYSFTN1  PIC X(1).
              03  SC31-SASJKYSFTN2  PIC X(1).
              03  SC31-SASJKYSFTN3  PIC X(1).
              03  SC31-SASJKYSFTN4  PIC X(1).
              03  SC31-SASJKYSFTN5  PIC X(1).
              03  SC31-SASJKYSFTN6  PIC X(1).
      *��װ��ŵ��ء����������
            02  SC31-SAGHKNFTN.
              03  SC31-SAGHKNFTN1   PIC X(1).
              03  SC31-SAGHKNFTN2   PIC X(1).
              03  SC31-SAGHKNFTN3   PIC X(1).
              03  SC31-SAGHKNFTN4   PIC X(1).
              03  SC31-SAGHKNFTN5   PIC X(1).
              03  SC31-SAGHKNFTN6   PIC X(1).
              03  SC31-SAGHKNFTN7   PIC X(1).
              03  SC31-SAGHKNFTN8   PIC X(1). 
