      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������    ����         ����
      *  01.00.01    ����      2020/02/12   �������򥳥��Ⱥ��
      ********************************************************************
       01  SC07.
      *����ǯ����
              03  SC07-SEIYMD           PIC  X(22).
      *ɽ��ǯ��
              03  SC07-PRTYM            PIC  X(16).
      *����
              03  SC07-ADRS             PIC  X(100).
      *���ŵ���̾
              03  SC07-HOSPNAME         PIC  X(100).
      *������̾
              03  SC07-KAISETUNAME      PIC  X(40).
      *�����ֹ�
              03  SC07-TEL              PIC  X(15).
      *���ŵ��إ�����
              03  SC07-HOSPCD           PIC  X(14).
      *��׶��
              03  SC07-GKINGAKU         PIC  X(22).
      *�����
              03  SC07-GKENSU           PIC  X(10).
      *���ڡ�����
              03  SC07-GPAGE            PIC  X(06).
      *�ڡ�����
              03  SC07-PAGE             PIC  X(06).
      *��Į¼�ֹ�
              03  SC07-CITYNAME         PIC  X(20).
      *���ťơ��֥�
              03  SC07-SRYTBL           OCCURS  25.
      *������ֹ�
                  05  SC07-JKYSNUM      PIC  X(20).
      *���ǼԻ�̾
                  05  SC07-NAME         PIC  X(20).
      *������
                  05  SC07-TENSU        PIC  X(07).
      *ɸ����ô��
                  05  SC07-HJNFTN       PIC  X(06).
      *�������������
                  05  SC07-KESE         PIC  X(07).
      *������
                  05  SC07-KYURATE1     PIC  X(02).
                  05  SC07-KYURATE2     PIC  X(02).
      *��Į¼��ô��
                  05  SC07-KOHFTN       PIC  X(09).
      *��ץơ��֥�
              03  SC07-SUM-TBL.
      *��ס����
                  05  SC07-SKENSU       PIC  X(02).
      *��ס�����
                  05  SC07-GTENSU       PIC  X(09).
      *��ס�ɸ����ô��
                  05  SC07-GHJNFTN      PIC  X(07).
      *��ס��������������
                  05  SC07-GKESE        PIC  X(09).
      *��ס���Į¼��ô��
                  05  SC07-GKOHFTN      PIC  X(09).
      *������
              03  SC07-BIKO-TBL OCCURS 25.
                  05  SC07-BIKO1        PIC  X(18).
                  05  SC07-BIKO2        PIC  X(18).
                  05  SC07-BIKO3        PIC  X(18).
