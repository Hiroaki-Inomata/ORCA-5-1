      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ���         19/06/10  Ģɼ�ͼ����ѹ�
      *                                     �����ͼ����ɲ�
      ********************************************************************
       01  SC03.
      *����ǯ��
              03  SC03-PRTYM            PIC  X(16).
      *��Į¼�ֹ�
              03  SC03-TOWNNUM          PIC  X(8).
      *������ô���ֹ�
              03  SC03-KOHFTNJANUM      PIC  X(8).
      *���ֹ�
              03  SC03-PREFNUM.
                  05 SC03-PREFNUM1      PIC  X(02).
                  05 SC03-PREFNUM2      PIC  X(02).
      *���ŵ��إ�����
              03  SC03-HOSPCD.
                  05 SC03-HOSPCD1       PIC  X(02).
                  05 SC03-HOSPCD2       PIC  X(02).
                  05 SC03-HOSPCD3       PIC  X(02).
                  05 SC03-HOSPCD4       PIC  X(02).
                  05 SC03-HOSPCD5       PIC  X(02).
                  05 SC03-HOSPCD6       PIC  X(02).
                  05 SC03-HOSPCD7       PIC  X(02).
      *������ʬɽ�������
              03  SC03-TENHYOKBN1       PIC  X(2).
      *������ʬɽ��������
              03  SC03-TENHYOKBN2       PIC  X(2).
      *������ʬɽ����Ĵ��
              03  SC03-TENHYOKBN3       PIC  X(2).
      *��Į¼̾
              03  SC03-TOWNNAME         PIC  X(14).
      *����ǯ����
              03  SC03-SEIYMD           PIC  X(22).
      *����
              03  SC03-ADRS             PIC  X(100).
      *���ŵ���̾
              03  SC03-HOSPNAME         PIC  X(100).
      *������̾
              03  SC03-KANRINAME        PIC  X(24).
      *�����ֹ�
              03  SC03-TEL              PIC  X(30).
      *���
              03  SC03-GOUKEI-TBL       OCCURS  2.
                  05  SC03-KENSU        PIC  X(5).
                  05  SC03-GNISSU       PIC  X(5).
                  05  SC03-GTENSU       PIC  X(9).
                  05  SC03-GKYUFU       PIC  X(9).
                  05  SC03-GTYO         PIC  X(2).
                  05  SC03-GKOUJO       PIC  X(9).
      *����"90"���
              03  SC03-KOHISAIKEI90-TBL.
                  05  SC03-GKOHNUM           PIC  X(2).
                  05  SC03-GKKENSU           PIC  X(5).
                  05  SC03-GKTENSU           PIC  X(9).
                  05  SC03-GKFTNKG           PIC  X(9).
      *����Ʒǹ��
              03  SC03-KOHISAIKEI-TBL   OCCURS  3.
                  05  SC03-KOHNUM           PIC  X(2).
                  05  SC03-KKENSU           PIC  X(5).
                  05  SC03-KTENSU           PIC  X(9).
                  05  SC03-KFTNKG           PIC  X(9).
      *�����
              03  SC03-GKENSU           PIC  X(5).
      *����
              03  SC03-KOBETU-TBL       OCCURS  14.
                  05  SC03-JKYSNUM      PIC  X(7).
                  05  SC03-NAME	        PIC  X(18).
                  05  SC03-HKNJANUM     PIC  X(8).
                  05  SC03-KIGONUM      PIC  X(36).
                  05  SC03-KOH1FTNJANUM  PIC  X(8).
                  05  SC03-KOH1JKYJANUM  PIC  X(7).
                  05  SC03-BIRTHDAY     PIC  X(9).
                  05  SC03-SRYYM        PIC  X(4).
                  05  SC03-NYUGAIKBN    PIC  X(1).
                  05  SC03-KYUFURATE    PIC  X(1).
                  05  SC03-TOKKI        PIC  X(2).
                  05  SC03-TYOKI        PIC  X(2).
                  05  SC03-NISSU        PIC  X(2).
                  05  SC03-TENSU        PIC  X(7).
                  05  SC03-KOH1TEN      PIC  X(7).
                  05  SC03-NYUYOJITEN   PIC  X(7).
                  05  SC03-FTNMONEY     PIC  X(7).
                  05  SC03-KOH1FTNKG    PIC  X(7).
                  05  SC03-KOUJO        PIC  X(7).
