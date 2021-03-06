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
      * 01.00.01     @@@@-���    11/01/07  ������������
      ********************************************************************
      01  A05.
      *    �ڡ�����
              03  A05-PAGE          PIC  X(03).
      *    �о�ǯ��
              03  A05-SKYYM         PIC  X(18).
      *    ����ǯ����
              03  A05-SYSYMD        PIC  X(22).
      *    ���顼��å�����
              03  A05-JOBMSG        PIC  X(60).
      *    ���϶�ʬ
              03  A05-KBN           PIC  X(20).
      *    �����ֹ�
              03  A05-PTNUM-TBL.
                  05  A05-PTNUM     PIC  X(08) OCCURS 20.
      *    ���Ի�̾
              03  A05-NAME-TBL.
                  05  A05-NAME      PIC  X(30) OCCURS 20.
      *    �ݸ����ֹ�
              03  A05-HKNJANUM-TBL.
                  05  A05-HKNJANUM  PIC  X(16) OCCURS 20.
      *    ������ʬ
              03  A05-NYUGAI-TBL.
                  05  A05-NYUGAI    PIC  X(04) OCCURS 20.
      *    �ܿ͡���²��ʬ
              03  A05-HONKZKKBN-TBL.
                  05  A05-HONKZKKBN PIC  X(02) OCCURS 20.
      *    �ݸ��ֹ桦Ϸ���ݸ��ֹ�
              03  A05-HKNNUM-TBL.
                  05  A05-HKN-RJN-TBL OCCURS 20.
                      07  A05-HKNNUM        PIC  X(06).
                      07  A05-RJNHKNNUM     PIC  X(06).
      *    ���ճ��
              03  A05-KYURATE-TBL.
                  05  A05-KYURATE   PIC  X(06) OCCURS 20.
      *    �����ֹ棱
              03  A05-KOHNUM1-TBL.
                  05  A05-KOHNUM1   PIC  X(06) OCCURS 20.
      *    �����ֹ棲
              03  A05-KOHNUM2-TBL.
                  05  A05-KOHNUM2   PIC  X(06) OCCURS 20.
      *    ���ż�����
              03  A05-NISSU-TBL.
                  05  A05-NISSU     PIC  X(08) OCCURS 20.
      *    ����
              03  A05-TENSU-TBL.
                  05  A05-TENSU     PIC  X(18) OCCURS 20.
      *    ���ް�����ô��
              03  A05-YKZFTN-TBL.
                  05  A05-YKZFTN    PIC  X(14) OCCURS 20.
      *    ������ô��
              03  A05-ITBFTN-TBL.
                  05  A05-ITBFTN    PIC  X(14) OCCURS 20.
      *    ��������
              03  A05-SNISSU-TBL.
                  05  A05-SNISSU    PIC  X(08) OCCURS 20.
      *    �������
              03  A05-SKINGK-TBL.
                  05  A05-SKINGK    PIC  X(18) OCCURS 20.
      *    ����ɸ����ô��
              03  A05-SFTNGK-TBL.
                  05  A05-SFTNGK    PIC  X(14) OCCURS 20.
      *    ����ǯ��
              03  A05-SRYYM-TBL.
                  05  A05-SRYYM     PIC  X(16) OCCURS 20.
