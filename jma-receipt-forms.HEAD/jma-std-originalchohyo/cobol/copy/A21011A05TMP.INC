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
      *                           11/01/13  ������Ǥ��ܲȶ�ʬ�ѹ����б�
      * 01.00.02     @@@@-���    11/10/18  ������Ǥ���ô���ֹ��ѹ����б�
      * 01.00.03     @@@@-���    16/03/11  ����ե�������õ�������ɲá�07Ϸʻ��08Ϸ���
      * 01.00.04     @@@@-���    16/05/16  ����ե������Ŭ�ѳ���ǯ�������ɲ�
      ********************************************************************
      * 01  A05TMP-REC.
           03  A05TMP-KEY.
      *���϶�ʬ
               05  A05TMP-SYUTURYOKU      PIC  X(01).
      *�����ֹ�
               05  A05TMP-PTNUM           PIC  X(20).
      *������ʬ
               05  A05TMP-NYUGAIKBN       PIC  X(01).
      *���ճ��
               05  A05TMP-KYURATE         PIC  9(03).
      *����ǯ��
               05  A05TMP-SRYYM           PIC  X(06).
      *�ݸ����ֹ�
               05  A05TMP-HKNJANUM        PIC  X(08).
      *�ܿ͡���²��ʬ
               05  A05TMP-HONKZKKBN       PIC  X(01).
      *��ô���ֹ�
               05  A05TMP-KOHFTNNUM1      PIC  X(08).
               05  A05TMP-KOHFTNNUM2      PIC  X(08).
      *�õ������07Ϸʻ��08Ϸ���
               05  A05TMP-TOKKI           PIC  X(02).
      *Ŭ�ѳ���ǯ����
               05  A05TMP-TEKSTYMD        PIC  X(08).
      *���Ի�̾
           03  A05TMP-PTNAME              PIC  X(30).
      *���ݸ��ֹ�
           03  A05TMP-HKNNUM              PIC  X(03).
      *Ϸ���ݸ��ֹ�
           03  A05TMP-RJNHKNNUM           PIC  X(03).
      *�����ֹ棱
           03  A05TMP-KOHNUM1             PIC  X(03).
      *�����ֹ棲
           03  A05TMP-KOHNUM2             PIC  X(03).
      *����
           03  A05TMP-NISSU               PIC  9(02).
      *����
           03  A05TMP-TENSU               PIC  9(08).
      *���ް�����ô��
           03  A05TMP-YKZFTN              PIC  9(05).
      *������ô��
           03  A05TMP-ITBFTN              PIC  9(09).
      *��������
           03  A05TMP-SNISSU              PIC  9(02).
      *����������
           03  A05TMP-SKINGK              PIC  9(09).
      *������ô��
           03  A05TMP-SFTNGK              PIC  9(09).