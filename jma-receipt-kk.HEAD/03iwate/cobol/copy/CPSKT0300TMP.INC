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
      ************************************************************
      *    �����������     COPY (CPSKT0300TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01     ����     2006/08/29   ������̾(����)�ɲ�63��64
      *  01.00.02     ����     2010/03/11   ������̾(����)�ɲ�8��9
      *  01.00.03     ����     2011/04/04   ����������ʿ�β��Ͽ��б�
      *  01.00.04     ���     2018/10/11   �������򥳥��Ⱥ��
      ************************************************************
       01  SKT0300TMP-REC.
           03  SKT0300TMP-KEY.
      *���ȶ�ʬ
               05  SKT0300TMP-GENMEN               PIC  X(01).
      *����ơ��֥�
           03  SKT0300TMP-SEIKYU-TBL     OCCURS  12.
               05  SKT0300TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT0300TMP-PREF-TBL  OCCURS  2.
                       09  SKT0300TMP-KENSU        PIC  9(06).
                       09  SKT0300TMP-NISSU        PIC  9(06).
                       09  SKT0300TMP-TENSU        PIC  9(10).
      *����ơ��֥�
           03  SKT0300TMP-ZAI-TBL      OCCURS  6.
               05  SKT0300TMP-ZPREF-TBL  OCCURS  2.
                   07  SKT0300TMP-ZKENSU           PIC  9(06).
                   07  SKT0300TMP-ZNISSU           PIC  9(06).
                   07  SKT0300TMP-ZTENSU           PIC  9(10).
      *����������ơ��֥�
           03  SKT0300TMP-SKJ-TBL      OCCURS  3.
               05  SKT0300TMP-SPREF-TBL  OCCURS  2.
                   07  SKT0300TMP-SKENSU           PIC  9(06).
                   07  SKT0300TMP-SNISSU           PIC  9(06).
                   07  SKT0300TMP-STENSU           PIC  9(10).
      *��ץơ��֥�
           03  SKT0300TMP-GOKEI-TBL      OCCURS  5.
               05  SKT0300TMP-GPREF-TBL  OCCURS  2.
                   07  SKT0300TMP-GKENSU           PIC  9(06).
                   07  SKT0300TMP-GNISSU           PIC  9(06).
                   07  SKT0300TMP-GTENSU           PIC  9(10).
      *���ճ��
           03  SKT0300TMP-KYURATE                  PIC  9(02).
      *�����ݸ���
           03  SKT0300TMP-NHKNJA-TBL  OCCURS  40.
               05  SKT0300TMP-KENNAI-FLG           PIC  9(01).
      *��̾
           03  SKT0300TMP-KENMEI-TBL  OCCURS  50.
               05  SKT0300TMP-KENMEI               PIC  9(02).
      *�����ݸ���
           03  SKT0300TMP-GHKNJA-TBL   OCCURS  9.
               05  SKT0300TMP-KENGAI-KENSU         PIC  9(02).
      *�����ݸ��ԣ�
           03  SKT0300TMP-GHKNJA-TBL2  OCCURS  50.
               05  SKT0300TMP-KENGAI-KENSU2         PIC  9(02).
      *����ݸ���
           03  SKT0300TMP-GOKEIHKNJA-TBL  OCCURS  2.
               05  SKT0300TMP-GOKEIHKNJA           PIC  9(03).
