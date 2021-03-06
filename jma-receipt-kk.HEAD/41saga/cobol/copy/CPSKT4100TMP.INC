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
      *    �����������     COPY (CPSKT4100TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2009/05/15  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.02    ����    2019/02/07  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT4100TMP-REC.
           03  SKT4100TMP-KEY.
               05  SKT4100TMP-SRYYM                 PIC  X(06).
               05  SKT4100TMP-SYUTURYOKU            PIC  9(01).
               05  SKT4100TMP-HKNJANUM              PIC  X(08).
               05  SKT4100TMP-HKNJANAME             PIC  X(08).
               05  SKT4100TMP-ZAITAKU               PIC  X(01).
               05  SKT4100TMP-KYURATE               PIC  9(02).
           03  SKT4100TMP-HKN-TBL   OCCURS  3.
               05  SKT4100TMP-NYUGAI-TBL    OCCURS  2.
                   07  SKT4100TMP-KENSU             PIC  9(05).
                   07  SKT4100TMP-NISSU             PIC  9(05).
                   07  SKT4100TMP-TENSU             PIC  9(10).
           03  SKT4100TMP-KENNAI                    PIC  9(06).
           03  SKT4100TMP-KENGAI                    PIC  9(06).
           03  SKT4100TMP-KOUKENGAI                 PIC  9(06).
           03  SKT4100TMP-KOUKENNAI                 PIC  9(06).
