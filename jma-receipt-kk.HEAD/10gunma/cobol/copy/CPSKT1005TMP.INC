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
      *    ����ɽ����ե�����                                    *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2011/04/08  ����������ʿ�β��Ͽ��б�
      *  01.00.02    ���    2018/10/02  ���ץ����Ȥ���
      ************************************************************
       01  SKT1005TMP-REC.
           03  SKT1005TMP-KEY.
               05  SKT1005TMP-SYUTURYOKU     PIC  X(01).
               05  SKT1005TMP-HKNJANUM       PIC  X(08).
               05  SKT1005TMP-ZAITAKU        PIC  X(01).
               05  SKT1005TMP-RATEFLG        PIC  9(01).
               05  SKT1005TMP-KYURATE        PIC  9(02).
               05  SKT1005TMP-ADRS           PIC  X(40).
           03  SKT1005TMP-NYUGAI-TBL  OCCURS  2.
               05  SKT1005TMP-KENSU          PIC  9(06).
               05  SKT1005TMP-TENSU          PIC  9(10).
      *