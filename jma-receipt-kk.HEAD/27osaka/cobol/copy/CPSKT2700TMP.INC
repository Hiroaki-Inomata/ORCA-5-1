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
      *    �����������     COPY (CPSKT2700TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SKT2700TMP-REC.
           03  SKT2700TMP-KEY.
               05  SKT2700TMP-SYUTURYOKU            PIC  9(01).
               05  SKT2700TMP-HKNJANUM              PIC  X(06).
               05  SKT2700TMP-SRYYM                 PIC  9(06).
           03  SKT2700TMP-KYURATE                   PIC  9(02).
           03  SKT2700TMP-AREA  OCCURS  31.
               05  SKT2700TMP-KENSU                 PIC  9(06).
               05  SKT2700TMP-TENSU                 PIC  9(10).
               05  SKT2700TMP-YKZFTN                PIC  9(10).
               05  SKT2700TMP-FTNMONEY              PIC  9(10).
               05  SKT2700TMP-S-KENSU               PIC  9(06).
               05  SKT2700TMP-S-KINGAKU             PIC  9(10).
               05  SKT2700TMP-S-FTNGAKU             PIC  9(10).
               05  SKT2700TMP-BIKO                  PIC  9(02).
           03  SKT2700TMP-TAKOH                     PIC  9(03).
