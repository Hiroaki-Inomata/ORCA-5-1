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
      *    �����������     COPY (CPKOH4300TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2008/07/04  H20ǯ4��������ϵ��ͼ��ǰ���
      *  01.00.02    ���    2018/09/05  ���ץ����Ȥ���
      ************************************************************
       01  KOH4300TMP-REC.
           03  KOH4300TMP-KEY.
               05  KOH4300TMP-KBN                   PIC  9(01).
               05  KOH4300TMP-SRYYM                 PIC  9(06).
               05  KOH4300TMP-KOHNUM                PIC  X(03).
           03  KOH4300TMP-KOH-TBL.
               05  KOH4300TMP-NYUGAIKBN  OCCURS 2.
                   07  KOH4300TMP-KENSU             PIC  9(04).
                   07  KOH4300TMP-TENSU             PIC  9(10).
               05  KOH4300TMP-SKENSU                PIC  9(04).
               05  KOH4300TMP-SKINGK                PIC  9(10).
               05  KOH4300TMP-SFTNGK                PIC  9(10).
