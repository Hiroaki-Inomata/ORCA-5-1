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
      *  ��������ɽ         COPY (CPSKT4500TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������   ����        ����
      *  01.00.01    Ĺ��     08/05/03    Ģɼ�ͼ���ȼ������
      *  01.00.02    ����     09/05/07    Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.03    ���     19/02/01    �������򥳥��Ⱥ��
      ************************************************************
       01  SKT4500TMP-REC.
           03  SKT4500TMP-KEY.
               05  SKT4500TMP-KENKBN                PIC  9(01).
               05  SKT4500TMP-RECEKA                PIC  X(02).
           03  SKT4500TMP-SEIKYU-TBL      OCCURS  6.
               05  SKT4500TMP-NYUGAIKBN   OCCURS  2.
                   07  SKT4500TMP-KENSU             PIC  9(06).
                   07  SKT4500TMP-TENSU             PIC  9(10).
           03  SKT4500TMP-GKENSU      PIC  9(06)   OCCURS  2 TIMES.
           03  SKT4500TMP-GTENSU      PIC  9(10)   OCCURS  2 TIMES.
