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
      *    ����ɽ����ե�����           CPSKT2200TMP.INC           *
      *  01.00.01    ��ƣ    2008/04/26    Ģɼ�ͼ���ȼ������
      *  01.00.02    ����    2018/09/10    �������򥳥��Ⱥ��
      ************************************************************
       01  SKT2200TMP-REC.
           03  SKT2200TMP-KEY.
               05  SKT2200TMP-ZAITAKU        PIC  X(01).
               05  SKT2200TMP-KBN            PIC  9(01).
               05  SKT2200TMP-KUMIAI         PIC  9(01).
               05  SKT2200TMP-SRYYM          PIC  9(06).
               05  SKT2200TMP-HKNJANUM       PIC  X(06).
           03  SKT2200TMP-DATA-TBL       OCCURS 26.
               05  SKT2200TMP-KENSU          PIC  9(06).
               05  SKT2200TMP-NISSU          PIC  9(10).
               05  SKT2200TMP-TENSU          PIC  9(10).
