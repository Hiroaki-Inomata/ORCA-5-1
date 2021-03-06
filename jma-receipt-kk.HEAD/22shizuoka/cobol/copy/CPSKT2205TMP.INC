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
      *    ����ɽ����ե�����           CPSKT2205TMP.INC           *
      *  01.00.01    ��ƣ    08/04/26    Ģɼ�ͼ���ȼ������
      *  01.00.02    ����    08/06/23    �����˿���ǯ���ɲ�
      *  01.00.03    ����    18/09/10    �������򥳥��Ⱥ��
      ************************************************************
       01  SKT2205TMP-REC.
           03  SKT2205TMP-KEY.
               05  SKT2205TMP-ZAITAKU        PIC  X(01).
               05  SKT2205TMP-KBN            PIC  9(01).
               05  SKT2205TMP-KUMIAI         PIC  9(01).
               05  SKT2205TMP-HKNJANUM       PIC  X(02).
               05  SKT2205TMP-SRYYM          PIC  9(06).
           03  SKT2205TMP-DATA-TBL       OCCURS 26.
               05  SKT2205TMP-KENSU          PIC  9(06).
               05  SKT2205TMP-NISSU          PIC  9(10).
               05  SKT2205TMP-TENSU          PIC  9(10).
