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
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2013/10/08  ��������
      *  01.00.02    ����    2017/01/17  Ģɼ�ͼ����ѹ�
      *  01.00.03    ���    2018/08/06  �㳲��499�ˤ������б�
      *  01.00.04    ���    2018/11/01  �������򥳥��Ⱥ��
      ******************************************************************
       01  SC09.
           03  SC09-SEIYMD                  PIC  X(22).
           03  SC09-HOSPCD                  PIC  X(07).
           03  SC09-ADRS                    PIC  X(100).
           03  SC09-HOSPNAME                PIC  X(100).
           03  SC09-TEL                     PIC  X(15).
           03  SC09-SRYYM                   PIC  X(16).
           03  SC09-SEIKYU-TBL OCCURS 4.
               05  SC09-KENSU               PIC  X(05).
               05  SC09-SEIKYUGK            PIC  X(09).
