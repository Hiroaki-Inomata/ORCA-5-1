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
      *    �����������     COPY (CPSKT1900TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         08/05/27  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.02    ���         18/11/12  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT1900TMP-REC.
           03  SKT1900TMP-KEY.
               05  SKT1900TMP-TYOHYOKBN             PIC  9(01).
               05  SKT1900TMP-SYUTURYOKU            PIC  9(01).
               05  SKT1900TMP-HKNJANUM              PIC  X(06).
               05  SKT1900TMP-KYURATE               PIC  9(03).
           03  SKT1900TMP-HKN-AREA  OCCURS  9.
               05  SKT1900TMP-NYUGAI-SKJ-TBL  OCCURS  3.
                   07  SKT1900TMP-KENSU             PIC  9(05).
                   07  SKT1900TMP-NISSU             PIC  9(05).
                   07  SKT1900TMP-TENSU             PIC  9(11).
