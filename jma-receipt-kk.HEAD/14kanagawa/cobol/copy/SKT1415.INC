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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����      2008/04/26   ��������(���������б�)
      *  01.00.02    ����      2015/01/30   ʿ������ǯ��������б�
      ************************************************************
        01  SKT1415.
            03  SKT1415-SRYYM           PIC X(16).
            03  SKT1415-SEIYMD          PIC X(22).
            03  SKT1415-HOSPCD          PIC X(40).
            03  SKT1415-HKNJANUM        PIC X(12).
            03  SKT1415-ADRS            PIC X(100).
            03  SKT1415-HOSPNAME        PIC X(100).
            03  SKT1415-KAISETUNAME     PIC X(40).
            03  SKT1415-ZAI1            PIC X(2).
            03  SKT1415-ZAI2            PIC X(2).
            03  SKT1415-NO              PIC X(4).
            03  SKT1415-SEIKYU-TB       OCCURS  2 TIMES.
              05  SKT1415-NYUGAI-TBL    OCCURS  2 TIMES.
                07  SKT1415-KENSU       PIC X(5).
                07  SKT1415-NISSU       PIC X(5).
                07  SKT1415-TENSU       PIC X(9).
                07  SKT1415-ITBFTN      PIC X(9).
              05  SKT1415-S-KENSU       PIC X(5).
              05  SKT1415-S-NISSU       PIC X(5).
              05  SKT1415-S-KINGK       PIC X(9).
              05  SKT1415-S-FTNGK       PIC X(9).
            03  SKT1415-KOUHI-TBL       OCCURS  25 TIMES.
              05  SKT1415-KKENSU        PIC X(5).