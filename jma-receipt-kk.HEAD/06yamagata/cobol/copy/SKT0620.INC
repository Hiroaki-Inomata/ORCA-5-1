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
      *  01.00.01    ����    2009/11/05  ��������������б�
      *  01.00.02    ����    2012/05/10  Ģɼ�ͼ����ѹ�
      *  01.00.03    ���    2018/10/12  �������򥳥��Ⱥ��
      ********************************************************************
        01  SKT0620.
            03  SKT0620-SRYYM          PIC X(16).
            03  SKT0620-SEIYMD         PIC X(22).
            03  SKT0620-HOSPCD         PIC X(07).
            03  SKT0620-HKNJANAME      PIC X(26).
            03  SKT0620-HKNJANUM       PIC X(06).
            03  SKT0620-ADRS           PIC X(100).
            03  SKT0620-HOSPNAME       PIC X(100).
            03  SKT0620-TEL            PIC X(15).
            03  SKT0620-FAX            PIC X(15).
            03  SKT0620-KAISETU        PIC X(40).
            03  SKT0620-NO             PIC X(04).
            03  SKT0620-TOKUBETU       PIC X(10).
            03  SKT0620-SEIKYU-TBL      OCCURS  13 TIMES.
                05  SKT0620-KENSU        PIC X(05).
                05  SKT0620-NISSU        PIC X(05).
                05  SKT0620-TENSU        PIC X(09).
                05  SKT0620-ITBFTN       PIC X(09).
                05  SKT0620-SKENSU       PIC X(05).
                05  SKT0620-SNISSU       PIC X(05).
                05  SKT0620-SKINGK       PIC X(09).
                05  SKT0620-SFTNGK       PIC X(09).
            03  SKT0620-KOHI-TBL      OCCURS  12 TIMES.
                05  SKT0620-HBTNUM       PIC X(02).
                05  SKT0620-KKENSU       PIC X(05).
                05  SKT0620-KNISSU       PIC X(05).
                05  SKT0620-KTENSU       PIC X(09).
                05  SKT0620-KITBFTN      PIC X(09).
                05  SKT0620-KSKENSU      PIC X(05).
                05  SKT0620-KSNISSU      PIC X(05).
                05  SKT0620-KSKINGK      PIC X(09).
                05  SKT0620-KSFTNGK      PIC X(09).
