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
      *  01.00.01    ����      2007/10/05   �����ƥ�����ޥ�����1901�פ�
      *                                     ���ŵ��ؾ���򻲾�
      *  01.00.02    ����      2010/10/01   ��������������б�
      *  01.00.03    ����      2018/11/07   �������򥳥��Ⱥ��
      ************************************************************
        01  SKT1800.
            03  SKT1800-SRYYM           PIC X(16).
            03  SKT1800-SEIYMD          PIC X(22).
            03  SKT1800-HOSPCDN         PIC X(20).
            03  SKT1800-ADRS            PIC X(100).
            03  SKT1800-HOSPNAME        PIC X(100).
            03  SKT1800-TEL             PIC X(15).
            03  SKT1800-TOKUBETU        PIC X(10).
            03  SKT1800-KEN-TBL         OCCURS  2 TIMES.
                05  SKT1800-SEIKYU-TBL      OCCURS  10 TIMES.
                    07  SKT1800-KENSU   PIC X(6).
                    07  SKT1800-NISSU   PIC X(6).
                    07  SKT1800-TENSU   PIC X(10).
                05  SKT1800-HKNJASU     PIC X(03).
            03  SKT1800-KOH-N-TBL       OCCURS  6 TIMES.
                05  SKT1800-KOH-N-NUM   PIC X(02).
                05  SKT1800-KOH-N-KENSU PIC X(06).
                05  SKT1800-KOH-N-NISSU PIC X(06).
                05  SKT1800-KOH-N-TENSU PIC X(10).
            03  SKT1800-KOH-G-TBL       OCCURS  2 TIMES.
                05  SKT1800-KOH-G-NUM   PIC X(02).
                05  SKT1800-KOH-G-KENSU PIC X(06).
                05  SKT1800-KOH-G-NISSU PIC X(06).
                05  SKT1800-KOH-G-TENSU PIC X(10).
