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
      *  01.00.01    ����    2010/08/04  ��������������б�
      *  01.00.02    ����    2015/01/30  ʿ������ǯ��������б�
      ********************************************************************
       01  SKT1410.
      *    NO
           03  SKT1410-NO                PIC  X(04).
      *    ����ǯ��
           03  SKT1410-SRYYM             PIC  X(16).
      *    �ݸ����ֹ�
           03  SKT1410-HKNJANUM          PIC  X(12).
      *    ����ǯ����
           03  SKT1410-SEIYMD            PIC  X(22).
      *    ����
           03  SKT1410-ZAI               PIC  X(02).
           03  SKT1410-ZAIMARU           PIC  X(02).
      *    ���Ų�
           03  SKT1410-SRYKA             PIC  X(04).
      *    ���ŵ��إ�����
           03  SKT1410-HOSPCDN           PIC  X(20).
      *    ���ŵ��ؽ���
           03  SKT1410-ADRS              PIC  X(100).
      *    ���ŵ���̾
           03  SKT1410-HOSPNAME          PIC  X(100).
      *    ���߼�̾
           03  SKT1410-KAISETUNAME       PIC  X(40).
      *    �����ֹ�
           03  SKT1410-TEL               PIC  X(15).
      *=== ��������������б� by Yoshikawa(2010/08/04) start
      *    ����������
           03  SKT1410-TOKUBETU          PIC  X(10).
      *=== ��������������б� by Yoshikawa(2010/08/04) end
      *    ����ơ��֥�
           03  SKT1410-SEIKYU-TBL     OCCURS  15.
      *    �����ơ��֥�
               05  SKT1410-NYUGAI-TBL      OCCURS   2.
      *    ���
                   07  SKT1410-KENSU     PIC  X(05).
      *    ������
                   07  SKT1410-NISSU     PIC  X(05).
      *    ����
                   07  SKT1410-TENSU     PIC  X(09).
      *    ���ް�����ô��
      *             07  SKT1410-YKZFTN    PIC  X(07).
      *    ������ô��
                   07  SKT1410-ITBFTN    PIC  X(09).
      *    �������ܡ����
               05  SKT1410-S-KENSU       PIC  X(05).
      *    �������ܡ�����
               05  SKT1410-S-NISSU       PIC  X(05).
      *    �������ܡ����
               05  SKT1410-S-KINGAKU     PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT1410-S-FTNGAKU     PIC  X(09).
      *    ����ơ��֥�
           03  SKT1410-KOH-TBL        OCCURS   34.
      *    ���
               05  SKT1410-K-KENSU       PIC  X(03).
      *    ����
      *         05  SKT1410-K-TENSU       PIC  X(07).
      *    ���ް�����ô��
      *         05  SKT1410-K-YKZFTN      PIC  X(07).
      *    ɸ����ô��
      *         05  SKT1410-K-ITBFTN      PIC  X(07).
      *    ��ץơ��֥�
           03  SKT1410-SUM-TBL  OCCURS  2.
      *    ��ס����
               05  SKT1410-GKENSU        PIC  X(06).
      *    ��ס����
               05  SKT1410-GKINGAKU      PIC  X(07).
      *    ����¾�ơ��֥�
           03  SKT1410-ETC-TBL  OCCURS  1.
      *    ˡ��̾
               05  SKT1410-ETC-HBT       PIC  X(06).
      *    ������
               05  SKT1410-ETC-CODE      PIC  X(02).
      *    ���
               05  SKT1410-ETC-KENSU     PIC  X(03).
