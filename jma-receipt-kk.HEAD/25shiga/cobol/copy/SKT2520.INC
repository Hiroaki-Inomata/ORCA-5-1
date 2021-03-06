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
      * Maj/Min/Rev  ������   ����         ����
      *  01.00.01    ����     2009/06/10   �ݸ���̾�Х��ȿ�����
      *  01.00.02    ����     2010/11/01   ��������������б�
      *  01.00.03    ���     2018/12/12   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2520.
      *    NO
           03  SKT2520-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT2520-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT2520-SEIYMD              PIC  X(22).
      *    ����
           03  SKT2520-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT2520-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT2520-KAISETUNAME         PIC  X(80).
      *    �����ֹ�
           03  SKT2520-TEL                 PIC  X(15).
      *    �ݸ���̾
           03  SKT2520-HKNJANAME           PIC  X(40).
      *    �ݸ����ֹ�
           03  SKT2520-HKNJANUM            PIC  X(06).
      *    ���ŵ��إ�����
           03  SKT2520-HOSPCD              PIC  X(07).
      *    �����ǡ��߰���
           03  SKT2520-ZAISOU1             PIC  X(02).
      *    ����������
           03  SKT2520-TOKUBETU            PIC  X(10).
      *    ���ճ��
           03  SKT2520-KYURATE-TBL  OCCURS  4.
               05  SKT2520-KYURATE         PIC  X(02).
      *    ����ơ��֥�
           03  SKT2520-SEIKYU-TBL  OCCURS  11.
      *    ���
               05  SKT2520-KENSU       PIC  X(05).
      *    ���ż�����
               05  SKT2520-NISSU       PIC  X(05).
      *    ����
               05  SKT2520-TENSU       PIC  X(09).
      *    ������ô��
               05  SKT2520-ITBFTN      PIC  X(09).
      *    ʡ���������ơ��֥�
           03  SKT2520-FSEIKYU-TBL.
      *    ���
               05  SKT2520-FKENSU       PIC  X(05).
      *    ���ż�����
               05  SKT2520-FNISSU       PIC  X(05).
      *    ����
               05  SKT2520-FTENSU       PIC  X(09).
      *    ������ô��
               05  SKT2520-FITBFTN      PIC  X(09).
      *    �����������ơ��֥�
           03  SKT2520-KSEIKYU-TBL OCCURS 2.
      *    ����̾
               05  SKT2520-KNAME        PIC  X(12).
      *    ���
               05  SKT2520-KKENSU       PIC  X(05).
      *    ���ż�����
               05  SKT2520-KNISSU       PIC  X(05).
      *    ����
               05  SKT2520-KTENSU       PIC  X(09).
      *    ������ô��
               05  SKT2520-KITBFTN      PIC  X(09).
      *    Ĺ����ۼ��·��
           03  SKT2520-TYOKI-KENSU      PIC  X(05).
      *
      *    ����Ϸ��ʻ�ѷ��
           03  SKT2520-GENBK-KENSU      PIC  X(05).
