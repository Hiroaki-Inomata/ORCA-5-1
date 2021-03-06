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
      *  01.00.01    ����    2011/06/15  ��������������б�
      *  01.00.02    ����    2012/04/19  Ģɼ�ͼ����ѹ�
      *  01.00.03    ���    2018/10/10  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT0310.
      *    �ڡ����ֹ�
           03  SKT0310-NO                   PIC  X(04).
      *    ����ǯ��
           03  SKT0310-SRYYM                PIC  X(16).
      *    �ݸ���̾
           03  SKT0310-HKNJANAME            PIC  X(26).
      *    �ݸ����ֹ�
           03  SKT0310-HKNJANUM             PIC  X(12).
      *    ����ǯ����
           03  SKT0310-SEIYMD               PIC  X(22).
      *    ���ŵ��إ�����
           03  SKT0310-HOSPCDN              PIC  X(40).
      *    ���ŵ��ؽ���
           03  SKT0310-ADRS                 PIC  X(100).
      *    ���ŵ���̾
           03  SKT0310-HOSPNAME             PIC  X(100).
      *    �����ֹ�
           03  SKT0310-TEL                  PIC  X(15).
      *    ���߼�̾
           03  SKT0310-KAISETUNAME          PIC  X(40).
      *    ���ʰ����ݸ��ԡ�
           03  SKT0310-KYURATE              PIC  X(02).
      *
           03  SKT0310-KUMIAI1              PIC  X(02).
           03  SKT0310-KUMIAI2              PIC  X(04).
           03  SKT0310-KUMIAI3              PIC  X(02).
      *    ����������
           03  SKT0310-TOKUBETU             PIC  X(10).
      *    ����ơ��֥�
           03  SKT0310-SEIKYU-TBL     OCCURS  8.
      *    �����ơ��֥�
               05  SKT0310-NYUGAI-TBL     OCCURS  2.
      *    ���
                   07  SKT0310-KENSU        PIC  X(05).
      *    ������
                   07  SKT0310-NISSU        PIC  X(05).
      *    ����
                   07  SKT0310-TENSU        PIC  X(09).
      *    ������ô��
                   07  SKT0310-ITBFTN       PIC  X(09).
      *    Ĺ��
                   07  SKT0310-TYOKI        PIC  X(02).
      *    �������ܡ����
               05  SKT0310-S-KENSU          PIC  X(05).
      *    �������ܡ�����
               05  SKT0310-S-NISSU          PIC  X(05).
      *    �������ܡ����
               05  SKT0310-S-KINGAKU        PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0310-S-FTNGAKU        PIC  X(09).
      *    �����ǥơ��֥�
           03  SKT0310-ZAI-TBL     OCCURS  4.
      *    ���
               05  SKT0310-ZKENSU           PIC  X(05).
      *    ������
               05  SKT0310-ZNISSU           PIC  X(05).
      *    ����
               05  SKT0310-ZTENSU           PIC  X(09).
      *    ������ô��
               05  SKT0310-ZITBFTN          PIC  X(09).
      *    Ĺ��
               05  SKT0310-ZTYOKI           PIC  X(02).
