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
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������    ����        ����
      *  01.00.01    Ĺ��      08/04/29    Ģɼ�ͼ���ȼ������
      *  01.00.02    ����      10/11/26    open-cobol1.0�б�
      *  01.00.03    ����      11/01/12    ��������������б�
      *  01.00.04    ����      11/08/19    ���߼�̾�ΥХ��ȿ�����
      *  01.00.05    ���      18/09/05    ���ץ����Ȥ���
      *****************************************************************
       01  SKT3830.
      *NO
           03  SKT3830-NO                  PIC  X(04).
      *����ǯ��
           03  SKT3830-SRYYM               PIC  X(16).
      *����ǯ����
           03  SKT3830-SEIYMD              PIC  X(22).
      *���ŵ��إ�����
           03  SKT3830-HOSPCD              PIC  X(14).
      *���ŵ��ؽ���
           03  SKT3830-HOSPADRS            PIC  X(100).
      *���ŵ���̾��
           03  SKT3830-HOSPNAME            PIC  X(100).
      *���߼�̾
           03  SKT3830-KAISETU             PIC  X(60).
      *�ݸ���̾��
           03  SKT3830-HKNJANAME           PIC  X(24).
      *�ݸ����ֹ�
           03  SKT3830-HKNJANUM            PIC  X(12).
      *�����ǡ��߰���
           03  SKT3830-ZAI                 PIC  X(02) OCCURS 2.
      *����������
           03  SKT3830-TOKUBETU            PIC  X(10).
      *�������ܤε��եơ��֥�
           03  SKT3830-K-SEIKYU.
               05  SKT3830-K-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT3830-KKENSU        PIC  X(06).
      *����
                   07 SKT3830-KNISSU        PIC  X(06).
      *����
                   07 SKT3830-KTENSU        PIC  X(10).
      *������ô��
                   07 SKT3830-KITBFTN       PIC  X(09).
      *�������
           03  SKT3830-S-KKENSU         PIC  X(06).
      *��������
           03  SKT3830-S-KNISSU         PIC  X(06).
      *����������
           03  SKT3830-S-KMONEY         PIC  X(09).
      *ɸ����ô��
           03  SKT3830-S-KFTNGAKU       PIC  X(09).
