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
      *  01.00.01    ����    11/03/04    ��������������б�
      *  01.00.02    ����    12/01/16    ������ΥХ��ȿ�����
      *  01.00.02    ���    19/02/04    �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT4600.
      *����ǯ��
           03  SKT4600-SRYYM               PIC  X(16).
      *���⡦����
           03  SKT4600-KENNNAI-GAI         PIC  X(02) OCCURS  2.
      *ɸ�ֲ�
           03  SKT4600-SRYKANAME           PIC  X(30) OCCURS 4.
      *���ŵ��إ�����
           03  SKT4600-HOSPCD              PIC  X(07).
      *͹���ֹ�
           03  SKT4600-POST                PIC  X(18).
      *���ŵ��ؽ���
           03  SKT4600-HOSPADRS            PIC  X(100).
      *���ŵ���̾��
           03  SKT4600-HOSPNAME            PIC  X(100).
      *���߼�̾
           03  SKT4600-KAISETU             PIC  X(40).
      *�����ֹ�
           03  SKT4600-TEL                 PIC  X(15).
      *�����ݸ��Կ�������ס�
           03  SKT4600-HKNSUUG             PIC  X(05).
      *��������������ס�
           03  SKT4600-SKYSUUG             PIC  X(05).
      *���ٽ����������ס�
           03  SKT4600-MEISAISUUG          PIC  X(05).
      *�����ݸ��Կ�
           03  SKT4600-HKNSUU              PIC  X(05).
      *��������
           03  SKT4600-SKYSUU              PIC  X(05).
      *���ٽ����
           03  SKT4600-MEISAISUU           PIC  X(05).
      *�����ݸ��Կ��ʸ�������
           03  SKT4600-HKNSUUK             PIC  X(05).
      *���������ʸ�������
           03  SKT4600-SKYSUUK             PIC  X(05).
      *���ٽ�����ʸ�������
           03  SKT4600-MEISAISUUK          PIC  X(05).
      *����������
           03  SKT4600-TOKUBETU            PIC  X(10).
      *���ܤε��եơ��֥�
           03  SKT4600-SEIKYU  OCCURS  13.
               05  SKT4600-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT4600-KENSU        PIC  X(05).
      *����
                   07 SKT4600-TENSU        PIC  X(10).
      *���ް�����ô��
      *             07 SKT4600-YKZFTN       PIC  X(07).
      *������ô��
                   07 SKT4600-ITBFTN       PIC  X(09).
      *����������ơ��֥�
           03  SKT4600-SEIKYU-S  OCCURS  11.
      *�������
               05  SKT4600-SKENSU          PIC  X(05).
      *����������
               05  SKT4600-SKINGK          PIC  X(09).
      *ɸ����ô��
               05  SKT4600-SFTNGK          PIC  X(09).
