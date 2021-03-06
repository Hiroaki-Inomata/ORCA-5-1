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
      ********************************************************************
      *  ������������ɽ�ʷ��ܸ���     COPY (SKT4305.INC)
      *
      *  ��������    ��ȼ�        ����
      *  08/04/18    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         12/03/14  �����ȿ���������η������
      *  01.00.02    ���         18/09/05  ���ץ����Ȥ���
      ********************************************************************
       01  SKT4305.
      *    ����ǯ��
           03  SKT4305-SRYYM               PIC  X(16).
      *    ͹���ֹ�
           03  SKT4305-POST.
               05  SKT4305-POST-OCC        PIC  X(02) OCCURS 7.
      *    ɸ�ֲ�̾
           03  SKT4305-SRYKANAME           PIC  X(16) OCCURS 2.
      *    NO
           03  SKT4305-NO                  PIC  X(04).
      *    ���ŵ��ؽ����
           03  SKT4305-ADRS                PIC  X(100).
      *    ���ŵ���̾��
           03  SKT4305-HOSPNAME            PIC  X(100).
      *    �����ֹ�
           03  SKT4305-TEL                 PIC  X(15).
      *    ���߼Ի�̾
           03  SKT4305-KAISETUNAME         PIC  X(40).
      *    ���ŵ��إ�����
           03  SKT4305-HOSPCD              PIC  X(40).
      *    �������
           03  SKT4305-SEIKYU-TBL  OCCURS 8.
      *    ��Į¼̾
               05  SKT4305-HKNJANAME       PIC  X(20).
      *    �ݸ��ơ��֥�
               05  SKT4305-NYUGAI-TBL   OCCURS  2.
      *    �����ơ��֥�
                   07  SKT4305-KYURATE-TBL  OCCURS 2.
      *    ���
                       09  SKT4305-KENSU   PIC  X(04).
      *    ����
                       09  SKT4305-TENSU   PIC  X(08).
      *    �������
               05  SKT4305-S-KENSU         PIC  X(04).
      *    ����������
               05  SKT4305-S-KINGK         PIC  X(08).
      *    ɸ����ô��
               05  SKT4305-S-FTNGK         PIC  X(07).
      *    ��ץ��ꥢ
           03  SKT4305-GOKEI-TBL.
               05  SKT4305-GNYUGAI-TBL   OCCURS  2.
      *    �����ơ��֥�
                   07  SKT4305-GKYURATE-TBL OCCURS 2.
      *    ���
                       09  SKT4305-GKENSU  PIC  X(04).
      *    ����
                       09  SKT4305-GTENSU  PIC  X(08).
      *    �������
               05  SKT4305-S-GKENSU        PIC  X(04).
      *    ����������
               05  SKT4305-S-GKINGK        PIC  X(08).
      *    ɸ����ô��
               05  SKT4305-S-GFTNGK        PIC  X(07).
      *    ���񥨥ꥢ
           03  SKT4305-KOUHI-TBL  OCCURS 6.
      *    ����ˡ���ֹ�
               05  SKT4305-HBTNUM          PIC X(02).
      *
               05  SKT4305-KNYUGAI-TBL  OCCURS 2.
      *    ���񡧷��
                   07  SKT4305-KKENSU      PIC X(04).
      *    ��������
                   07  SKT4305-KTENSU      PIC X(08).
      *    ���񡧿������
               05  SKT4305-S-KKENSU        PIC X(04).
      *    ���񡧿���������
               05  SKT4305-S-KKINGK        PIC X(08).
      *    ����ɸ����ô��
               05  SKT4305-S-KFTNGK        PIC X(07).
      *    �����
           03  SKT4305-SOUKENSU            PIC X(07).
