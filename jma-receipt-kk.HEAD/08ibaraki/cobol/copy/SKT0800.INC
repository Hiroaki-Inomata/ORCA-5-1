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
      * Maj/Min/Rev     ������   ����         ����
      *  01.00.01       Ĺ��   08/04/26    Ģɼ�ͼ���ȼ������
      *  01.00.02       ����   09/02/06    ���߼�̾�Х��ȿ�����
      *  01.00.03       ����   11/01/11    ��������������б�
      *  01.00.04       ���   18/10/09    ���ץ����Ȥ���
      *****************************************************************
       01  SKT0800.
      *    ����ǯ��
           03    SKT0800-SRYYM          PIC  X(16).
      *    ���ŵ��إ�����
           03    SKT0800-HOSPCDN        PIC  X(20).
      *    ���ŵ��ؽ���
           03    SKT0800-ADRS           PIC  X(100).
      *    ���ŵ���̾��
           03    SKT0800-HOSPNAME       PIC  X(100).
      *    ���߼�̾
           03    SKT0800-KAISETUNAME    PIC  X(60).
      *    ����������
           03    SKT0800-TOKUBETU       PIC  X(10).
           03       SKT0800-SEIKYU-TBL OCCURS 4.
                 05   SKT0800-NYUGAI-TBL OCCURS 2.
      *    ���
                      07  SKT0800-KENSU      PIC  X(07).
      *    ����
                      07  SKT0800-TENSU      PIC  X(14).
      *    �������
                 05   SKT0800-S-KENSU        PIC  X(07).
      *    �������
                 05   SKT0800-S-KINGK        PIC  X(11).
           03    SKT0800-KOH-TBL OCCURS 5.
      *    ������
                 05    SKT0800-KKENSU        PIC  X(07).
      *    ��������
                 05    SKT0800-KTENSU        PIC  X(14).
