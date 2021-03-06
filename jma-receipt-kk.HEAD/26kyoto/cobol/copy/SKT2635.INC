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
      *    �����������ɽ�ʵ����ܡ�     COPY (SKT2635.INC)               
      *
      *  ��������    ��ȼ�        ����
      *  08/04/11    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������     ����        ����
      *  01.00.01    ����       2008/04/29  ����������ϻ���ξ��˰���
      *  01.00.02    ���       2018/12/04  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2635.
      *    ����ǯ��
           03  SKT2635-SRYYM              PIC  X(16).
      *    �ݸ���̾
           03  SKT2635-HKNJANAME          PIC  X(100).
      *    ͹���ֹ�
           03  SKT2635-POST               PIC  X(8).
      *    ����
           03  SKT2635-ADRS               PIC  X(100).
      *    ���ŵ���̾
           03  SKT2635-HOSPNAME           PIC  X(100).
      *    ���߼�̾
           03  SKT2635-KAISETUNAME        PIC  X(40).
      *    �����ֹ�
           03  SKT2635-TEL                PIC  X(15).
      *    ���ŵ��إ�����
           03  SKT2635-HOSPCD             PIC  X(14).
      *    ������������
           03  SKT2635-T-KENSU            PIC  X(06).
      *    ��������������
           03  SKT2635-T-TENSU            PIC  X(09).
      *    ����ơ��֥�
           03  SKT2635-SEIKYU-TBL    OCCURS  3.
      *    �����ơ��֥�
               05  SKT2635-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2635-KENSU      PIC  X(06).
      *    ����
                   07  SKT2635-TENSU      PIC  X(09).
      *    ������ô��
                   07  SKT2635-ITBFTN     PIC  X(09).
      *    ¾
                   07  SKT2635-TAKOH      PIC  X(02).
      *    Ĺ��
                   07  SKT2635-TYOKI      PIC  X(02).
      *    ����
                   07  SKT2635-GENBAKU    PIC  X(02).
      *    ������
                   07  SKT2635-NISSU      PIC  X(03).
      *    �������ܡ����
                   07  SKT2635-S-KENSU    PIC  X(06).
      *    �������ܡ����
                   07  SKT2635-S-KINGK    PIC  X(09).
      *    �������ܡ�ɸ����ô��
                   07  SKT2635-S-FTNGK    PIC  X(09).
