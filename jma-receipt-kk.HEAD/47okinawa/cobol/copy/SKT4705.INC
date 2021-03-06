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
      *    ������������ɽ �ʲ��츩��     COPY (SKT4705.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/05/03    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT4705.
      *    ����ǯ��
           03  SKT4705-SRYYM                    PIC X(16).
      *    ����ǯ����
           03  SKT4705-SEIYMD                   PIC X(22).
      *    ���ŵ��إ�����
           03  SKT4705-HOSPCD                   PIC X(14).
      *    ���ŵ��ؽ���
           03  SKT4705-ADRS                     PIC X(100).
      *    ���ŵ���̾��
           03  SKT4705-HOSPNAME                 PIC X(100).
      *    ���߼�̾
           03  SKT4705-KAISETUNAME              PIC X(40).
      *    �����ֹ�
           03  SKT4705-TEL                      PIC X(15).
      *    �쥻��
           03  SKT4705-RECEKA                   PIC X(20).
      *    ����ơ��֥�
           03  SKT4705-SEIKYU-TBL      OCCURS  2.
      *    ���ճ��ơ��֥�
               05  SKT4705-KYURATE-TBL     OCCURS  2.
      *    ���
                   07  SKT4705-KENSU            PIC X(05).
      *    ���ץơ��֥�
           03  SKT4705-SYOKEI-TBL      OCCURS  2.
      *    ���׷��
               05  SKT4705-SKENSU               PIC X(06).
      *    ��׷��
           03  SKT4705-GKENSU                   PIC X(06).
      *    ����ơ��֥�
           03  SKT4705-KOH-TBL         OCCURS  7.
      *    ˡ���ֹ�
               05  SKT4705-HBTNUM               PIC X(02).
      *    ����̾
               05  SKT4705-KOHNAME              PIC X(10).
      *    ���������ơ��֥�
               05  SKT4705-KNYUGAI-TBL     OCCURS  2.
      *    ������
                   07  SKT4705-KKENSU           PIC X(05).
      *    �����׷��
           03  SKT4705-GKKENSU                  PIC X(06).
