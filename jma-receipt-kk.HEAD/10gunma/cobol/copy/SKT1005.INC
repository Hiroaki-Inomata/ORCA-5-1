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
      *  �������԰�������ɽ�ʷ��ϸ���     COPY (SKT1005.INC)               
      *
      *  ��������    ��ȼ�        ����
      *  08/04/17    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT1005.
      *    ����ǯ��
              03  SKT1005-SRYYM            PIC  X(16).
      *    ���ŵ��إ�����
              03  SKT1005-HOSPCD           PIC  X(07).
      *    ����
              03  SKT1005-ADRS             PIC  X(100).
      *    ���ŵ���̾
              03  SKT1005-HOSPNAME         PIC  X(100).
      *    �����ֹ�
              03  SKT1005-TEL              PIC  X(15).
      *    ���߼�̾
              03  SKT1005-KAISETUNAME      PIC  X(40).
      *    ���߼�̾
              03  SKT1005-HYOBETU          PIC  X(02).
      *    ����ơ��֥�
              03  SKT1005-SEIKYU-TBL    OCCURS  20.
      *    �߰���
                  05  SKT1005-ZAI          PIC  X(02).
      *    �ݸ���̾
                  05  SKT1005-HKNJANAME    PIC  X(20).
      *    ���
                  05  SKT1005-KYURATE      PIC  X(02).
      *    �����ơ��֥�
                  05  SKT1005-NYUGAI OCCURS  2.
      *    ���
                      07  SKT1005-KENSU        PIC  X(05).
      *    ����
                      07  SKT1005-TENSU        PIC  X(09).
      *    ���
              03  SKT1005-SUM-AREA  OCCURS  2.
                  05  SKT1005-G-NYUGAI  OCCURS  2.
      *    ��ס����
                      07  SKT1005-GKENSU       PIC  X(05).
      *    ��ס�����
                      07  SKT1005-GTENSU       PIC  X(10).