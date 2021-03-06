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
      *    ����������������ɽ �ʴ�긩��     COPY (SKT0305.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/22    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         10/09/22  �����ΥХ��ȿ�����
      *  01.00.02    ����         11/04/11  ����������ʿ�β��Ͽ��б�
      *  01.00.03    ����         12/04/19  Ģɼ�ͼ����ѹ�
      *  01.00.04    ���         18/10/10  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT0305.
      *    ����ǯ��
           03  SKT0305-SRYYM               PIC X(16).
      *    ���ŵ��ؽ���
           03  SKT0305-ADRS                PIC X(100).
      *    ���ŵ���TEL
           03  SKT0305-TEL                 PIC X(15).
      *    ���ŵ���̾��
           03  SKT0305-HOSPNAME            PIC X(100).
      *    ���߼�̾
           03  SKT0305-KAISETUNAME         PIC X(40).
      *    ���ŵ��إ�����
           03  SKT0305-HOSPCD              PIC X(40).
      *    �ڡ����ֹ�
           03  SKT0305-NO                  PIC X(04).
      *    ����ơ��֥�
           03  SKT0305-SEIKYU-TBL   OCCURS 2.
      *    �����ơ��֥�
               05  SKT0305-NYUGAI-TBL   OCCURS 2.
      *    ���̥ơ��֥�
                   07  SKT0305-PREF-TBL  OCCURS  2 TIMES.
      *    ���
                       09  SKT0305-KENSU   PIC X(05).
      *    ����
                       09  SKT0305-NISSU   PIC X(06).
      *    ����
                       09  SKT0305-TENSU   PIC X(09).
      *    ����ơ��֥�
           03  SKT0305-ZAI-TBL      OCCURS  2 TIMES.
      *    ���̥ơ��֥�
               05  SKT0305-ZPREF-TBL    OCCURS  2 TIMES.
      *    ���
                   07  SKT0305-ZKENSU      PIC X(5).
      *    ����
                   07  SKT0305-ZNISSU      PIC X(6).
      *    ����
                   07  SKT0305-ZTENSU      PIC X(9).
      *    ��ץơ��֥�
           03  SKT0305-GOKEI-TBL    OCCURS  2 TIMES.
      *    ���̥ơ��֥�
               05  SKT0305-GPREF-TBL    OCCURS  2 TIMES.
      *    ���
                   07  SKT0305-GKENSU      PIC X(5).
      *    ����
                   07  SKT0305-GNISSU      PIC X(7).
      *    ����
                   07  SKT0305-GTENSU      PIC X(9).
      *    ������ơ��֥�ʸ����
           03  SKT0305-NHKNJA-TBL   OCCURS  36 TIMES.
      *    �ݸ���
               05  SKT0305-NHKNJA          PIC X(2).
      *    �����踩̾�ơ��֥�ʸ�����
           03  SKT0305-KENMEI-TBL   OCCURS  19 TIMES.
      *    ��̾
               05  SKT0305-KENMEI          PIC X(6).
      *    ���������ơ��֥�ʻ��긩��
           03  SKT0305-GHKNJA-TBL   OCCURS  9 TIMES.
      *    ���
               05  SKT0305-GHKNJA          PIC X(2).
      *    ���������ơ��֥�
           03  SKT0305-GHKNJA-TBL2   OCCURS  19 TIMES.
      *    ���
               05  SKT0305-GHKNJA2         PIC X(2).
      *    ����������ץơ��֥�
           03  SKT0305-GOKEIHKNJA-TBL    OCCURS  2 TIMES.
      *    ���
               05  SKT0305-GOKEIHKNJA      PIC X(3).
